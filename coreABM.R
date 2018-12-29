#This is the model of the processes that are in Books 1-3 of the 
#Traveller game that came out in the late seventies. This will
#use agent based modelling to explore the survival of captains 
#and their ships from bankruptcy. Can free traders survive? If
#not, what constraints need to be relaxed for them to do so?
#this project is to learn how to design and implement an ABM 
#and to explore the solution space for free trader survival
#https://github.com/Justin-In-Oz/TravellerABM.git

## library calls and why
library(httr) # make API calls to Traveller Map
library(jsonlite) # parse the returns of the API calls
library(magrittr) # pipe stuff
library(adagio) # knapsack cargo selection and hence destination selection

## Function Set Up
destList <- function(portLocation, shipRange) {
  # match planet and return the list of destinations within 
  # jump range
  # for the time being this will have to be using the Traveller Map API
  # https://travellermap.com/api/coordinates?sx=sx&sy=sy
  # https://travellermap.com/api/jumpworlds?sx=sx&sy=sy&hx=hx&hy=hy
  # these will have to return objects using the httr package
  # create the query to send
  # start with the location
  jumpQuery <- portLocation
  #add the jump range
  jumpQuery[["jump"]] <-shipRange
  
  rawReturn <- GET(url = "https://travellermap.com/", 
                          path = "api/jumpworlds", 
                          query = jumpQuery)
  destinationsList <- rawReturn$content %>%
    rawToChar() %>%
    fromJSON() %>%
    as.data.frame()
  # create the value for the current hex
  currentHex = paste(portLocation$hx, portLocation$hy, sep = "")
  # remove the destination that is the current hex using the 
  # funky [row, column] reference notation from r
  destinationsList <- destinationsList[!(destinationsList$Worlds.Hex == currentHex), ]
  return (destinationsList) 
} # end of destList function

cargoList <- function (cargoSource, shipRange) {
  # lookup the UPPs for the two points and generate the 
  # lots and tonnage going between the two ports
  # Determine the ports of Call
  portsOfCall <- destList(portLocation = cargoSource, shipRange = jumpRange)
  # extract the pop numbers from the UPP string
  # Population is the fifth with the starport being the first
  # assign the exteracted numbers to a new vector in the df
  portsOfCall$Worlds.PopNum <- vapply(X = portsOfCall$Worlds.UWP, 
                                      FUN = substr, 
                                      start = 5, 
                                      stop = 5,
                                      FUN.VALUE = character(1))
  #convert the characters into integers, this might not work for hex pop numbers over 9
  portsOfCall$Worlds.PopNum <- vapply(X = portsOfCall$Worlds.PopNum,
                                      FUN = as.integer,
                                      FUN.VALUE = integer(1))

  # create an empty numeric vector of that length
  pointToPointCargos <- vector(mode = "list", length = nrow(portsOfCall))
  
  # originally I had done the below code with a for loop. I asked the question on stack overflow
  # https://stackoverflow.com/questions/53475367/eliminate-for-loop-through-functional-programming
  # I was happy with the answers. 
  # lapply a 1d6*5 random number generator across the vector
  pointToPointCargos <- lapply(X = portsOfCall$Worlds.PopNum, 
                               FUN = function(popN) {sample.int(
                               n = 6, replace = TRUE,
                               size = popN)* 5})
  
  # name the cargo destinations in the list
  names(pointToPointCargos) <- portsOfCall$Worlds.Name

  return (pointToPointCargos)
} # end of the cargo list function

# roll numbers of d6 based upon the populatin of the source 
# and modify the result based upon the pop of the destination
# The values of the number of dice and the modifiers are 
# taken from Traveller LBB vol 2 page 7 pub by GDW 1977 
availPassengers <- function (startPop, endPop) {
  # declare the vectors for the number of dice
  diceHighPlus     <- c(0,1,3,3,3,3,3,2,2,2,2,2)
  diceHighMinus    <- c(0,1,2,3,2,2,2,1,1,1,0,0)
  diceMiddlePlus   <- c(0,1,2,3,3,3,3,3,2,2,2,2)
  diceMiddleMinus  <- c(0,1,2,3,2,2,2,2,1,1,1,0)
  diceLowPlus      <- c(0,3,3,4,4,3,3,4,4,4,5,6)
  diceLowMinus     <- c(0,1,1,1,1,0,0,0,0,0,0,0)
 # declare the voectors for the modifiers for the passengers 
 # based upon the destriantion pop number
  modifierHigh    <- c(0,-1,-1,-1,0,0,0,1,1,1,0,0)
  modifierMiddle  <- c(0,-2,-1,-1,-1,0,0,0,1,1,1,0)
  modifierLow     <- c(0,-4,-3,-2,-1,-1,0,0,0,2,4,0)
  
  # create the empty named list
  availPassengers <- list("High"= 0, "Middle" = 0, "Low" = 0)
# generate the number of high passage demand to the destination
# one set of rolls minus the other but with a zero as a floor 
# by calling the max between the sum and zero. This stops a -ve result
  highPassengers <- max(0,  sum(sample(x = 1:6, 
                                       size = diceHighPlus[startPop], 
                                        replace = TRUE))
                           - sum(sample(x = 1:6, 
                                        size = diceHighMinus[startPop], 
                                        replace = TRUE))
                           + modifierHigh[endPop]
                        )

  # generate the middle passangers
  middlePassengers <- max(0,  sum(sample(x = 1:6, 
                                         size = diceMiddlePlus[startPop], 
                                       replace = TRUE))
                              - sum(sample(x = 1:6, 
                                     size = diceMiddleMinus[startPop], 
                                     replace = TRUE))
                              + modifierMiddle[endPop]
                          )    

#Generate the number of low passangers
lowPassengers <- max(0,  sum(sample(x = 1:6, 
                                    size = diceLowPlus[startPop], 
                                    replace = TRUE))
                        - sum(sample(x = 1:6, 
                                    size = diceLowMinus[startPop], 
                                    replace = TRUE))
                        + modifierLow[endPop]
                      )
availPassengers <- list("High"   = highPassengers, 
                        "Middle" = middlePassengers, 
                        "Low"    = lowPassengers)
} # end available passangers function

## Initialise
# locations are expressed as (sx, sy) and (hx, hy)
# captains have a ship, a location and a bank balance

## Random Activiation

# Jump in System
  # unload passengers 
highPassengersOnBoard <- 0
middlePassengersOnBoard <- 0
  # unload cargo
cargosInTheHold <- 0
  # revive lowberths
lowPassengersOnBoard <- 0

# ** Find available cargoes for systems within range, this is a function call
# that returns a list of destinations and cargoes available

# test data
#pass location as Regina
currentLocation <- list(sx=-4, sy=-1, hx=19, hy=10)
#set the jump range to be that of a freetrader
jumpRange <- 1

# call the destList function to find out what is available
availableCargos <- cargoList(cargoSource = currentLocation, 
                             shipRange = jumpRange)

# cargo selection is a load packing problem.
# this can be done via linear program, various heuristics
# or random brute force. 
# The load packing problem is called the knapsack problem
# lapply the knapsack function to the cargo list and look
# for the one which maxes the return

#test Data
shipCargoCapacity <- 82 # this is the cargo cap of a class A Freetrader

# create the list of the results of the knpasack problem for the available cargos
cargoRevenues <- lapply(X = names(availableCargos), 
                        FUN = function(nm) {
                        knapsack(w = availableCargos[[nm]],
                                 p = 1000 * availableCargos[[nm]],
                                 cap = shipCargoCapacity)
                          })
# choose a destination based upon max revenues
# pick one at random if there are several equal to the max

# extract the revenues available
revenuesAvailable <- lapply(seq_along(cargoRevenues), 
                            function(i) cargoRevenues[[i]][["profit"]])

# pull out the max revenue
maxRevenue <- max(unlist(revenuesAvailable))

# Choose at random one of the destinations that satisfies the max revenue criteria
# create the list of those which satisfy the max criteria
destinationChoices <- which(revenuesAvailable == maxRevenue)

# choose one from the list
jumpDestination <- sample(x = names(availableCargos)[destinationChoices],size = 1)

# set to zero the available cargoes that have been choosen
#return the index of the destination 
destinationIndex <- which(names(availableCargos)== jumpDestination)

#return the indices of the knapsack selection
chosenCargoes <- cargoRevenues[[destinationIndex]][["indices"]]

# set the cargoes in the hold to be those chosen
cargosInTheHold <- availableCargos[[jumpDestination]][chosenCargoes]

#set the available cargoes selected to zero
availableCargos[[jumpDestination]][chosenCargoes] <- 0

# after cargo selection, post destination and 
# ** seek passengers

# test data for passenger call
# set to population of the source world
sourcePop <- 8
# set the population of the destination world
destPop <- as.numeric(length(availableCargos[[jumpDestination]]))

portPassengers <- availPassengers(sourcePop,destPop)

# set the test values to be those of a Type A
passengerStaterooms <- 6
lowBerthCap <- 20

# passenger selection is easier than cargo packing. Fill up 
# on the High Passages untill they run out or you are at 
# capacity, then switch to middle and do the same. FIll up 
# on Low until your cryo bins are full or there are no more to take. 

# load up the high passages
if(portPassengers[["High"]] > passengerStaterooms) {
  highPassengersOnBoard = passengerStaterooms
  portPassengers[["High"]] <- portPassengers[["High"]] - passengerStaterooms
} else if (portPassengers[["High"]] <= passengerStaterooms) {
  highPassengersOnBoard = portPassengers[["High"]]
  portPassengers[["High"]] <- 0
}

# load up Middle Passages with remaining capacity

# determine the capacity less the number of High Passengers
remainingStaterooms <- passengerStaterooms - highPassengersOnBoard

# load up the high passages
if(portPassengers[["Middle"]] > remainingStaterooms) {
  middlePassengersOnBoard = remainingStaterooms
  portPassengers[["Middle"]] <- portPassengers[["Middle"]] - remainingStaterooms
} else if (portPassengers[["Middle"]] <= remainingStaterooms) {
  middlePassengersOnBoard = portPassengers[["Middle"]]
  portPassengers[["Middle"]] <- 0
}
# allocate the remaining spaces to Low Passengers
if(portPassengers[["Low"]] > lowBerthCap) {
  lowPassengersOnBoard = lowBerthCap
  portPassengers[["Low"]] <- portPassengers[["Low"]] - lowBerthCap
} else if (portPassengers["Low"] <= lowBerthCap) {
  lowPassengersOnBoard = portPassengers[["Low"]]
  portPassengers[["Low"]] <- 0
}


# ** then jump and advance your clock 1 week cannoncally, but 
#to simplify the model the time step will be advanced by 2 
#weeks at this point. This will abstract all of the time in 
#port to be 1 week additional. Rinse, repeat.

# calculate the accountancy for the ship at this point too. Even 
# though the expenses would occur at different timings, the whole 
# lot can be abstracted to the one moment per trip.
commonCarriageRevenue <- sum(cargosInTheHold) * 1000
highPassageRevenue <- highPassengersOnBoard * 10000
middlePassengeRevenue <- middlePassengersOnBoard * 8000
lowPassageRevenue <- lowPassengersOnBoard * 1000

tripRevenue <- commonCarriageRevenue + 
               highPassageRevenue + 
               middlePassengeRevenue + 
               lowPassageRevenue


# costs come from fuel, life support, maintenance, salaries, 
# mortgage, berthing. They are all put to the per trip unit
fuel <- 15000 # 500 per dTon by 30 dTons
lifeSupport <- 20000 # 10 staterooms
maintenance <- 1426 # cashPrice/1000/26
salaries <- 4500 # assumes pilot owner
berthing <- 100 # chump change
mortgage <- 77250 # cashPrice/480/2
perTripCosts <- fuel + lifeSupport + salaries + berthing + mortgage

tripProfitLoss <- tripRevenue - perTripCosts

# return the location of the jump destination
# call up jump list
systemsInJumpRange <- destList(portLocation = currentLocation, shipRange = jumpRange)

# look up destination in the results
destinationLocationXY <- list(destX = systemsInJumpRange$Worlds.WorldX[[destinationIndex]],
                              destY = systemsInJumpRange$Worlds.WorldY[[destinationIndex]])

# convert this to sx sy hx hy format
destHX <- (destinationLocationXY$destX + 1) %% 32
destHY <- (destinationLocationXY$destY + 40) %% 40
destSX <- floor((destinationLocationXY$destX + 1) / 32)
destSY <- floor((destinationLocationXY$destY + 40) / 40)

# set the current location to be equal to the destination i.e. jump
  
## Post turn Admin
# if a ship has had a -ve bank balance for 5 (?) turns on the 
# trot, the skip jumpers catch up and repo the ship. Game over.
