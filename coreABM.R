#This is the model of the processes that are in Books 1-3 of the 
#Traveller game that came out in the late seventies. This will
#use agent based modelling to explore the survival of captains 
#and their ships from bankruptcy. Can free traders survive? If
#not, what constraints need to be relaxed for them to do so.
#this project is to learn how to design and implement an ABM 
#and to explore the solution space for free trader survival
#https://github.com/Justin-In-Oz/TravellerABM.git

## library calls
library(httr)
library(jsonlite)
library(magrittr)

## Function Set Up
destList <- function(portLocation, shipRange) {
  #match planet and return the list of destinations within 
  #jump range
  # for the time being this will have to be using the Traveller Map API
  # https://travellermap.com/api/coordinates?sx=sx&sy=sy
  # https://travellermap.com/api/jumpworlds?sx=sx&sy=sy&hx=hx&hy=hy
  # these will have to return objects using the httr package
  # create the query to send
  #start with the location
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
  #convert the characters into integers, this might not work for hex pop numbers over 10
  portsOfCall$Worlds.PopNum <- vapply(X = portsOfCall$Worlds.PopNum,
                                      FUN = as.integer,
                                      FUN.VALUE = integer(1))
  
  # create an empty numeric vector of that length
  pointToPointCargos <- vector(mode = "list", length = nrow(portsOfCall))
  
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
                                         size = diceMiddlePlus[startPop,], 
                                       replace = TRUE))
                              - sum(sample(x = 1:6, 
                                     size = diceMiddleMinus[startPop,], 
                                     replace = TRUE))
                              + modifierMiddle[endPop,]
                          )    
  

#Generate the number of low passangers
lowPassengers <- max(0,  sum(sample(x = 1:6, 
                                    size = diceLowPlus[startPop,], 
                                    replace = TRUE))
                        - sum(sample(x = 1:6, 
                                    size = diceLowMinus[startPop,], 
                                    replace = TRUE))
                        + modifierLow[endPop,]
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

# find available cargoes for systems within range, this is a fubction call
# that returns a list of destinations and cargoes available

# test data
#pass location as Regina
currentLocation <- list(sx=-4, sy=-1, hx=19, hy=10)
#set the jump range to be that of a freetrader
jumpRange <- 2

# call the destList function to find out what is available
availableCrgos <- cargoList(cargoSource = currentLocation, 
                             shipRange = jumpRange)

# cargo selection is a load packing problem.
# this can be done via linear program, various heuritics
# or random brute force.

# after cargo selection, post destination and seek passengers

# test data for passenger call
# set to population of the source world
sourcePop <- 6
# set the population of the destination world
destPop <- 8

testPassengers1 <- availPassengers(sourcePop,destPop)
testPassengers1
## Post turn Admin

