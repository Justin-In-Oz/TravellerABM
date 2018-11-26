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
                               size = portsOfCall$Worlds.PopNum)* 5})
  
  # name the cargo destinations in the list
  names(pointToPointCargos) <- portsOfCall$Worlds.Name

  return (pointToPointCargos)
} 

## Initialise
# locations are expressed as (sx, sy) and (hx, hy)
# captains have a ship, a location and a bank balance

## Random Activiation

# Jump in System

# find available cargoes for systems within range
#pass location as Regina
currentLocation <- list(sx=-4, sy=-1, hx=19, hy=10)

#set the jump range to be that of a freetrader
jumpRange <- 1

# call the destList function to find out what is available
availableCrgos <- cargoList(cargoSource = currentLocation, 
                             shipRange = jumpRange)


## Post turn Admin

