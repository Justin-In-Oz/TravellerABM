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
destList <- function(portLocation, jumpRange) {
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
  jumpQuery[["jump"]] <-jumpRange
  
  rawReturn <- GET(url = "https://travellermap.com/", 
                          path = "api/jumpworlds", 
                          query = jumpQuery)
  detstinationsList <- rawReturn$content %>%
    rawToChar() %>%
    fromJSON() %>%
    as.data.frame()
} #return (destinationsList)

cargoList <- function (cargoSource, jumpRange) {
  #Determine the ports of Call
  portsOfCall <- destList(cargoSource, jumpRange)
  
  
  # lookup the UPPs for the two points and generate the 
  # lots and tonnage going between the two ports
  
} return (pointToPointCargos)

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
jumpDestinations <- destList(currentLocation, jumpRange)


## Post turn Admin

