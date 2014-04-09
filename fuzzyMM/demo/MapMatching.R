## Function to import enviroCar trajectories
####################################################################################
## Code modified from                                                             ##
## Title: Analyzing Envirocar trajectory data with R                              ##
## Author: Edzer Pebesma                                                          ##  
## Date: 2013                                                                     ##
## URL: https://github.com/edzer/rpubs/blob/master/Envirocar.Rmd                  ##
####################################################################################
importEnviroCar = function(file) {
  require(rjson) # fromJSON
  require(maptools) # spCbind
  require(rgdal) #readOGR
  require(RCurl) #getURL
  require(stringr) #str_replace_all
  
  # read data as spatial object:
  layer = readOGR(getURL(file,ssl.verifypeer = FALSE), layer = "OGRGeoJSON")
  
  # convert time from text to POSIXct:
  layer$time = as.POSIXct(layer$time, format="%Y-%m-%dT%H:%M:%SZ")
  # the third column is JSON, we want it in a table (data.frame) form:
  # 1. form a list of lists
  l1 = lapply(as.character(layer[[3]]), fromJSON)
  # 2. parse the $value elements in the sublist:
  l2 = lapply(l1,function(x) as.data.frame(lapply(x, function(X) X$value)))
  # dynamic parsing of phenomenon names and units
  phenomenonsUrl = "https://www.envirocar.org/api/stable/phenomenons"
  phenomenons = fromJSON(getURL(phenomenonsUrl,ssl.verifypeer = FALSE))
  
  colNames <- c("GPS.Bearing", "GPS.HDOP", "GPS.Speed")
  if (!all(colNames %in% names(l2[[1]]))) 
    stop("Trajectory does not contain all the necessary data (GPS.Bearing, GPS.HDOP, GPS.Speed)")
  else
    colNames <- names(l2[[1]])
  
  
  resultMatrix = matrix(nrow = length(l2),ncol = length(colNames))
  dimnames(resultMatrix)[[2]] = colNames
  for (i in seq(along = l2))
    resultMatrix[i,colNames] = as.numeric(l2[[i]])[match(colNames, names(l2[[i]]))]
  result = as.data.frame(resultMatrix)
  
  # set the units:
  units <- sapply(phenomenons[[1]], "[[", "unit")
  names(units)=colNames
  
  # add a units attribute to layer
  layer[[3]] = NULL
  # add the table as attributes to the spatial object
  if (length(layer) == nrow(result)) {
    layer = spCbind(layer, result)
    attr(layer, "units") = units
    layer
  } else
    NULL
}

## URL of the trajectory
url = "https://envirocar.org/api/stable/tracks/52f3836be4b0d8e8c27ed6f0"

## Import the trajectory
traj = importEnviroCar(url)

## Do the map matching
matchedTraj <- mm(traj, plot = TRUE)