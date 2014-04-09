### R code from vignette source 'fuzzyMM.Rnw'

###################################################
### code chunk number 1: a
###################################################
library(fuzzyMM)
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

  # thanks to Kristina Helle!
  # dynamic parsing of phenomenon names and units
  phenomenonsUrl = "https://www.envirocar.org/api/stable/phenomenons"
  phenomenons = fromJSON(getURL(phenomenonsUrl,ssl.verifypeer = FALSE))

  
  colNames <- c("GPS.Bearing", "GPS.HDOP", "GPS.Speed")
  if (!all(colNames %in% names(l2[[1]]))) 
    stop("Track does not contain all the necessary data (GPS.Bearing, GPS.HDOP, GPS.Speed)")
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

url = "https://envirocar.org/api/stable/tracks/52b45583e4b0f9afbd29bb6b"
track = importEnviroCar(url)
roads <- create_drn(bbox(track))


###################################################
### code chunk number 2: fuzzyMM.Rnw:104-106
###################################################
names(track)
proj4string(track)


###################################################
### code chunk number 3: fuzzyMM.Rnw:113-115
###################################################
plot(track$coords.x1[1:6], track$coords.x2[1:6], pch = 16, col="blue")
lines(roads@sl)


###################################################
### code chunk number 4: b
###################################################
matched_track <- mm(track)


###################################################
### code chunk number 5: fuzzyMM.Rnw:129-132
###################################################
plot(track$coords.x1[1:6], track$coords.x2[1:6], pch = 16, col="blue")
lines(roads@sl)
points(matched_track$coords.x1, matched_track$coords.x2,pch=16, col = "red")


###################################################
### code chunk number 6: fuzzyMM.Rnw:147-148
###################################################
get_var_bounds()


###################################################
### code chunk number 7: fuzzyMM.Rnw:154-155
###################################################
set_var_bounds("speed_high", c(4, 7))


###################################################
### code chunk number 8: fuzzyMM.Rnw:159-160
###################################################
update_mf()


###################################################
### code chunk number 9: d
###################################################
fis_imp <- get_fis("IMP")
str(fis_imp)
fis_imp$varinp.mf


###################################################
### code chunk number 10: fuzzyMM.Rnw:170-171
###################################################
plotMF(fis_imp)


###################################################
### code chunk number 11: fuzzyMM.Rnw:175-176
###################################################
plotMF(fis_imp)


