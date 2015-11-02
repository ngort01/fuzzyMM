#' Digital Road Network
#' 
#' Create a Digital Road Network 
#' 
#' @param bbox spatial bounding box from spatial data (\code{\link{bbox}})
#' 
#' @details 
#' This function downloads OSM road data for the area defined 
#' by the bounding box and creates a digital road network.
#' 
#' @author Nikolai Gorte <n.gorte@@gmail.com>
#' 
#' @examples
#' \dontrun{
#' data(traj)
#' roads <- create_drn(bbox(traj))
#' 
#' # Plot everything
#' plot(traj)
#' lines(slot(roads, "sl"))
#' 
#' # Shortest path
#' shortest.paths(slot(roads, "g"), V(slot(roads, "g"))[1], V(slot(roads, "g"))[23])
#' }
#' @export 
create_drn <- function(bbox) {
  x1 <- bbox[[1]] 
  y1 <- bbox[[2]]
  x2 <- bbox[[3]] 
  y2 <- bbox[[4]] 
  
  if (!requireNamespace("RCurl", quietly = TRUE))
	stop("package RCurl required")
  # Using overpass API, because it offers better possibilties for 
  # filtering data than the the OSM API
  url <- paste0("http://www.overpass-api.de/api/xapi?way[bbox=",x1,",",y1,",",x2,",",y2,"][highway=*]")  
  response <- RCurl::getURL(url, .encoding = "UTF-8")
  
  if (!requireNamespace("XML", quietly = TRUE))
	stop("package XML required")
  # Parse Data
  resp <- XML::xmlParse(response)
  
  # Transform parsed data to osmar object
  roads <- as_osmar(resp)
  
  v <- k <- NULL # make visible bindings and R CMD check happy
  
  # Get ID's of all streets used by cars
  id <- find(roads, way(tags(k == "highway" & !(v %in% c("cycleway", 
                                                         "footway", "bridleway", "steps", "path")))))
  roads <- subset(roads, ids = find_down(roads, way(id)))
  
  # Get coordinates of each node
  nodes <- roads$nodes
  coords <- nodes$attrs[c("id", "lat", "lon")]
  
  # Create an igraph from the osmar object
  graph <- as_igraph(roads)
  graph <- as.undirected(graph, mode = "each")
  V(graph)$id <- as.numeric(V(graph)$name)
  # Bring coordinates in the right order
  coords <- coords[match(V(graph)$id, coords$id),]
  V(graph)$lon <- coords$lon
  V(graph)$lat <- coords$lat
  
  # Convert the osmar object to spatial lines and split each line into segments
  roads <- as_sp(roads, "lines")
  roads <- lines2segments(roads)
  #roads <- SpatialLinesDataFrame(roads, as.data.frame(get.edgelist(graph)), match.ID = FALSE)
  roads <- new("DigitalRoadNetwork", sl = roads, g = graph)
  roads
}


setClass("igraph")
setClass("DigitalRoadNetwork", representation(sl = "SpatialLinesDataFrame", g = "igraph"), 
         validity = function(object) {stopifnot(length(object@sl) == length(E(object@g)))})





#############################################################################################################
## Code modified from                                                                                      ##
## Title: split/divide SpatialLines (sp) into n segments                                                   ##
## Author: Roger Bivand                                                                                    ##  
## Date: Apr 10, 2013                                                                                      ##
## URL: http://r-sig-geo.2731867.n2.nabble.com/split-divide-SpatialLines-sp-into-n-segments-td7583234.html ##
#############################################################################################################
lines2segments <- function(sl){
  coords <- coordinates(sl)
  osm_ids <- sl@data$id # osm edge ids
  in_nrows <- lapply(coords, function(x) sapply(x, nrow))
  outn <- sapply(in_nrows, function(y) sum(y-1))
  osm_ids <- rep(osm_ids, outn)
  res <- vector(mode = "list", length = sum(outn))
  i <- 1
  for (j in seq(along = coords)) {
    for (k in seq(along = coords[[j]])) {
      for (l in 1:(nrow(coords[[j]][[k]]) - 1)) {
        res[[i]] <- coords[[j]][[k]][l:(l + 1),]
        i <- i + 1
      }
    }
  }
  res1 <- vector(mode = "list", length = sum(outn))
  for (i in seq(along = res))
    res1[[i]] <- Lines(list(Line(res[[i]])), as.character(i))
  outSL <- SpatialLines(res1, osm_crs())
  outSL <- SpatialLinesDataFrame(outSL, data.frame(osm_ids))
  outSL
}
