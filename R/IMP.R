## Initial MapMatching Process: identifies the intial road link 
imp <- function (traj, roads = "DigitalRoadNetwork", err_region) { 
  i <- 1
  count <- 0
  found <- FALSE
  initial_links <- data.frame(V1 = numeric(0), V2 = numeric(0), edge_id = numeric(0), 
                                direction = numeric(0), NP_x = numeric(0), NP_y = numeric(0))
  
  while (!found){
    # Get coordinates of the current position and create SpatialPoints
    lon <- traj$coords.x1[i] #lon
    lat <- traj$coords.x2[i] #lat
    current_pt <- cbind(lon, lat)
    rec <- err_region(lon, lat, err_region)
    
    if (!requireNamespace("rgeos", quietly = TRUE))
	  stop("package rgeos required")
    # Get edges inside the error region 
    candidate_links <- data.frame(edge_id = unique(c(which(rgeos::gIntersects(rec, roads@sl, byid = TRUE)), 
                                                     which(rgeos::gContains(rec, roads@sl, byid = TRUE)))))
    
    # Nodes of the candidate links
    candidate_links$V1 <- get.edgelist(roads@g)[candidate_links$edge_id, 1]
    candidate_links$V2 <- get.edgelist(roads@g)[candidate_links$edge_id, 2]
    
    
	if (!requireNamespace("geosphere", quietly = TRUE))
		stop("package geosphere required")
    # Calculate the perpendicular distance from the current point to all 
    # segments inside the error region and the closest point on the segments
    PD <- sapply(candidate_links[,c("edge_id")], 
                 function(x) geosphere::dist2Line(current_pt, 
				 	roads@sl@lines[[x]]@Lines[[1]]@coords))
    
    # Perpendicular distance
    candidate_links$PD <- PD[1,]
    # Nearest point
    candidate_links$NP_x <- PD[2,]
    candidate_links$NP_y <- PD[3,]
    
    # Calculate the beraing of the segments
    # If a segment is defined by the points a and b, bearing can be:
    # bearing(a,b) or bearing(b,a)
    # Which one is chosen depends on the differnce between the bearing and the GPS.Bearing
    gps_bearing <- traj$GPS.Bearing[i]
    candidate_links$direction <- sapply(candidate_links$edge_id, 
                                  function(x) {
                                    bearing <- geosphere::bearing(roads@sl@lines[[x]]@Lines[[1]]@coords[1,],
                                                       roads@sl@lines[[x]]@Lines[[1]]@coords[2,])
                                    if (bearing - gps_bearing <= -90) {
                                      bearing <- bearing + 180
                                      if (bearing > 360) bearing <- bearing - 360
                                      bearing
                                    } else if (bearing - gps_bearing > 90) {
                                      bearing <- bearing - 180
                                      if (bearing < 0) bearing <- bearing + 360
                                      bearing
                                    } else bearing
                                  }) 
    
    # Calculate the heading error
    candidate_links$HE <- abs(candidate_links$direction - traj$GPS.Bearing[i])
    
    speed <- traj$GPS.Speed[i]/3.6
    hdop <- traj$GPS.HDOP[i]
    
    # Prepare data for FIS1: speed, HE, PD, HDOP
    newdata <- cbind(rep(speed, nrow(candidate_links)), 
                     candidate_links$HE,
                     candidate_links$PD, 
                     rep(hdop, nrow(candidate_links)))
    
    # Get FIS1 from the cache environment
    fis1 <- get("fis1", envir = cacheEnv)
    
    # Probability of being the correct link
    # Sometimes warnings are produced saying the data is out of the specified range,
    # but these have no negativ impact on the link identification
    candidate_links$pred <- predict(fis1, newdata)$predicted.val
    initial_links[i,] <- candidate_links[candidate_links$pred == 
                                       max(candidate_links$pred),][,c("V1", "V2", "edge_id", "direction", "NP_x", "NP_y")]
    
    # Decide if  a link is the initial link depending on the number
    # of points matched to the link
    if (i > 1) {
      if (identical(E(roads@g)[initial_links$edge_id[i]]$name, 
                    E(roads@g)[initial_links$edge_id[i - 1]]$name)) {
        count <- count +1
      } else {
        count <- 0
      } 
      if (count == 2) {
        initial_links <- initial_links[(i - 2):i,]
        found <- TRUE
      }
    }
    i <- i + 1
  }
  
  # Matched coordinates 
  traj$coords.x1[(i - 3):(i - 1)] <- initial_links$NP_x
  traj$coords.x2[(i - 3):(i - 1)] <- initial_links$NP_y
  #OSM ID of the roads 
  traj$OSM_ID[(i - 3):(i - 1)] <- E(roads@g)[initial_links$edge_id]$name
  
  list(traj = traj, index = i, current_link = initial_links[nrow(initial_links),])
}



## Create the error region with fixed size using UTM coordinates
## 
## Args:
##  x: lon
##  y: lat
##
## Returns:
##  Error region as SpatialPolygon
err_region <- function(x, y, size = 38) {  
  current_pt <- data.frame(x, y)
  coordinates(current_pt) <- ~x + y 
  proj4string(current_pt) <- osm_crs()
  
  # Transform to UTM
  UTMzone <- trunc((180 + 5) / 6) + 1
  UTM <- CRS(paste0("+proj=utm +zone=",UTMzone," +ellps=WGS84 +datum=WGS84"))
  current_pt <- spTransform(current_pt, UTM)
  x <- coordinates(current_pt )[1]
  y <- coordinates(current_pt )[2]
  
  # Create the error region with fixed size and transform back to osm_crs()
  x_rec <- c(x - size, x + size, x + size, x - size, x - size)
  y_rec <- c(y - size, y - size, y + size, y + size, y - size)
  rec <- cbind(x_rec, y_rec)
  rec <- Polygons(list(Polygon(rec, hole = FALSE)), 1)
  rec <- SpatialPolygons(list(rec), proj4string = UTM)
  rec <- spTransform(rec, osm_crs())
}

              





