## Subsequent MapMatching Process (SMP-2) at a junction 
smp2 <- function(traj, roads = "DigitalRoadNetwork", current_link, pt_index = "numeric", err_region) {
    
  lon <- traj$coords.x1[pt_index] 
  lat <- traj$coords.x2[pt_index] 
  rec <- err_region(lon, lat, err_region)

  current_pt <- cbind(lon, lat)
  last_fix <- cbind(traj$coords.x1[pt_index - 1], traj$coords.x2[pt_index - 1])
  edge_id <- current_link$edge_id
  
  # Current selected link becomes the previous link because SMP-2 chooses a new link
  prev_link <- current_link
  
  # make R CMD check happy
  name <- NULL
  from <- NA 
  rm(from)
  
  # Check which node of the prev_link is the end node
  if (0 <= prev_link$direction && prev_link$direction <= 180) {
    prev_link_end <- ifelse(V(roads@g)[name == prev_link$V1]$lon >= V(roads@g)[name == prev_link$V2]$lon
                                ,prev_link$V1, prev_link$V2)
  } else {
    prev_link_end <- ifelse(V(roads@g)[name == prev_link$V1]$lon < V(roads@g)[name == prev_link$V2]$lon
                               ,prev_link$V1, prev_link$V2)
  }
    
  if (!requireNamespace("rgeos", quietly = TRUE))
	stop("package rgeos required")
  # Get edges inside the error region 
  candidate_links <- data.frame(edge_id = unique(c(which(rgeos::gIntersects(rec, roads@sl, byid = TRUE)), 
                                                   which(rgeos::gContains(rec, roads@sl, byid = TRUE)))))
  # Nodes of the candidate links
  candidate_links$V1 <- get.edgelist(roads@g)[candidate_links$edge_id, 1]
  candidate_links$V2 <- get.edgelist(roads@g)[candidate_links$edge_id, 2]
  candidate_links <- candidate_links[!candidate_links$edge_id == edge_id,]
 
  # Check if the line segments are connected to the prev_link
  candidate_links$conn <- sapply(candidate_links[,c("edge_id")],                         
                                 function(x) {
                                   # edges connected to the previously selected link
                                   conn_edges <- E(roads@g)[from(prev_link_end)] 
                                   if (isTRUE(any(as.vector(conn_edges) == x))) 1 else 0
                                   })
  
  if (!requireNamespace("geosphere", quietly = TRUE))
	stop("package geosphere required")
  # Calculate the perpendicular distance from the current point to all 
  # segments inside the error region and the closest point on the segments
  #str(candidate_links[,c("edge_id")])
  PD <- sapply(candidate_links[,c("edge_id")], 
            function(x) geosphere::dist2Line(current_pt, roads@sl@lines[[x]]@Lines[[1]]@coords)) 
  
  #str(PD) -- EP: might be list(), which then breaks;
  #str(class(PD))
  str(candidate_links)
  if (length(PD) == 0) {
    # Perpendicular distance
    #candidate_links$PD <- 1e9 # large
    # Nearest point
    #candidate_links$NP_x <- 0
    #candidate_links$NP_y <- 0
  } else {
    # Perpendicular distance
    candidate_links$PD <- PD[1,]
    # Nearest point
    candidate_links$NP_x <- PD[2,]
    candidate_links$NP_y <- PD[3,]
  }
    
  # Calculate the beraing of the segments
  # If a segment is defined by the points a and b, bearing can be:
  # bearing(a,b) or bearing(b,a)
  # Which one is chosen depends on the differnce between the bearing and the GPS.Bearing
  gps_bearing <- traj$GPS.Bearing[pt_index]
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
  candidate_links$HE <- abs(candidate_links$direction - traj$GPS.Bearing[pt_index])
  
  # Distance (m) from last fix to the end node of the prev_link
  end_node <- cbind(V(roads@g)[name == prev_link_end]$lon, V(roads@g)[name == prev_link_end]$lat)
  d1 <- spDists(end_node, last_fix, longlat = TRUE) * 1000
  
  
  # Shortest path from prev_link_end to the segments and closest vertex of the segments
  sp <- as.data.frame(do.call(rbind, 
                              lapply(1:nrow(candidate_links), 
                                     function(x) {
                                       spV1 <- shortest.paths(roads@g, prev_link_end, candidate_links$V1[x])
                                       spV2 <- shortest.paths(roads@g, prev_link_end, candidate_links$V2[x])
                                       if (spV1 < spV2) {
                                         c(candidate_links$V1[x], spV1)
                                       } else {
                                         c(candidate_links$V2[x], spV2)}})))
  
  candidate_links$cl_vertex <- as.character(as.vector(sp[,1]))
  # length of the shortest path (m)
  candidate_links$sp <- as.numeric(as.vector(sp[,2]))
  candidate_links$sp[is.infinite(candidate_links$sp)] <- 300
  
  # Distance on the candidate links: from the start node of the link to
  # the nearest point on the link from the current position fix
  candidate_links$d_link <- apply(candidate_links[,c("NP_x", "NP_y", "cl_vertex")], 1,
                            function(z) 
                              spDists(cbind(V(roads@g)[name == z[3]]$lon,V(roads@g)[name == z[3]]$lat),
                                                cbind(as.numeric(z[1]), as.numeric(z[2])), 
                                                longlat = TRUE) * 1000)
  
  # Distance travelled since last position fix
  t <- as.double(traj$time[pt_index] - traj$time[pt_index-1])
  d <- (traj$GPS.Speed[pt_index]/3.6) * t
  
  # Distance error
  candidate_links$dist_err <- apply(candidate_links[,c("sp", "d_link")], 1,
                              function(x) abs(d - (d1 + x[1] + x[2]))) 

  
  speed <- traj$GPS.Speed[pt_index] / 3.6
  hdop <- traj$GPS.HDOP[pt_index]
  
  # Prepare data for FIS3: speed, HE, PD, HDOP, connectivity, dist_err
  newdata <- cbind(rep(speed, nrow(candidate_links)), 
                   candidate_links$HE,
                   candidate_links$PD, 
                   rep(hdop, nrow(candidate_links)), 
                   candidate_links$conn, 
                   candidate_links$dist_err)
  
  fis3 <- get("fis3", envir = cacheEnv)
  # Probability of being the correct link
  # Sometimes warnings are produced saying the data is out of the specified range,
  # but these have no negativ impact on the link identification
  candidate_links$pred <- predict(fis3, newdata)$predicted.val
  
  # Current link the vehicle is traveling on
  current_link <- candidate_links[which.max(candidate_links$pred),c("V1", "V2", "edge_id", "direction", "NP_x", "NP_y")] 
  
  current_link 
}
