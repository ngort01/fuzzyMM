## Subsequent MapMatching Process (SMP-1) along a link 
smp1 <- function (traj, roads = "DigitalRoadNetwork", current_link, pt_index = "numeric") {
  
  last_fix <- cbind(traj$coords.x1[pt_index - 1], traj$coords.x2[pt_index - 1])
  current_pt <- cbind(traj$coords.x1[pt_index], traj$coords.x2[pt_index])
  edge_id <- current_link$edge_id
  name <- NULL # make R CMD check happy

    
  if (0 <= current_link$direction && current_link$direction <= 180) {
    current_link_end <- ifelse(V(roads@g)[name == current_link$V1]$lon 
                               >= V(roads@g)[name == current_link$V2]$lon
                               ,current_link$V1, current_link$V2)
  } else {
    current_link_end <- ifelse(V(roads@g)[name == current_link$V1]$lon 
                               < V(roads@g)[name == current_link$V2]$lon
                               ,current_link$V1, current_link$V2)
  }
  
  if (!requireNamespace("geosphere", quietly = TRUE))
	stop("package geosphere required")
  # location of the current position fix, relative to the link, as seen from
  # the last matched position
  alpha <- abs(geosphere::bearing(last_fix, current_pt) - current_link$direction)
  
  # location of the current position fix, relative to the link, as seen from
  # the downstream junction
  beta <- abs(geosphere::bearing(current_pt, cbind(V(roads@g)[name == current_link_end]$lon,
                                    V(roads@g)[name == current_link_end]$lat))
              - current_link$direction)
  
  # Heading increment
  HI <- abs(traj$GPS.Bearing[pt_index] - traj$GPS.Bearing[pt_index - 1])
  
  # Distance from the last fix to the downstream junction (m)
  d1 <- spDists(last_fix, cbind(V(roads@g)[name == current_link_end]$lon,
                                V(roads@g)[name == current_link_end]$lat), 
                longlat = TRUE) * 1000
  
  # Distance travelled since last position fix
  t <- as.double(traj$time[pt_index] - traj$time[pt_index-1])
  d2 <- (traj$GPS.Speed[pt_index]/3.6) * t
  
  # the difference between the distance from the last matched position to
  # the downstream junction and the distance travelled by the vehicle since
  # the last position fix
  delta_d <- d1 - d2
  
  speed <- traj$GPS.Speed[pt_index] / 3.6
  hdop <- traj$GPS.HDOP[pt_index]
  newdata <- cbind(speed, hdop, alpha, beta, delta_d, HI, HI)
  fis2 <- get("fis2", envir = cacheEnv)
  
  # Sometimes warnings are produced saying the data is out of the specified range,
  # but these have no negativ impact on the link identification
  pred_val <- predict(fis2, newdata)
  
  # Probability of matching the psotion fix on the current link
  pred_val
}


