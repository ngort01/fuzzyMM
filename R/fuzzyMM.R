#' Map Matching
#'
#' Function that matches GPS trajectories to the OSM digital road network
#' using a fuzzy logic map matching algorithm.
#' 
#' @param traj \link[sp]{SpatialPointsDataFrame-class} or one of the \link[trajectories]{Track} 
#'              classes containing the GPS trajectories. See Details for additional info.
#' @param plot boolean. Matched trajectory will be plotted if true.
#' @param DRN optional DigitalRoadNetwork that should be used. 
#' @param err_region Radius of a circle around a GPS position.
#' @param ... not used.
#' 
#' @details
#' \bold{mm} is the main function of this package.
#' The input to the function is a \link[sp]{SpatialPointsDataFrame-class} 
#' or one of the \link[trajectories]{Track} classes
#' containing the GPS trajectory that should be map matched.
#' 
#' To succesfully apply the map matching algorithm the data part of the 
#' trajectories must include data for HDOP, Speed and Bearing, with the columns 
#' named "GPS.HDOP", "GPS.Speed" and "GPS.Bearing" respectively.
#' Values for GPS.Speed must be given in km/h.
#' Missing values in the data will be replaced with zeros and can lead
#' to incorrect matchings.
#' 
#' The map data is obtained from OpenStreetMap. 
#' 
#' The algorithm consists of three major parts responsible for the identification of
#' the links the vehicle is travelling on.
#' \itemize{
#'   \item Initial MapMatching Process (IMP)
#'   \item Subsequent MapMatching Process along a link (SMP-1)
#'   \item Subsequent MapMatching Process at a junction (SMP-2)
#' }
#' Each of this processes uses a Fuzzy Inference System (FIS) for the
#' link identification.
#' Input variables, fuzzy subsets and fuzzy rules for each FIS can be
#' seen in \code{\link{FIS_IMP}}, \code{\link{FIS_SMP1}} and 
#' \code{\link{FIS_SMP2}} or by getting the corresponding \code{\link{frbs}}
#' object using \code{\link{get_fis}}.
#' 
#' A detailed description of the fuzzy logic map matching
#' algorithm and the FIS can be found in Quddus (2006).
#' 
#' 
#' 
#' @author Nikolai Gorte <n.gorte@@gmail.com>
#' 
#' @seealso
#' \code{\link{FIS_IMP}}, \code{\link{FIS_SMP1}}, \code{\link{FIS_SMP2}},
#' \code{\link{get_fis}}, \code{\link{frbs}}
#' 
#' @references
#' Quddus, M. A. 2006 (January). \emph{High Integrity Map Matching Algorithms 
#' for Advanced Transport Telematics Applications}. Ph.D. thesis, 
#' Imperial College London, United Kingdom.
#' 
#' 
#' @examples
#' \dontrun{  
#' data(traj) ## SpatialPointsDataFrame
#' matched_traj <- mm(traj, plot = TRUE)
#' 
#' ## Create Track
#' require(trajectories)
#' require(spacetime)
#' traj_points <- SpatialPoints(coordinates(traj),CRS(proj4string(traj)))
#' track <- STIDF(traj_points, traj$time, traj@@data)
#' track <- Track(track)
#' matched_track <- mm(track, plot = TRUE)
#' }
#' @export
#' @rdname mm
setGeneric(
  name = "mm",
  def = function(traj, ...) standardGeneric("mm")
)


#' @rdname mm
mm.SpatialPointsDataFrame <- function(traj, plot = FALSE, DRN = NULL, err_region = 38) {
  if (!is(traj, "SpatialPointsDataFrame")) 
    stop ("Not a SpatialPointsDataFrame object!")
  if (is.null(proj4string(traj)))
    stop ("No projection specified!")
  if (!all(c("GPS.Bearing", "GPS.HDOP", "GPS.Speed") %in% names(traj))) 
    stop("Trajectory does not contain all the necessary data (GPS.Bearing, GPS.HDOP, GPS.Speed)!")
  if (!is(traj$time, "POSIXct") && !is(traj$time, "POSIXlt"))
    stop ("time must be of class POSIXct or POSIXlt!")
  
  traj <- spTransform(traj, osm_crs())
  coordnames(traj) <- c("coords.x1", "coords.x2")
  traj@data[is.na(traj@data)] <- 0
  bbox <- bbox(traj)
  
  # Create digital road network
  if (!is(DRN, "DigitalRoadNetwork")) {
    roads <- create_drn(bbox)
  } else {
    roads <- DRN
  }

  traj <- as(traj, "data.frame")
  traj$OSM_ID <- 0
  
  # Execute the Initial Map-Matching Process (IMP)
  list <- imp(traj, roads, err_region)
  edit_traj <- list$traj
  pt_index <- list$index
  current_link <- list$current_link
  
  # Map match remainig points using SMP1 and SMP2
  for (j in pt_index:nrow(edit_traj)) {
    # Check the possibility of matching the next point on the current link
    pred_val <- smp1(edit_traj, roads, current_link, j)$predicted.val
    if (pred_val >= 60) {
	  if (!requireNamespace("geosphere", quietly = TRUE))
	  	stop("package geosphere required")
      PD <- geosphere::dist2Line(edit_traj[,c("coords.x1", "coords.x2")][j,], 
                      roads@sl@lines[[current_link$edge_id]]@Lines[[1]]@coords)
      edit_traj$coords.x1[j] <- PD[2]
      edit_traj$coords.x2[j] <- PD[3]
      edit_traj$OSM_ID[j] <- edit_traj$OSM_ID[j - 1]
      #pt_index <- pt_index + 1
    } else {
      current_link <- smp2(edit_traj, roads, current_link, j, err_region)
      edit_traj$coords.x1[j] <- current_link$NP_x
      edit_traj$coords.x2[j] <- current_link$NP_y
      edit_traj$OSM_ID[j] <- E(roads@g)[current_link$edge_id]$name
      #pt_index <- pt_index + 1
    }
  }
  
 data <- edit_traj[,!names(edit_traj) %in% c("coords.x1", "coords.x2")]
 matched_coords <- edit_traj[,c("coords.x1", "coords.x2")]
 matched_traj <- SpatialPointsDataFrame(matched_coords, data, proj4string=osm_crs())
 
 if (plot) {
   plot(traj$coords.x1, traj$coords.x2, pch = 16, col = "blue")
   points(matched_traj$coords.x1, matched_traj$coords.x2,pch = 16, col = "red")
   lines(roads@sl)
 }
 matched_traj
}


#' @rdname mm
setMethod("mm", signature("SpatialPointsDataFrame"), mm.SpatialPointsDataFrame)


#' @rdname mm
mm.Track <- function(traj, plot = FALSE, DRN = NULL, err_region = 38) {
  track <- SpatialPointsDataFrame(traj@sp, traj@data, proj4string=proj4string(traj), bbox = bbox(traj))
  track <- mm(track, plot)
  track_points <- SpatialPoints(coordinates(track),CRS(proj4string(track)))
  track <- STIDF(track_points, traj@time, track@data)
  track <- Track(track)
  track
}

#' @rdname mm
setMethod("mm", signature("Track"), mm.Track)


#' @rdname mm
mm.Tracks <- function(traj, plot = FALSE, DRN = NULL, err_region = 38) {
  tracks <- list()
  for (i in 1:dim(traj)[[1]]) {
    tracks[i] <- mm(traj[i]) 
  }
  tracks <- Tracks(tracks)
  tracks
}

#' @rdname mm
setMethod("mm", signature("Tracks"), mm.Tracks)


#' @rdname mm
mm.TracksCollection <- function(traj, plot = FALSE, DRN = NULL, err_region = 38) {
  trcol <- list()
  for (i in 1:dim(traj)[[1]]) {
    trcol[i] <- mm(traj[i])
  }
  trcol <- TracksCollection(trcol)
  trcol
}

#' @rdname mm
setMethod("mm", signature("TracksCollection"), mm.TracksCollection)
