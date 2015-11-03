#' Map Matching Using a Fuzzy Logic Based Algorithm.
#'
#' fuzzyMM is a package that implements a fuzzy logic based map
#' matching algorithm.
#' More information about each function can be found in its help 
#' documentation.
#'
#' Map matching is the process of matching inaccurate GPS trajectories to 
#' a digital road network.
#' The algorithm implemented in this package uses fuzzy
#' logic to solve this problem.
#' 
#' A detailed description of the fuzzy logic map matching
#' algorithm can be found in Quddus (2006).
#' 
#' 
#' @seealso \code{\link{mm}}
#' @author Nikolai Gorte <n.gorte@@gmail.com>
#' @references
#' Quddus, M. A. 2006 (January). \emph{High Integrity Map Matching Algorithms 
#' for Advanced Transport Telematics Applications}. Ph.D. thesis, 
#' Imperial College London, United Kingdom.
#' 
#' @docType package
#' @name fuzzyMM-package
#' @import methods frbs osmar sp spacetime trajectories
#' @importFrom igraph as.undirected V "V<-" get.edgelist E shortest.paths
NULL
