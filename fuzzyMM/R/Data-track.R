#' enviroCar GPS trajectory
#' 
#' A dataset containing GPS postions of a vehicle and data of several measured phenomenons.
#' 
#' \itemize{
#'   \item id. 
#'   \item time. 
#'   \item MAF: Mass Air Flow, l/s. 
#'   \item Engine.Load, \%.
#'   \item GPS.PDOP: Positional Dilution of Precision, precision.
#'   \item Intake.Temperature, deg. C.
#'   \item Calculated.MAF: Calculated Mass Air Flow, g/s.
#'   \item Throttle.Position, \%.
#'   \item O2.Lambda.Voltage.ER, ratio.
#'   \item Rpm, u/min.
#'   \item GPS.Bearing, deg.
#'   \item GPS.Speed, km/h.
#'   \item O2.Lambda.Voltage, V.
#'   \item Intake.Pressure, kPa.
#'   \item GPS.Altitude, m.
#'   \item GPS.HDOP: Horizontal Dilution of Precision, precision.
#'   \item GPS.VDOP: Vertical Dilution of Precision, precision.
#'   \item Speed, km/h.
#'   \item GPS.Accuracy, \%.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format \link[sp]{SpatialPointsDataFrame-class} with 82 observations of  19 variables. 
#' @source enviroCar: \url{https://envirocar.org/api/stable/tracks/52f3836be4b0d8e8c27ed6f0}
#' @name traj
NULL