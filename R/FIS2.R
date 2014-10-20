#' Fuzzy Inference System 2 (FIS2)
#' 
#' Fuzzy Inference System used in the Subsequent Map Matching Process
#' along a link (SMP-1).
#'
#' SMP-1 checks if the vehicle has already crossed a
#' junction or is about to cross one and matches subsequent positions to the
#' link identified by the IMP if that is not the case.  
#' Here you can see the input variables, fuzzy subsets and fuzzy rules used
#' in FIS2. It is usefull to know the variables and how they affect the rule 
#' outputs in case you plan to change the range of the fuzzy subsets.
#' 
#' 
#' 
#' Input variables to this FIS are:
#' \itemize{
#'   \item speed of the vehicle, v (m/s) 
#'   \item horizontal dilution of precision (HDOP)
#'   \item \eqn{\Delta d (m),} 
#'   the difference between the distance from the last matched position to
#'   the downstream junction and the distance travelled by the vehicle since
#'   the last position fix
#'   \item heading increment, HI (degrees)
#'   \item \eqn{\alpha and \beta (degrees),} 
#'   location of the current position fix, 
#'   relative to the link, seen from the last matched position and from the downstream junction respectively
#' }
#' 
#' 
#' 
#' The fuzzy subsets of the input variables are:
#' \itemize{
#'   \item high, low, zero 
#'   \item good, bad
#'   \item positiv, negativ
#'   \item small, large, 180
#'   \item below90, above90
#' }
#' 
#' 
#' 
#' The corresponding fuzzy rules used in this FIS are:
#' \itemize{
#' \item If (\eqn{\alpha} is below 90 ) and (\eqn{\beta} is below 90 degrees) then ( L2 is high)  
#' \item If (\eqn{\Delta} d is positive) and (\eqn{\alpha} is above 90 degrees) then ( L2 is low)  
#' \item If (\eqn{\Delta} d is positive) and (\eqn{\beta} is above 90 degrees) then ( L2 is low)  
#' \item If (HI is small) and (\eqn{\alpha}  is below 90 degrees) and (\eqn{\beta} is below 90 degrees) then (L2 is high)  
#' \item If (HI is small) and (\eqn{\Delta}d is positive) and (\eqn{\alpha} is above 90 degrees) then (L2 is low) 
#' \item If (HI is small) and (\eqn{\Delta}d is positive) and (\eqn{\beta} is above 90 degrees) then (L2 is low) 
#' \item If (HI is large) and (\eqn{\alpha} is below 90 degrees) and (\eqn{\beta} is below 90 degrees) then (L2 is low)  
#' \item If (HDOP is good) and (v is zero) then  (L2 is high) 
#' \item If (HDOP is good) and (\eqn{\Delta}d  is negative) then (L2 is average)     
#' \item If (HDOP is good) and (\eqn{\Delta}d  is positive) then (L2 is low) 
#' \item If (v is high) and (HI is small) then (L2 is average)    
#' \item If (HDOP is good) and (v is high) and (HI is 180 degrees) then (L2 is high) 
#' }
#'
#' @seealso
#' \link{get_var_bounds}, \link{set_var_bounds} 
#'
#' @examples
#' fis_smp1 <- get_fis("SMP1")
#' fis_smp1
#' # Plot membership functions
#' plotMF(fis_smp1)
#' 
#' @docType data
#' @keywords datasets
#' @format frbs object
#' @name FIS_SMP1
NULL

## Function that creates FIS2 used in SMP-1
##
## Returns:
##  frbs object
create_fis2 <- function() {
  # Define the shape and parameters of the membership functions
  # Parameters are aquired through the get_params() function
  # Please see fuzzifier function to contruct the matrix
  # First row is the type of membership function
  # Types used: 6 = sigmoidal
  var_bounds <- get_var_bounds()
  m <- matrix(c(6, get_params(var_bounds[1, 1], var_bounds[1, 2], "s"), #speed_high
           get_mid(var_bounds, 1), NA, NA,   
           6, get_params(var_bounds[2, 1], var_bounds[2, 2], "z"), #speed_low 
           get_mid(var_bounds, 2), NA, NA, 
           6, get_params(var_bounds[3, 1], var_bounds[3, 2], "z"), #speed_zero
           get_mid(var_bounds, 3), NA, NA, 
           6, get_params(var_bounds[8, 1], var_bounds[8, 2], "z"), #HDOP_good
           get_mid(var_bounds, 8), NA, NA, 
           6, get_params(var_bounds[9, 1], var_bounds[9, 2], "s"), #HDOP_bad
           get_mid(var_bounds, 9), NA, NA,
           6, get_params(var_bounds[10, 1], var_bounds[10, 2], "z"), #alpha_below90
           get_mid(var_bounds, 10), NA, NA,   
           6, get_params(var_bounds[11, 1], var_bounds[11, 2], "s"), #alpha_above90
           get_mid(var_bounds, 11), NA, NA, 
           6, get_params(var_bounds[12, 1], var_bounds[12, 2], "z"), #beta_below90
           get_mid(var_bounds, 12), NA, NA,   
           6, get_params(var_bounds[13, 1], var_bounds[13, 2], "s"), #beta_above90
           get_mid(var_bounds, 13), NA, NA,  
           6, get_params(var_bounds[14, 1], var_bounds[14, 2], "z"), #delta_dist_neg
           get_mid(var_bounds, 14), NA, NA, 
           6, get_params(var_bounds[15, 1], var_bounds[15, 2], "s"), #delta_dist_pos
           get_mid(var_bounds, 15), NA, NA, 
           6, get_params(var_bounds[16, 1], var_bounds[16, 2], "z"), #HI_small
           get_mid(var_bounds, 16), NA, NA, 
           6, get_params(var_bounds[17, 1], var_bounds[17, 2], "s"), #HI_large
           get_mid(var_bounds, 17), NA, NA, 
           4, 125, 150, 200, 225),#HI_180
         nrow = 5, byrow = FALSE)
  
  assign("varinp.mf2", m, envir = cacheEnv)


  # Names of the variables
  colnames.var2 <- c("speed", "HDOP", "alpha", "beta", "delta_dist", 
                   "HI", "HI1", "output")

  # Give the names of the fuzzy terms of each input variable
  # Names of the fuzzy terms must be unique
  varinput2.1 <- c("fast", "slow", "zero")
  varinput2.2 <- c("good", "bad")
  varinput2.3 <- c("below90", "above90")
  varinput2.4 <- c("below90b", "above90b")
  varinput2.6 <- c("pos", "neg")
  varinput2.7 <- c("small", "large")
  varinput2.8 <- c("180")
  names.varinput2 <- c(varinput2.1, varinput2.2, varinput2.3, varinput2.4,
                    varinput2.6, varinput2.7, varinput2.8)
  
  # Set interval of data."speed", "HDOP", "alpha", "beta", "delta_dist", 
  # "HI", "HI1", "output"
  range.data2 <- matrix(c(0, 50, 0, 20, 0, 360, 0, 360, -500, 500, 0, 360, 0, 360, 0, 100), nrow = 2)
  
  # Define number of fuzzy terms of input variables
  num.fvalinput2 <- matrix(c(3, 2, 2, 2, 2, 2, 1), nrow = 1)


  # Define the fuzzy IF-THEN rules; 
  # For TSK model, it isn't necessary to put linguistic terms in consequent parts.
  r1 <- c("dont_care", "and", "dont_care", "and","below90","and","below90b","and",
          "dont_care", "and", "dont_care", "and", "dont_care","->")
  r2 <- c("dont_care", "and","dont_care", "and","above90","and", "dont_care", "and",
          "pos","and", "dont_care", "and","dont_care", "->")
  r3 <- c("dont_care", "and","dont_care", "and","dont_care","and", "above90b", "and",
          "pos","and", "dont_care", "and","dont_care", "->")
  r4 <- c("dont_care", "and","dont_care", "and", "below90", "and","below90b","and",
          "dont_care", "and","small","and","dont_care", "->")
  r5 <- c("dont_care", "and","dont_care", "and", "above90", "and","dont_care","and",
          "pos", "and","small","and","dont_care", "->")
  r6 <- c("dont_care", "and","dont_care", "and", "dont_care", "and","above90b","and",
          "pos", "and","small","and","dont_care", "->")
  r8 <- c("dont_care", "and","dont_care", "and", "below90", "and","below90b","and",
          "dont_care", "and","large","and","dont_care", "->")
  r9 <- c("zero","and", "good", "and","dont_care", "and","dont_care", "and",
          "dont_care", "and","dont_care", "and","dont_care", "->")
  r10 <- c("dont_care", "and","good","and","dont_care", "and","dont_care", "and","neg", 
           "and","dont_care", "and","dont_care", "->")
  r11 <- c("dont_care", "and","good","and","dont_care", "and","dont_care", "and","pos", 
           "and","dont_care", "and","dont_care", "->")
  r12 <- c("fast","and","dont_care", "and","dont_care", "and","dont_care", "and",
           "dont_care", "and","small", "and", "dont_care", "->")
  r13 <- c("fast", "and","good","and","dont_care", "and","dont_care", "and",
           "dont_care", "and","dont_care", "and","180", "->")
  rule2 <- list(r1, r2, r3, r4, r5, r6, r8, r9, r10, r11, r12, r13)
  rule2 <- do.call(rbind, rule2)
  
    
  # Set the name of the simulation
  name2 <- "Sim-2"
  
  
  # Define linear functions of TSK  
  func.tsk2 <- matrix(c(100, 10, 10, 100, 10, 10, 10, 100, 50, 10, 50, 100), 
                      nrow = 12, byrow = TRUE)
  
  
    
  # Generate a fuzzy model with frbs.gen
  # For TSK model we do not need to input: 
  # num.fvaloutput, varout.mf, names.varoutput, type.defuz
  varinp.mf2 <- get("varinp.mf2", envir = cacheEnv)
  fis2 <- frbs.gen(range.data2, num.fvalinput2, names.varinput2, num.fvaloutput = NULL, 
                   varout.mf = NULL, names.varoutput = NULL, rule2, 
                   varinp.mf2, type.model, type.defuz = NULL, type.tnorm, type.snorm, func.tsk2, 
                   colnames.var2, type.implication.func, name2)
  
  assign("fis2", fis2, envir = cacheEnv)
}

create_fis2()

