#' Fuzzy Inference System 1 (FIS1)
#' 
#' Fuzzy Inference System used in the Initial Map Matching Process (IMP).
#'  
#' The IMP is the first and very important part
#' of the map matching process since it is responsible for the identification of the
#' initial link.  
#' Here you can see the input variables, fuzzy subsets and fuzzy rules used
#' in FIS1. It is usefull to know the variables and how they affect the rule 
#' outputs in case you plan to change the range of the fuzzy subsets.
#' 
#' 
#' 
#' Input variables to this FIS are:
#' \itemize{
#'   \item speed of the vehicle, v (m/s) 
#'   \item heading error, HE (degrees)
#'   \item perpendicular distance to candidate link, PD (m)
#'   \item horizontal dilution of precision (HDOP)
#' }
#' 
#' 
#' 
#' The fuzzy subsets of the input variables are:
#' \itemize{
#'   \item high, low, zero 
#'   \item small, large
#'   \item short, long
#'   \item good, bad
#' }
#' 
#' 
#' 
#' The corresponding fuzzy rules used in this FIS are:
#' \itemize{
#' \item If ( v is high) and (HE is small) then (L1 is average)   
#' \item If (v is high) and (HE is large) then (L1 is low)                                  
#' \item If (HDOP is good) and (PD is short) then (L1 is average) 
#' \item If (HDOP is good) and (PD is long) then (L1 is low)   
#' \item If ( HE is small) and (PD is short) then ( L1 is high)    
#' \item If ( HE is large) and ( PD is long) then (L1 is low)  
#' }
#'
#' @examples
#' fis_imp <- get_fis("IMP")
#' fis_imp
#' # Plot membership functions
#' plotMF(fis_imp)
#' 
#' @seealso
#' \link{get_var_bounds}, \link{set_var_bounds} 
#' 
#' @docType data
#' @keywords datasets
#' @format frbs object
#' @name FIS_IMP
NULL

## Function that creates FIS1 used in IMP
##
## Returns:
##  frbs object
create_fis1 <- function() {
  # Define the shape and parameters of the membership functions
  # Parameters are aquired through the get_params() function
  # Please see fuzzifier function to contruct the matrix
  # First row is the type of membership function
  # Types used: 6 = sigmoidal
  var_bounds <- get_var_bounds()
  m <- matrix(c(6, get_params(var_bounds[1, 1], var_bounds[1, 2], "s"), #speed_high
           get_mid(var_bounds, 1), NA, NA,   
           6, get_params(var_bounds[2, 1], var_bounds[2, 2], "z"), #sped_low 
           get_mid(var_bounds, 2), NA, NA, 
           6, get_params(var_bounds[3, 1], var_bounds[3, 2], "z"), #speed_zero
           get_mid(var_bounds, 3), NA, NA, 
           6, get_params(var_bounds[4, 1], var_bounds[4, 2], "z"), #HE_small 
           get_mid(var_bounds, 4), NA, NA,  
           6, get_params(var_bounds[5, 1], var_bounds[5, 2], "s"), #HE_large
           get_mid(var_bounds, 5), NA, NA, 
           6, get_params(var_bounds[6, 1], var_bounds[6, 2], "z"), #PD_short
           get_mid(var_bounds, 6), NA, NA, 
           6, get_params(var_bounds[7, 1], var_bounds[7, 2], "s"), #PD_long
           get_mid(var_bounds, 7), NA, NA, 
           6, get_params(var_bounds[8, 1], var_bounds[8, 2], "z"), #HDOP_good
           get_mid(var_bounds, 8), NA, NA, 
           6, get_params(var_bounds[9, 1], var_bounds[9, 2], "s"), #HDOP_bad
           get_mid(var_bounds, 9), NA, NA), 
         nrow = 5, byrow = FALSE)
  assign("varinp.mf1", m, envir = cacheEnv)





  # Names of the variables
  colnames.var1 <- c("v", "HE", "PD", "HDOP", "output")
  
  # Give the names of the fuzzy terms of each input variable
  # Names of the fuzzy terms must be unique
  varinput.1 <- c("high", "low", "zero")
  varinput.2 <- c("small", "large")
  varinput.3 <- c("short", "long")
  varinput.4 <- c("good", "bad")
  names.varinput1 <- c(varinput.1, varinput.2, varinput.3, varinput.4)

  # Set interval of data. "v", "HE", "PD", "HDOP", "output"
  range.data1 <- matrix(c(0, 50, 0, 360, 0, 60, 0, 20, 0, 100), nrow = 2)

  # Define number of fuzzy terms of input variables.
  num.fvalinput1 <- matrix(c(3, 2, 2, 2), nrow = 1)


  # Set the name of the simulation
  name1 <- "Sim-1"


  # Define the fuzzy IF-THEN rules 
  # For TSK model, it isn't necessary to put linguistic terms in consequent parts
  r1 <- c("high","and","small","and","dont_care","and","dont_care", "->")
  r2 <- c("high","and","large", "and", "dont_care", "and", "dont_care", "->")
  r3 <- c("dont_care", "and", "dont_care", "and", "short","and","good", "->")
  r4 <- c("dont_care", "and", "dont_care", "and", "long","and","good", "->")
  r5 <- c("dont_care", "and", "small","and","short", "and", "dont_care", "->")
  r6 <- c("dont_care", "and", "large","and","long", "and", "dont_care", "->")
  rule1 <- list(r1, r2, r3, r4, r5, r6)
  rule1 <- do.call(rbind, rule1)
  
  # Define linear functions of TSK 
  func.tsk1 <- matrix(c(50, 10, 50, 10, 100, 10), nrow = 6, byrow = TRUE)




  # Generate a fuzzy model with frbs.gen
  # For TSK model we do not need to input: 
  # num.fvaloutput, varout.mf, names.varoutput, type.defuz
  varinp.mf1 <- get("varinp.mf1", envir = cacheEnv)
  fis1 <- frbs.gen(range.data1, num.fvalinput1, names.varinput1, num.fvaloutput = NULL, 
         varout.mf = NULL, names.varoutput = NULL, rule1, 
         varinp.mf1, type.model, type.defuz = NULL, type.tnorm, type.snorm, func.tsk1, colnames.var1, 
         type.implication.func, name1)

  assign("fis1", fis1, envir = cacheEnv)
}

create_fis1()









