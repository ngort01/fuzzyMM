#' Fuzzy Inference System 3 (FIS3)
#' 
#' Fuzzy Inference System used in the Subsequent Map Matching Process
#' at a junction (SMP-2).
#'  
#'  
#' SMP-2 identifies a new link among the candidate
#' links if the vehicle has crossed a junction.
#' Here you can see the input variables, fuzzy subsets and fuzzy rules used
#' in FIS3. It is usefull to know the variables and how they affect the rule 
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
#'   \item link connectivity
#'   \item distance error
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
#'   \item indirect, direct
#'   \item small2, large2
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
#' \item If (The connectivity with the previous link is low) then (The L3 is low)
#' \item If (The connectivity with the previous link is high) then (The L3 is high)
#' \item If (The  distance error is low) then (The L3 is high)
#' \item If (The distance error  is high) then (The L3 is low) 
#' }
#' 
#' @examples
#' fis_smp2 <- get_fis("SMP2")
#' fis_smp2
#' # Plot membership functions
#' plotMF(fis_smp2)
#' 
#' @seealso
#' \link{get_var_bounds}, \link{set_var_bounds} 
#' 
#' @docType data
#' @keywords datasets
#' @format frbs object
#' @name FIS_SMP2
NULL


## Function that creates FIS3 used in SMP-2
##
## Returns:
##  frbs object
create_fis3 <- function() {
  # Define the shape and parameters of the membership functions
  # Parameters are aquired through the get_params() function
  # Please see fuzzifier function to contruct the matrix
  # First row is the type of membership function
  # Types used: 1 = trinagle, 6 = sigmoidal
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
                get_mid(var_bounds, 9), NA, NA,
                1, -1, 0, 1, NA, #indirect
                1, 0, 1, 2, NA, #direct
                6, get_params(var_bounds[21, 1], var_bounds[21, 2], "z"), #dist_err_small
                get_mid(var_bounds, 21), NA, NA,
                6, get_params(var_bounds[22, 1], var_bounds[22, 2], "s"), #dist_err_large
                get_mid(var_bounds, 22), NA, NA),
              nrow = 5, byrow = FALSE)
  
  assign("varinp.mf3", m, envir = cacheEnv)
  
  
  # Names of the variables
  colnames.var3 <- c("v", "HE", "PD", "HDOP", "Connectivity", "dist_err", "output")
  
  # Give the names of the fuzzy terms of each input variable.
  # Names of the fuzzy terms must be unique
  varinput3.1 <- c("high", "low", "zero")
  varinput3.2 <- c("small", "large")
  varinput3.3 <- c("short", "long")
  varinput3.4 <- c("good", "bad")
  varinput3.5 <- c("indirect", "direct")
  varinput3.6 <- c("small2", "large2")
  names.varinput3 <- c(varinput3.1, varinput3.2, varinput3.3, varinput3.4,
                       varinput3.5, varinput3.6)
  
  # Set interval of data, "v", "HE", "PD", "HDOP", "Connectivity", "dist_err", "output".
  range.data3 <- matrix(c(0, 50, 0, 360, 0, 60, 0, 20, 0, 1, 0, 1000, 0, 100), nrow=2)
  
  # Define number of fuzzy terms of input variables
  num.fvalinput3 <- matrix(c(3, 2, 2, 2, 2, 2), nrow=1)
  
  
  # Set the name of the simulation.
  name3 <- "Sim-3"
  
  
  
  # Define linear functions of TSK 
  # The dimension of this matrix is: 
  # [<number_of_rules>, <number_of_variables> + 1]
  func.tsk3 <- matrix(c(50, 10, 50, 10, 100, 
                        10, 10, 100, 100, 10), nrow = 10, byrow = TRUE)
  
  
  
  # Define the fuzzy IF-THEN rules; 
  # For TSK model, it isn't necessary to put linguistic terms in consequent parts.
  r1 <- c("high","and","small","and","dont_care","and","dont_care", 
          "and","dont_care","and","dont_care","->")
  r2 <- c("high","and","large", "and", "dont_care", "and", "dont_care", 
          "and","dont_care","and","dont_care","->")
  r3 <- c("dont_care", "and", "dont_care", "and", "short","and","good", 
          "and","dont_care","and","dont_care","->")
  r4 <- c("dont_care", "and", "dont_care", "and", "long","and","good", 
          "and","dont_care","and","dont_care","->")
  r5 <- c("dont_care", "and", "small","and","short", "and", "dont_care", 
          "and","dont_care","and","dont_care","->")
  r6 <- c("dont_care", "and", "large","and","long", "and", "dont_care", 
          "and","dont_care","and","dont_care","->")
  r7 <- c("dont_care","and","dont_care","and","dont_care","and","dont_care",
          "and","indirect","and","dont_care", "->")
  r8 <- c("dont_care","and","dont_care","and","dont_care","and","dont_care",
          "and","direct","and","dont_care", "->")
  r9 <- c("dont_care","and","dont_care","and","dont_care","and","dont_care","and",
          "dont_care","and","small2","->")
  r10 <- c("dont_care","and","dont_care","and","dont_care","and","dont_care","and",
           "dont_care","and","large2","->")
  rule3 <- list(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)
  rule3 <- do.call(rbind, rule3)
  
  # Generate a fuzzy model with frbs.gen
  # For TSK model we do not need to input: 
  # num.fvaloutput, varout.mf, names.varoutput, type.defuz
  varinp.mf3 <- get("varinp.mf3", envir = cacheEnv)
  fis3 <- frbs.gen(range.data3, num.fvalinput3, names.varinput3, num.fvaloutput = NULL, 
                   varout.mf = NULL, names.varoutput = NULL, rule3, 
                   varinp.mf3, type.model, type.defuz = NULL, type.tnorm, type.snorm, func.tsk3, colnames.var3, 
                   type.implication.func, name3)
  
  assign("fis3", fis3, envir = cacheEnv)
}

create_fis3()

