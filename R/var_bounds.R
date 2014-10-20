## Cache environment for package variables
cacheEnv <- new.env()

## Internal function used to create the data.frame containing the bounds of the fuzzy subsets.
##
## Returns:
##  data.frame containing the the default range of the fuzzy subsets 
init_vars <- function(){
  left_bounds <- c(3, 2, 0, 20, 25, 10, 20, 3, 4, 85, 90, 85, 90, -5, -5, 10, 15, 150, 0, 0, 5, 10)
  right_bounds <- c(6, 4, 2, 45, 60, 40, 50, 5, 6, 100, 120, 100, 120, 5, 10, 20, 30, 200, 1, 1, 15, 25)
  df <- data.frame(left_bounds, right_bounds, row.names = c("speed_high", "speed_low", "speed_zero", "HE_small",
                                         "HE_large", "PD_short", "PD_long", "HDOP_good",
                                         "HDOP_bad", "alpha_low", "alpha_high", "beta_low", "beta_high", 
                                         "delta_dist_neg", "delta_dist_pos", 
                                         "HI_small", "HI_large", "HI_180", "connectivity_direct", 
                                         "connectivity_indirect", "dist_err_small", "dist_err_large"))
  df$ID <- 1:nrow(df)
  df
}

assign("var_bounds", init_vars(), envir = cacheEnv)



#' Fuzzy subset range
#'
#' This function returns a data frame containing the bounds
#' of the fuzzy subsets of each fuzzy variable.
#' 
#' @details
#' Each fuzzy variable has fuzzy subsets.
#' For example, in ``speed is zero'', {speed} is the fuzzy variable
#' and {zero} is the fuzzy subset. 
#' 
#' The bounds of the fuzzy subset are used 
#' to create the membership functions, that are used to fuzzify the
#' inputs of the fuzzy inference system.
#' These bounds represent the x values at which the sigmoidal 
#' membership functions reach ~0 or ~1 
#' respectivly(e.g. ``speed is high'' ranges from 3 to 6 m/s).   
#' 
#' @author Nikolai Gorte <n.gorte@@gmail.com>

#' @examples
#' get_var_bounds()
#' 
#' @seealso
#' \link{FIS_IMP}, \link{FIS_SMP1}, \link{FIS_SMP2}
#' 
#' @export 
get_var_bounds <- function() get("var_bounds", envir = cacheEnv)



#' Set bounds of fuzzy subsets
#'
#' This function allows to set the bounds for the fuzzy subsets
#' to adjust the membership functions to your needs.
#' These bounds represent the x values at which the sigmoidal 
#' membership functions reach ~0 or ~1 
#' respectivly(e.g. ``speed is high'' ranges from 3 to 6 m/s).   
#' 
#' @param name name of the variable which bounds should be changed.
#' @param bounds numeric vector containing the lower and upper bound.
#' @param default logical, restores the default bounds if true.
#' 
#' @author Nikolai Gorte <n.gorte@@gmail.com>

#' @examples
#' set_var_bounds("speed_high", c(4, 7))
#' get_var_bounds()
#' update_mf()
#' 
#' @seealso
#' \link{FIS_IMP}, \link{FIS_SMP1}, \link{FIS_SMP2}, \link{update_mf}
#' 
#' @export 
set_var_bounds <- function(name = c("speed_high", "speed_low", "speed_zero", "HE_small",
                                    "HE_large", "PD_short", "PD_long", "HDOP_good",
                                    "HDOP_bad", "alpha_low, alpha_high", "beta_low", 
                                    "beta_high", 
                                    "delta_dist_neg", "delta_dist_pos", "HI_small", 
                                    "HI_large", "HI_180", "connectivity_direct", 
                                    "connectivity_indirect", "dist_err_small", "dist_err_large"),
                           bounds = "numeric", default = FALSE) {
  if(default) {
    assign("var_bounds", init_vars(), envir = cacheEnv) 
  } else {
    name <- match.arg(name)
    if (is.null(bounds))
      stop ("No bounds specified!")
    if (!is(bounds, "numeric"))
      stop ("Bounds must be numeric!")
    if (!length(bounds) == 2)
      stop ("Bound must be of length 2!")
    
    var_bounds <- get_var_bounds()
    var_bounds[rownames(var_bounds) == name, 1] <- bounds[1]
    var_bounds[rownames(var_bounds) == name, 2] <- bounds[2]
    assign("var_bounds", var_bounds, envir = cacheEnv) 
  }
}



#' Update Membership Functions
#'
#' This function updates the membership functions after
#' the bounds of the fuzzy subsets are changed and reinitializes
#' the fuzzy interference systems.
#' 
#' @author Nikolai Gorte <n.gorte@@gmail.com>

#' @examples
#' update_mf()
#' 
#' @seealso
#' \link{FIS_IMP}, \link{FIS_SMP1}, \link{FIS_SMP2}
#' 
#' @export 
update_mf <- function() {
  create_fis1()
  create_fis2()
  create_fis3()
} 


#' Get Fuzzy Inference System
#' 
#' Get the Fuzzy Inference System for IMP, SMP1 or SMP2.
#' 
#' @param name Name of the process: IMP, SMP1 or SMP2
#' 
#' @author Nikolai Gorte <n.gorte@@gmail.com>
#' 
#' @examples
#' fis_imp <- get_fis("IMP")
#' fis_imp
#' # Plot membership functions
#' plotMF(fis_imp)
#' 
#' @export 
get_fis <- function(name = c("IMP", "SMP1", "SMP2")) {
  name <- match.arg(name)
  if (name == "IMP")
    get("fis1", envir = cacheEnv)
  else if(name == "SMP1")
    get("fis2", envir = cacheEnv)
  else if (name == "SMP2") 
    get("fis3", envir = cacheEnv)
}


