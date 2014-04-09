## Set model: Takagi Sugeno Kang (TSK).
type.model <- "TSK"

## Use standard t-norm and s-norm.
type.tnorm <- "MIN"
type.snorm <- "MAX"

## Type of implication function
type.implication.func <- "MIN"




## Gets the coefficients for the sigmoidal membership functions.
##
## Args:
##  left: x values at which the sigmoidal membership function reachs ~0
##  right: x values at which the sigmoidal membership function reachs ~1
##  shape: shape of the sigmoidal membership function. s = ascending, z = descending
##
## Returns:
##  slope of the function at the crossover point
get_params <- function(l, r, shape = c("s", "z")) {
  shape <- match.arg(shape)
  if (shape == "s") 
    y <- c(0.01, 0.5, 0.99)
  else
    y <- c(0.99, 0.5, 0.01)
  x <- c(l, (l + r)/2, r)
  slope <- ifelse(shape == "s", 1/(r - l),  1/(r - l))
  data <- list(x = x, y = y)
  fitModel <- nls(y ~ a/(1 + exp(-b * (x - ((l + r)/2)))), 
                  data, start = c(a = 1, b = slope), 
                  algorithm = "port")  
  # get the coefficients using the coef function
  params <- coef(fitModel)
  params[2]
}

## gets parameter c 
get_mid <- function(bounds, row) {
  (bounds[row, 1] + bounds[row, 2])/2
}


