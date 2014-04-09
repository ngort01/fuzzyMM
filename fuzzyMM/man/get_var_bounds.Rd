\name{get_var_bounds}
\alias{get_var_bounds}
\title{Fuzzy subset range}
\usage{
get_var_bounds()
}
\description{
This function returns a data frame containing the bounds of
the fuzzy subsets of each fuzzy variable.
}
\details{
Each fuzzy variable has fuzzy subsets. For example, in
``speed is zero'', {speed} is the fuzzy variable and {zero}
is the fuzzy subset.

The bounds of the fuzzy subset are used to create the
membership functions, that are used to fuzzify the inputs
of the fuzzy inference system. These bounds represent the x
values at which the sigmoidal membership functions reach ~0
or ~1 respectivly(e.g. ``speed is high'' ranges from 3 to 6
m/s).
}
\examples{
get_var_bounds()
}
\author{
Nikolai Gorte <n.gorte@gmail.com>
}
\seealso{
\link{FIS_IMP}, \link{FIS_SMP1}, \link{FIS_SMP2}
}

