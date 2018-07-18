#===============================================================================
# Name   : cost_functions
# Author : Jorge Flores-Valiente & Marcelo Tobar
# Date   : 
# Version:
# Aim    : Function to maps an event or values of one or more variables onto a real 
#          number intuitively representing some "cost" associated with the event.
#          The output of these functions is Residual Sum of Squares
# URL    : 
#===============================================================================

# COST FUNCTION FOR LOGISTIC MODEL
logistic_cost <- function(par){
  # par     = input parameters of the model
  Wmax  = par[1]
  a     = par[2]
  k     = par[3]
  
  sim = logistic_model(Wmax = Wmax, a = a, k = k, weight = weight)
  RSS = sum((sim-weight)^2, na.rm=TRUE)
  return(RSS)
}

# COST FUNCTION FOR GOMPERTZ MODEL
gompertz_cost <- function(par){
  # par     = input parameters of the model
  W0    = par[1]
  m0    = par[2]
  k     = par[3]
  
  sim = gompertz_model(W0 = W0, m0 = m0, k = k, weight = weight)
  RSS = sum((sim-weight)^2, na.rm=TRUE)
  return(RSS)
}

# COST FUNCTION FOR QUADRATIC MODEL
quadratic_cost <- function(par){
  # par     = input parameters of the model
  a     = par[1]
  b     = par[2]
  c     = par[3]
  
  sim = quadratic_model(a = a, b = b, c = c, weight = weight)
  RSS = sum((sim-weight)^2, na.rm=TRUE)
  return(RSS)
}

# COST FUNCTION FOR VON BERTALANFFY
VB_cost <- function(par){
  # par     = input parameters of the model
  Winf  = par[1]
  t0    = par[2]
  k     = par[3]
  
  sim = VB_model(Winf = Winf, t0 = t0, k = k, weight = weight)
  RSS = sum((sim-weight)^2, na.rm=TRUE)
  return(RSS)
}

#===============================================================================
# END OF PROGRAM
#===============================================================================
