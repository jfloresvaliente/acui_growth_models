#===============================================================================
# Name   : model_functions
# Author : Jorge Flores-Valiente & Marcelo Tobar
# Date   : 
# Version:
# Aim    : Set up model growth functions
# URL    : 
#===============================================================================

# LOGISTIC MODEL (Ansah - Frimpong 2015)
logistic_model <- function(Wmax, a, k, weight){
  # Wmax    = maximum weight
  # a       = initial rate of growth
  # k       = rate of decrease of growth with time
  # weight  = time serie of weight values (gr/30days)

  W <- numeric(length(weight))
  for(t in seq_along(weight)){
    W[t] <- Wmax/(1+exp(a-k*t))
  }
  return(W)
}

# GOMPERTZ MODEL (Ansah - Frimpong 2015)
gompertz_model <- function(W0, m0, k, weight){
  # W0      = theoretical weight that corresponds to age 0
  # m0      = initial instantaneous growth rate
  # k       = rate of decrease of m0
  # weight  = time serie of weight values (gr/30days)
  # t       = time in days
  
  W <- numeric(length(weight))
  for(t in seq_along(weight)){
    W[t] <- W0*exp(m0*(1-exp(-k*t))) 
  }
  return(W)
}

# QUADRATIC MODEL (Ansah - Frimpong 2015)
quadratic_model <- function(a, b, c, weight){
  # a       = intercept
  # b       = linear slope
  # c       = quadratic slope,
  # weight  = time serie of weight values (gr/30days)
  
  W <- numeric(length(weight))
  for(t in seq_along(weight)){
    W[t] <- a + b*t + c*t^2
  }
  return(W)
}

# VON BERTALANFFY MODEL (Ansah - Frimpong 2015)
VB_model <- function(Winf, t0, k, weight, time = 1:365){
  # Winf    = asymptotic weight
  # t0      = is the theoretical age-at-zero weight
  # k       = rate of growth of VB
  # weight  = time serie of weight values (gr/30days)
  W <- numeric(length(weight))
  for(t in time){
    W[t] <- Winf * (1- exp(-k*(t-t0)))^3
  }
  return(W)
}

#===============================================================================
# END OF PROGRAM
#===============================================================================
