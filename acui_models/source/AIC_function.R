#===============================================================================
# Name   : AIC_function
# Author : Jorge Flores-Valiente & Marcelo Tobar
# Date   : 
# Version:
# Aim    : Set up model growth functions
# URL    : 
#===============================================================================
AIC <- function(obs, sim, k){
  # k <- number of parameters fitted
  # obs <- observed data
  # sim <- predicted data
  AIC_value <- length(obs) * log(sum((obs-sim)^2)/ length(obs)) + 2*k + (2*k*(k+1)/(length(obs)-k-1))
}

#===============================================================================
# END OF PROGRAM
#===============================================================================
