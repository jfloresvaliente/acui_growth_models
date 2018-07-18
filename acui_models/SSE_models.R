#===============================================================================
# Name   : SSE_models
# Author : Jorge Flores-Valiente & Marcelo Tobar
# Date   : 
# Version:
# Aim    : 
# URL    : 
#===============================================================================

# ORDENAR LOS DATOS INICIALES ---------------------------------------------

# Leer los datos de SSE de todos los centros
dat <- read.table(file = 'F:/COLLABORATORS/Marcelo/SSE_models_out.csv', header = T, sep = ';')
model_names <- names(dat)[3:7] # Seleccionar los nombre de los modelos

dat_ordenado <- NULL
for(i in seq_along(dat$CENTER)){
  
  centro_ordenado <- NULL
  for(j in 1:length(model_names)){
    centro    <- subset(dat, dat$CENTER == i) # Elegir un centro para obtener sus datos
    inCenter  <- which (names(dat) == model_names[j]) # Indice de numero de columna del modelo
    
    CENTER    <- centro$CENTER # Numero que identifica al centro
    n         <- centro$n      # Numero de datos de cada centro
    mod       <- model_names[j] # Nombre del modelo
    SSE       <- centro[,inCenter] # SSE de cada modelo
    if(model_names[j] == 'POLYNOMIAL3') params <- centro$num.params2 else params <- centro$num.params # Numero de parametros de cada modelo
    DUMMY     <- centro$DUMMY
    LOCATION  <- as.character(centro$LOCATION)
    SEASON    <- centro$SEASON
    SWB       <- as.character(centro$SWB)
    COMPANY   <- centro$COMPANY
    
    centroROW <- c(CENTER, n, mod, SSE, params, DUMMY, LOCATION, SEASON, SWB, COMPANY)
    centro_ordenado <- rbind(centro_ordenado, centroROW)
  }
dat_ordenado <- rbind(dat_ordenado, centro_ordenado)
}
colnames(dat_ordenado) <- c('CENTER', 'n', 'model', 'SSE','params', 'DUMMY', 'LOCATION', 'SEASON', 'SWB', 'COMPANY')


# CALCULO DEL AIC ---------------------------------------------------------
dat     <- as.data.frame(dat_ordenado)
centers <- levels(as.factor(dat$CENTER))
models  <- levels(as.factor(dat$model))

AIC <- NULL
m <- 1
for(i in seq_along(centers)){
  for(j in models){
    centro  <- subset(dat, dat$CENTER == i & dat$model == j)
    n       <- as.numeric(as.character(centro$n))
    k       <- as.numeric(as.character(centro$params))
    SSE     <- as.numeric(as.character(centro$SSE))
    AICs    <- n * log10(SSE/n) + (2 * k) + ( (2 * k * (k+1)) / (n - k - 1) )
    
    AIC[m] <-AICs
    m <- m + 1
  }
}

dat$AIC <- AIC


# CALCULO DEL Wi ----------------------------------------------------------

dat <- subset(dat, dat$model != 'POLYNOMIAL3')

centers <- levels(as.factor(dat$CENTER))

centro_all <- NULL
for(i in seq_along(centers)){
  # print(i)
  centro <- subset(dat, dat$CENTER == i)
  # rankingAIC <- order(centro$AIC, decreasing = T)
  aic_min <- min(centro$AIC)
  delta_i <- centro$AIC - aic_min
  
  wi <- numeric()
  for(j in 1:length(delta_i)){
    numerador   <- exp(-1/2 * delta_i[j])
    denominador <- exp(-1/2 * delta_i[1]) + exp(-1/2 * delta_i[2]) + exp(-1/2 * delta_i[3]) + exp(-1/2 * delta_i[4])
    wi[j] <- numerador/denominador
  }
  
  centro$delta_i <- delta_i
  centro$wi <- wi
  centro$rankingWI <- abs(order(wi)-(length(wi)+1))
    order(wi, decreasing = T)
  
  sort(wi, method = 'shell', index.return = T)
  
  centroROW <- centro
  centro_all <- rbind(centro_all, centroROW)
}

dat_ordenado <- centro_all

write.table(x = dat_ordenado, file = 'F:/COLLABORATORS/Marcelo/SSE_models_out_ranking.csv', col.names = T, row.names = F)

