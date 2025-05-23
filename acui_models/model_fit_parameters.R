#===============================================================================
# Name   : model_fit_parameters
# Author : Jorge Flores-Valiente & Marcelo Tobar
# Date   : 
# Version:
# Aim    : 
# URL    : 
#===============================================================================
source('source/model_functions.R')
source('source/cost_functions.R')
source('source/residual_sum_functions.R')
source('source/AIC_function.R')

outpath <- 'F:/COLLABORATORS/Marcelo/acui_models/output/'
dat <- read.table('F:/COLLABORATORS/Marcelo/acui_models/input/centros_acui.csv', header = T, sep = ';')
centros <- levels(dat$Center) # obtener los nombre de los centros

# dat2 <- subset(dat, dat$Center %nin% c('Auli', 'Chanco'))

fit_parameters <- NULL
for(i in centros){
  print(i)
  serie1 <- subset(dat, dat$Center == i)
  weight <- serie1$Weigth
  days <- serie1$Days
  n <- seq(0,540,0.1)
  
  # Logistic Model
  logi_par = c(Wmax = weight[length(weight)], a = 0.5, k = 0.3)
  # Los parámetros de entrada los puse forma abritraria. Podemos usar los de un paper y mejoraría el ajuste
  logi_cos = logistic_cost(par=logi_par)
  logi_opt = optim(par=logi_par, fn= logistic_cost) # usando algoritmo de OPTIMIZACION de R-base
  logi_sim = logistic_model(Wmax = logi_opt$par[1], a = logi_opt$par[2], k = logi_opt$par[3], weight = weight)
  logi_R2  = 1 - (sum((weight-logi_sim)^2)/sum((weight-mean(weight))^2))
  k = length(logi_opt$par)
  logi_AIC = AIC(obs = weight, sim = logi_sim, k = k)
  
  
  # Gomperts Model
  gomp_par = c(W0 = weight[1], m0 = 0.5, k = 0.3)
  # Los parámetros de entrada los puse forma abritraria. Podemos usar los de un paper y mejoraría el ajuste
  gomp_cos = gompertz_cost(par=gomp_par)
  gomp_opt = optim(par=gomp_par, fn= gompertz_cost) # usando algoritmo de OPTIMIZACION de R-base
  gomp_sim = gompertz_model(W0 = gomp_opt$par[1], m0 = gomp_opt$par[2], k = gomp_opt$par[3], weight = weight)
  gomp_R2  = 1 - (sum((weight-gomp_sim)^2)/sum((weight-mean(weight))^2))
  k = length(gomp_opt$par)
  gomp_AIC = AIC(obs = weight, sim = gomp_sim, k = k)
  
  # Quadratic Model
  quad_par = c(a = 2, b = 3, c = 4)
  # Los parámetros de entrada los puse forma abritraria. Podemos usar los de un paper y mejoraría el ajuste
  quad_cos = quadratic_cost(par=quad_par)
  quad_opt = optim(par=quad_par, fn= quadratic_cost) # usando algoritmo de OPTIMIZACION de R-base
  quad_sim = quadratic_model(a = quad_opt$par[1], b = quad_opt$par[2], c = quad_opt$par[3], weight = weight)
  quad_R2  = 1 - (sum((weight-quad_sim)^2)/sum((weight-mean(weight))^2))
  k = length(quad_opt$par)
  quad_AIC = AIC(obs = weight, sim = quad_sim, k = k)
  
  # # Von Bertalanffy model
  # VonB_par  = c(Winf = weight[length(weight)], t0 = 0, k = 0.3)
  # # Los parámetros de entrada los puse forma abritraria. Podemos usar los de un paper y mejoraría el ajuste
  # VonB_cost = VB_cost(par = VonB_par)
  # VonB_opt  = optim(par = VonB_par, fn = VB_cost)
  # VonB_sim  = VB_model(Winf = VonB_opt$par[1], t0 = VonB_opt$par[2], VonB_opt$par[3], weight = weight, time = days)
  # VonB_R2   = 1 - (sum((weight-VonB_sim)^2)/sum((weight-mean(weight))^2))
  # k = length(VonB_opt$par)
  # VonB_AIC  = AIC(obs = weight, sim = VonB_sim, k = k)
  
  model_parameters <- c(i,
                        logi_opt$par,
                        logi_opt$value,
                        logi_R2,
                        logi_AIC,
                        
                        gomp_opt$par,
                        gomp_opt$value,
                        gomp_R2,
                        gomp_AIC,
                        
                        quad_opt$par,
                        quad_opt$value,
                        quad_R2,
                        quad_AIC
                        
                        # ,VonB_opt$par
                        # ,VonB_opt$value
                        # ,VonB_R2
                        # ,VonB_AIC
                        )
  
  fit_parameters <- rbind(fit_parameters, model_parameters)
  
  ## PLOT MODELS AND DATA
  png(filename = paste0(outpath,i,'_Fit_Models.png'), width = 850, height = 850, res = 120)
  par(mar = c(3.5,3.5,1,1))
  plot_serie1 <- plot(serie1$Days,serie1$Weigth, ylim = c(0,5250), xlim = c(0, 540),
                      xlab = '', ylab = '')
  lines(serie1$Days, logi_sim, col = 'blue')
  lines(serie1$Days, gomp_sim, col = 'red')
  lines(serie1$Days, quad_sim, col = 'green')
  # lines(serie1$Days, VonB_sim, col = 'orange')
  
  mtext('Time in Days', side = 1, line = 2)
  mtext('Weight (gr)' , side = 2, line = 2)
    
  legend('topleft',legend = c('Logistic Model'
                              ,'Gompertz Model'
                              ,'Quadratic Model'
                              # ,'VB_model'
                              ), bty = 'n',
         lty = c(1,1,1), col = c('blue'
                                 ,'red'
                                 ,'green'
                                 # ,'orange'
                                 ), cex = 0.75)
  mtext(text = paste('Center:', i), side = 3, -2)
  dev.off()
  
  ## Save OBS vs PREDICT
  obs_pred <- cbind(weight, logi_sim, gomp_sim, quad_sim)
  write.table(file = paste0(outpath,i,'_obs_predict.csv'), x = obs_pred, row.names = F, sep = ',')
}
fit_parameters <- as.data.frame(fit_parameters)
names(fit_parameters) <- c('Center',
                           'logi_Wmax',
                           'logi_a',
                           'logi_k',
                           'logi_cost',
                           'logi_R2',
                           'logi_AIC',
                           
                           'gomp_W0',
                           'gomp_m0',
                           'gomp_k',
                           'gomp_cost',
                           'gomp_R2',
                           'gomp_AIC',
                           
                           'quad_a',
                           'quad_b',
                           'quad_c',
                           'quad_cost',
                           'quad_R2',
                           'quad_AIC'
                           
                           # ,'VonB_Winf'
                           # ,'VonB_t0'
                           # ,'VonB_k'
                           # ,'VonB_cost'
                           # ,'VonB_R2'
                           # ,'VonB_AIC'
                           )

write.table(file = paste0(outpath,'model_fit_parameters.csv'), x = fit_parameters, row.names = F, sep = ',')
