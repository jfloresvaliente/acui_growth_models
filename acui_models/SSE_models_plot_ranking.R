dirpath <- 'F:/COLLABORATORS/Marcelo/'

# FUNCION PARA GRAFICAR LOS RANKINGS --------------------------------------

rankig_calculation <- function(dat, file_name, ylim){
  models  <- levels(factor(dat$model))
  ranking <- levels(factor(dat$rankingWI))
  
  evals <- NULL
  for(i in models){
    for(j in ranking){
      
      best_rank <- dim(subset(dat, dat$model == i & dat$ranking == j))[1]
      
      evaluacion <- c(i,j,best_rank)
      evals <- rbind(evals, evaluacion)
    }
  }
  colnames(evals) <- c('model', 'ranking', 'frecuencia')
  evals <- data.frame(evals)
  
  # evals$model       <- as.character(evals$model)
  evals$ranking     <- as.numeric(as.character(evals$ranking))
  evals$frecuencia  <- as.numeric(as.character(evals$frecuencia))
  
  
  BEST <- NULL
  rank_names <- c('Best ranked', 'Second best', 'Third best', 'Fourth best')
  
  
  png(filename = paste0(file_name,'.png'), height = 850, width = 1250, res = 120)
  par(mfrow=c(2,2)
      ,oma = c(2, 2, 3, 0.5) # two rows of text at the outer left and bottom margin
      ,mar = c(3, 2, 1.5, 0) # space for one row of text at ticks and to separate plots
      ,mgp = c(2, .5, 0)     # axis label at 2 rows distance, tick labels at 1 row
      # ,xpd = NA)
  )
  
  for(i in 1:4){
    best <- subset(evals, evals$ranking == i)
    BEST <- rbind(BEST, best)
    
    barplot(best$frecuencia, names.arg = models, ylab = 'Frecuencia', ylim = ylim, cex.names=.75)
    mtext(text = rank_names[i], side = 3, line = -1, adj = .5, font = 2)
  }
  dev.off()
  write.table(x = BEST, file = paste0(file_name,'.csv'), sep = ';', row.names = F)
}


# GRAFICA DE CADA FACTOR POR SEPARADO -------------------------------------

dat <- read.table(paste0(dirpath,'SSE_models_out_ranking.csv'), header = T, sep = '')
dat_names <- names(dat)[6:10]

for (i in 1:length(dat_names)) {

  index_atributo  <- which (names(dat) == dat_names[i])
  fac_atributo <- levels(factor(dat[,index_atributo]))

  for (j in fac_atributo) {

    sub_dat <- subset(dat, dat[,index_atributo] == j)
    file_name <- paste0(dirpath, colnames(dat)[index_atributo], '_',j)
    rankig_calculation(dat = sub_dat, file_name = file_name, ylim = c(0,31))
  }
}


# GRAFICA DE CONBINACIONES DE FACTORES ------------------------------------

dat <- read.table(paste0(dirpath,'SSE_models_out_ranking.csv'), header = T, sep = '')

atr <- c(6,10)
atributo1 <- names(dat)[atr[1]]
atributo2 <- names(dat)[atr[2]]

fac_atributo1 <- levels(factor(dat[,atributo1]))
fac_atributo2 <- levels(factor(dat[,atributo2]))

for(i in fac_atributo1){
  for(j in fac_atributo2){
    sub_dat <- subset(dat, dat[,atr[1]] == i & dat[,atr[2]] == j)
    file_name <- paste0(dirpath, atributo1,'_' ,i,'_', atributo2, '_',j)
    rankig_calculation(dat = sub_dat, file_name = file_name, ylim = c(0,31))
  }
}

