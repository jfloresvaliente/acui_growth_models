
dat <- read.table('centros_acui.csv', sep = ';', header = T)

centros <- as.factor(dat$Center)
centros <- levels(centros)

# fechas <- seq(as.Date('2010-01-01'), as.Date('2016-12-01'), by = 'month')
# fechas <- format(fechas, format = '%y-%m')
# 
# 
# # tiff('all_centers.tiff', width = 1250, height = 650, res = 120)
# par(mar = c(4, 3, 1, 1))
# plot(1:length(fechas), type = 'n', xlab = '', ylab = '', ylim = c(0,5200), axes = F)
# mtext(text = 'Dates', side = 1, cex = 0.5, line = 1.5)
# mtext(text = 'Weigth (g)', side = 2, cex = 0.5, line = 1.5)
# axis(side = 1, at = 1:length(fechas), labels = fechas, cex.axis = 0.5)
# axis(side = 2, cex.axis = 0.5)
# box()
# 
# 
# for(i in centros){
#   # print(i)
#   cent <- subset(dat, dat$Center == i)
#   spaw <- as.factor(cent$Spawning)
#   spaw <- levels(spaw)
#   
#   for(j in spaw){
#     # print(j)
#     cent_spaw <- subset(cent, cent$Spawning == j)
#     
#     date_in <- as.Date(paste0(cent_spaw$year[1],'-',cent_spaw$Month[1],'-1'))
#     date_in <- format(date_in, format = '%y-%m')
#     
#     date_on <- as.Date(paste0(cent_spaw$year[dim(cent_spaw)[1]],'-',cent_spaw$Month[dim(cent_spaw)[1]],'-1'))
#     date_on <- format(date_on, format = '%y-%m')
#     
#     growth <- cent_spaw$Weigth
#     poin <- seq(which(fechas == date_in), length.out = dim(cent_spaw)[1])
#     lines(poin, growth)
#     
#   }
# }

# dev.off()

# tiff('all_centers_boxplot.tiff', width = 1250, height = 650, res = 120)
# par(mar = c(4, 3, 1, 1))
# box_graph <- boxplot(Weigth~Center,data=dat, notch = F, axes = F, ylim = c(0,5200))
# axis(side = 1, at = 1:length(box_graph$names), labels = box_graph$names, cex.axis = 0.5, las = 2)
# axis(side = 2, cex.axis = 0.5)
# # mtext(text = 'Centers', side = 1, cex = 0.5, line = 1.5)
# mtext(text = 'Weigth (g)', side = 2, cex = 0.5, line = 1.5)
# box()
# dev.off()


new_table <- NULL
for(i in centros){
  # print(i)
  cent <- subset(dat, dat$Center == i)
  spaw <- as.factor(cent$Spawning)
  spaw <- levels(spaw)
  
  pre_table <- NULL
  for(j in spaw){
    # print(j)
    cent_spaw <- subset(cent, cent$Spawning == j)
    
    # date_in <- as.Date(paste0(cent_spaw$year[1],'-',cent_spaw$Month[1],'-1'))
    # date_in <- format(date_in, format = '%y-%m')
    # 
    # date_on <- as.Date(paste0(cent_spaw$year[dim(cent_spaw)[1]],'-',cent_spaw$Month[dim(cent_spaw)[1]],'-1'))
    # date_on <- format(date_on, format = '%y-%m')
    
    # growth <- cent_spaw$Weigth
    # poin <- seq(which(fechas == date_in), length.out = dim(cent_spaw)[1])
    # lines(poin, growth)
    Days <- seq(from = 0, by = 30, length.out = dim(cent_spaw)[1])
    centro_num <- paste0(i,'_',j)
    centro_num <- rep(centro_num, length(Days))
    cent_spaw <- cbind(cent_spaw,Days,centro_num)
    pre_table <- rbind(pre_table, cent_spaw)
  }
  new_table <- rbind(new_table,pre_table)
}

dim(new_table)
# new_table <- new_table[,c(10,2:9)]
write.table(x = new_table, file = 'centros_acui3.csv', sep = ',', row.names = F, col.names = names(new_table))
read.table('centros_acui3.csv', col.names = F, header = T)
