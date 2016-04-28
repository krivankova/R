#===================================================================================
#Data

date <- rev(scan(file="U:/R/data/date.csv", sep=";", what=date(), dec=","))
date <- as.Date(date, "%d.%m.%Y") 
#date2 <- bizseq("2011-01-31", "2014-02-21",Calendar(holidaysANBIMA, weekdays=c("saturday", "sunday")))

matplot (date, data, type = "l", lty = 1, xaxt="n", # axes=FALSE, # ylim=c(0, 15000), 
         xlab = "t", ylab = "P(t)", main = "price of asset")
#axis(2, las=2)
axis.Date(side = 1, date)
#axis.Date(side = 1, date, format = "%d/%m/%Y", las = 2)
legend(date[650],10200, c("CEZ", "O2","KB", "PM", "Pegas", "CT", "NWR"), col=1:7, lty=1,  box.lty=0)

matplot (date, data[,3:4], type = "l", lty = 1, xaxt="n", col=c(3:4),# axes=FALSE, # ylim=c(0, 15000), 
         xlab = "t", ylab = "P(t)", main = "price of asset")
#axis(2, las=2)
axis.Date(side = 1, date)
#axis.Date(side = 1, date, format = "%d/%m/%Y", las = 2)
legend(date[650],min(c(data[,4])), c("KB", "PM"), col=c(3:4), lty=1,  box.lty=0)

matplot (date, data[,c(1,2,5,6,7)], type = "l", lty = 1, xaxt="n", col=c(1,2,5,6,7),# axes=FALSE, # ylim=c(0, 15000), 
         xlab = "t", ylab = "P(t)", main = "price of asset")
#axis(2, las=2)
axis.Date(side = 1, date)
#axis.Date(side = 1, date, format = "%d/%m/%Y", las = 2)
legend(date[650],max(c(data[,c(1,2,5,6,7)]))+30, c("CEZ", "O2", "Pegas", "CT", "NWR"), col=c(1,2,5,6,7), lty=1,  box.lty=0)


plot (date, data[,1], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ")
plot (date, data[,2], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "O2")
plot (date, data[,3], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "KB")
plot (date, data[,4], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "PM")
plot (date, data[,5], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "Pegas")
plot (date, data[,6], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CT")
plot (date, data[,7], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "NWR")

matplot (date, data/(data%*%e_k%*%e_k), type = "l", lty = 1, xaxt="n",#xlim=c(0, 300), 
         xlab = "t", ylab = "X(t)", main = "weight of asset")
axis.Date(side = 1, date)

#===================================================================================
#Simulace

dates <- bizseq("2014-02-22", "2016-02-23",Calendar(holidaysANBIMA, weekdays=c("saturday", "sunday")))

plot (dates, r, type = "l", lty = 1, xlab = "t", ylab = "r(t)", yaxt="n", main = "return of market")
#axis(2, at=pretty(r), lab=pretty(r) * 100)
axis(2, at=seq(-0.0002,0.0003,by=.0002), labels=paste(100*seq(-0.0002,0.0003,by=.0002), "%") )

matplot (dates, t(x), type = "l", lty = 1, xaxt="n", # ylim=c(0, 15000), 
         xlab = "t", ylab = "P(t)", main = "price of asset")
axis.Date(side = 1, dates)



plot (dates, x[1, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ")
plot (dates, x[2, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "O2")
plot (dates, x[3, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "KB")
plot (dates, x[4, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "PM")
plot (dates, x[5, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "Pegas")
plot (dates, x[6, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CT")
plot (dates, x[7, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "NWR")

matplot (dates, t(vahy), type = "l", lty = 1, xaxt="n", #xlim=c(0, 300), 
         xlab = "t", ylab = "X(t)", main = "weight of asset")
axis.Date(side = 1, dates)
#===================================================================================
#Data + Simulace

matplot (c(date,dates),t(cbind(t(data),x)), type = "l", lty = 1, xaxt="n", # ylim=c(0, 15000), 
         xlab = "t", ylab = "P(t)", main = "price of asset")
axis.Date(side = 1, dates)
abline (v = dates[1], lty = 2)

matplot (c(date,dates), t(cbind(t(data[,3:4]),x[3:4,])), type = "l", lty = 1, xaxt="n", col=c(3:4),# axes=FALSE, # ylim=c(0, 15000), 
         xlab = "t", ylab = "P(t)", main = "price of asset")
axis.Date(side = 1, date)
legend(dates[300],min(c(t(cbind(t(data[,4]),x[4,])))), c("KB", "PM"), col=c(3:4), lty=1,  box.lty=0)
abline (v = dates[1], lty = 2)

matplot (c(date,dates), t(cbind(t(data[,c(1,2,5,6,7)]),x[c(1,2,5,6,7),])), type = "l", lty = 1, xaxt="n", col=c(1,2,5,6,7),# axes=FALSE, # ylim=c(0, 15000), 
         xlab = "t", ylab = "P(t)", main = "price of asset")
axis.Date(side = 1, date)
legend(dates[300],max(c(data[,c(1,2,5,6,7)]))+30, c("CEZ", "O2", "Pegas", "CT", "NWR"), col=c(1,2,5,6,7), lty=1,  box.lty=0)
abline (v = dates[1], lty = 2)

plot (c(date,dates), cbind(t(data),x)[1, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ")
abline (v = dates[1], lty = 2)
plot (c(date,dates), cbind(t(data),x)[2, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "O2")
abline (v = dates[1], lty = 2)
plot (c(date,dates), cbind(t(data),x)[3, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "KB")
abline (v = dates[1], lty = 2)
plot (c(date,dates), cbind(t(data),x)[4, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "PM")
abline (v = dates[1], lty = 2)
plot (c(date,dates), cbind(t(data),x)[5, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "Pegas")
abline (v = dates[1], lty = 2)
plot (c(date,dates), cbind(t(data),x)[6, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CT")
abline (v = dates[1], lty = 2)
plot (c(date,dates), cbind(t(data),x)[7, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "NWR")
abline (v = dates[1], lty = 2)

matplot (c(date,dates),t(cbind(t(data/(data%*%e_k%*%e_k)),vahy)), type = "l", lty = 1, xaxt="n", #xlim=c(0, 300), 
         xlab = "t", ylab = "X(t)", main = "weight of asset")
axis.Date(side = 1, dates)
legend(dates[300],0.6, c("CEZ", "O2","KB", "PM", "Pegas", "CT", "NWR"), col=1:7, lty=1,  box.lty=0)
abline (v = dates[1], lty = 2)