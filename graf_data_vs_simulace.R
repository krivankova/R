#===================================================================================
#Data

tt<-seq(from=1, by=1, length=n)
matplot (tt, data, type = "l", lty = 1, # ylim=c(0, 15000), 
         xlab = "t", ylab = "P(t)", main = "price of asset")

plot (tt, data[,1], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ")
plot (tt, data[,2], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "O2")
plot (tt, data[,3], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "KB")
plot (tt, data[,4], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "PM")
plot (tt, data[,5], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "Pegas")
plot (tt, data[,6], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CT")
plot (tt, data[,7], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "NWR")

matplot (tt, data/(data%*%e_k%*%e_k), type = "l", lty = 1, #xlim=c(0, 300), 
         xlab = "t", ylab = "X(t)", main = "weight of asset")

#===================================================================================
#Simulace

t<-seq(from=0, by=h, length=N+1)
plot (t, r, type = "l", lty = 1, xlab = "t", ylab = "r(t)", yaxt="n", main = "return of market")
#axis(2, at=pretty(r), lab=pretty(r) * 100)
axis(2, at=seq(-0.0002,0.0003,by=.0002), labels=paste(100*seq(-0.0002,0.0003,by=.0002), "%") )

matplot (t, t(x), type = "l", lty = 1, # ylim=c(0, 15000), 
         xlab = "t", ylab = "P(t)", main = "price of asset")

plot (t, x[1, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ")
plot (t, x[2, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "O2")
plot (t, x[3, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "KB")
plot (t, x[4, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "PM")
plot (t, x[5, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "Pegas")
plot (t, x[6, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CT")
plot (t, x[7, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "NWR")

matplot (t, t(vahy), type = "l", lty = 1, #xlim=c(0, 300), 
         xlab = "t", ylab = "X(t)", main = "weight of asset")

#===================================================================================
#Data + Simulace


matplot (c(t,tt+N),t(cbind(t(data),x)), type = "l", lty = 1, # ylim=c(0, 15000), 
         xlab = "t", ylab = "P(t)", main = "price of asset")
abline (v = 771, lty = 2)

plot (c(t,tt+N), cbind(t(data),x)[1, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ")
abline (v = 771, lty = 2)
plot (c(t,tt+N), cbind(t(data),x)[2, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "O2")
abline (v = 771, lty = 2)
plot (c(t,tt+N), cbind(t(data),x)[3, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "KB")
abline (v = 771, lty = 2)
plot (c(t,tt+N), cbind(t(data),x)[4, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "PM")
abline (v = 771, lty = 2)
plot (c(t,tt+N), cbind(t(data),x)[5, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "Pegas")
abline (v = 771, lty = 2)
plot (c(t,tt+N), cbind(t(data),x)[6, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CT")
abline (v = 771, lty = 2)
plot (c(t,tt+N), cbind(t(data),x)[7, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "NWR")
abline (v = 771, lty = 2)

matplot (c(t,tt+N),t(cbind(t(data/(data%*%e_k%*%e_k)),vahy)), type = "l", lty = 1, #xlim=c(0, 300), 
         xlab = "t", ylab = "X(t)", main = "weight of asset")
abline (v = 771, lty = 2)