t<-seq(from=0, by=h, length=N+1)
matplot (t, t(x), type = "l", lty = 1,  ylim=c(0, 15000), 
         xlab = "t", ylab = "P(t)", main = "price of asset")

plot (t, r, type = "l", lty = 1, xlab = "t", ylab = "r(t)", main = "return of market")

plot (t, x[1, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ")
plot (t, x[2, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "O2")
plot (t, x[3, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "KB")
plot (t, x[4, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "PM")
plot (t, x[5, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "Pegas")
plot (t, x[6, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CT")
plot (t, x[7, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "NWR")

matplot (t, t(vahy), type = "l", lty = 1, #xlim=c(0, 300), 
         xlab = "t", ylab = "X(t)", main = "weight of asset")


tt<-seq(from=1, by=1, length=n)
matplot (tt, data, type = "l", lty = 1, # ylim=c(0, 15000), 
         xlab = "t", ylab = "P(t)", main = "price of asset")



matplot (c(t,tt+N),t(cbind(t(data),x)), type = "l", lty = 1, # ylim=c(0, 15000), 
         xlab = "t", ylab = "P(t)", main = "price of asset")

plot (c(t,tt+N), cbind(t(data),x)[1, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ")
plot (c(t,tt+N), cbind(t(data),x)[2, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "O2")
plot (c(t,tt+N), cbind(t(data),x)[3, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "KB")
plot (c(t,tt+N), cbind(t(data),x)[4, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "PM")
plot (c(t,tt+N), cbind(t(data),x)[5, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "Pegas")
plot (c(t,tt+N), cbind(t(data),x)[6, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CT")
plot (c(t,tt+N), cbind(t(data),x)[7, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "NWR")

