#Eulerova metoda
#sigma není diagonální, uvažují se kovariance mezi podkladovými aktivy
n<-771
k<-7
N<-500
sigma<-rcov
e_n<-rep(1,times=n)
e_k<-rep(1,times=k)
y0<-c(t(1/n*e_n%*%data))
h<-0.001
i<-0
yi<-y0
y<-y0
for (i in 1:N){
  dy<-c(h*((sigma*sigma)%*%e_k+yi*(sigma%*%yi)))
  yi<-yi+dy
  y<-cbind(y,yi)
}

t<-seq(from=0, by=h, length=N+1)
matplot (t, t(y), type = "l", lty = 1,  ylim=c(100, 150000), xlab = "t", ylab = "E(P)", main = "expected value")

plot (t, y[1, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "CEZ")
plot (t, y[2, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "O2")
plot (t, y[3, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "KB")
plot (t, y[4, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "PM")
plot (t, y[5, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "Pegas")
plot (t, y[6, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "CT")
plot (t, y[7, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "NWR")

