#Eulerova metoda
#sigma je diagonální, neuvažují se kovariance mezi podkladovými aktivy
n<-771        #poèet pozorování z hlediska èasu
k<-7          #poèet podkladových akcií
N<-500        #poèet simulací
sigma<-diag(diag(rcov))     #vypíše diagonálu matice do vektoru a následnì vytvoøí diagonální matici se složkami tohoto vektoru na diagonále
e_n<-rep(1,times=n)   #jednièový vektor
e_k<-rep(1,times=k)   #jednièový vektor
y0<-c(t(1/n*e_n%*%data)) #vektor prùmìrù, maticové násobení %*%, transpozice vektoru t()
h<-0.001      #èasový krok
i<-0          #index
yi<-y0        #i-tá iterace
y1<-y0        #matice iterací
for (i in 1:N){        #cyklus iterací numerické metody
  dy<-c(h*((sigma*sigma)%*%e_k+yi*(sigma%*%yi)))   #násobení po složkách *
  yi<-yi+dy
  y1<-cbind(y1,yi)
}

t<-seq(from=0, by=h, length=N+1)
matplot (t, t(y1), type = "l", lty = 1,  ylim=c(100, 150000), xlab = "t", ylab = "E(P)", main = "expected value")

plot (t, y1[1, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "CEZ")
plot (t, y1[2, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "O2")
plot (t, y1[3, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "KB")
plot (t, y1[4, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "PM")
plot (t, y1[5, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "Pegas")
plot (t, y1[6, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "CT")
plot (t, y1[7, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "NWR")

