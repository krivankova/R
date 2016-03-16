#Eulerova metoda
#sigma je diagonální, neuvažují se kovariance mezi podkladovými aktivy
n<-771        #poèet pozorování z hlediska èasu
k<-7          #poèet podkladových akcií
N<-500        #poèet simulací
sigma<-diag(diag(rcov))     #vypíše diagonálu matice do vektoru a následnì vytvoøí diagonální matici se složkami tohoto vektoru na diagonále
e_n<-rep(1,times=n)   #jednièový vektor
e_k<-rep(1,times=k)   #jednièový vektor
x0<-c(t(data[1,])) #posledni pozorovane hodnoty cen akcií
h<-0.001      #èasový krok
w<-rnorm(N*k,0,h)     #vektor pøírùstkù Wienerova procesu
W<-matrix(w,N,k)      #matice pøírùstkù Wienerova procesu
i<-0          #index
xi<-x0        #i-tá iterace
x1<-x0        #matice iterací
for (i in 1:N){        #cyklus iterací numerické metody
  dx<-c(h*(xi*(sigma%*%xi))+xi*(vol%*%W[i,]))   #násobení po složkách *
  xi<-xi+dx
  x1<-cbind(x1,xi)
}

t<-seq(from=0, by=h, length=N+1)
matplot (t, t(x1), type = "l", lty = 1,  ylim=c(100, 150000), xlab = "t", ylab = "P(t)", main = "price of asset")

plot (t, x1[1, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ")
plot (t, x1[2, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "O2")
plot (t, x1[3, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "KB")
plot (t, x1[4, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "PM")
plot (t, x1[5, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "Pegas")
plot (t, x1[6, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CT")
plot (t, x1[7, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "NWR")

