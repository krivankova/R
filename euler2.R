#Eulerova metoda
#sigma není diagonální, uvažují se kovariance mezi podkladovými aktivy
n<-771        #poèet pozorování z hlediska èasu
k<-7          #poèet podkladových akcií
N<-500        #poèet simulací
sigma<-rcov     #vypíše diagonálu matice do vektoru a následnì vytvoøí diagonální matici se složkami tohoto vektoru na diagonále
e_n<-rep(1,times=n)   #jednièový vektor
e_k<-rep(1,times=k)   #jednièový vektor
x0<-c(t(data[1,])) #posledni pozorovane hodnoty cen akcií
h<-0.001      #èasový krok
w<-rnorm(n*k,0,sqrt(h))     #vektor pøírùstkù Wienerova procesu
W<-matrix(w,n,k)      #matice pøírùstkù Wienerova procesu
i<-0          #index
xi<-x0        #i-tá iterace
x<-x0        #matice iterací
for (i in 1:N){        #cyklus iterací numerické metody
  dx<-c(h*(xi*(sigma%*%xi))+xi*(vol%*%W[i,]))   #násobení po složkách *
  xi<-xi+dx
  x<-cbind(x,xi)
}

t<-seq(from=0, by=h, length=N+1)
matplot (t, t(x), type = "l", lty = 1,  ylim=c(100, 150000), xlab = "t", ylab = "P(t)", main = "price of asset")

plot (t, x[1, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ")
plot (t, x[2, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "O2")
plot (t, x[3, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "KB")
plot (t, x[4, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "PM")
plot (t, x[5, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "Pegas")
plot (t, x[6, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CT")
plot (t, x[7, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "NWR")

