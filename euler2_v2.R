#Eulerova metoda
#sigma není diagonální, uvažují se kovariance mezi podkladovými aktivy
n<-771        #poèet pozorování z hlediska èasu
k<-7          #poèet podkladových akcií
N<-300        #poèet simulací
sigma<-rcov     #vypíše diagonálu matice do vektoru a následnì vytvoøí diagonální matici se složkami tohoto vektoru na diagonále
e_n<-rep(1,times=n)   #jednièový vektor
e_k<-rep(1,times=k)   #jednièový vektor
x0<-c(t(data[1,])) #posledni pozorovane hodnoty cen akcií
h<-1#0.001      #èasový krok
w<-rnorm(N*k,0,h)     #vektor pøírùstkù Wienerova procesu
W<-matrix(w,N,k)      #matice pøírùstkù Wienerova procesu
i<-0          #index
xi<-x0        #i-tá iterace
x<-x0        #matice iterací
#Ornstein–Uhlenbeck process
w_r<-rnorm(N,0,h)     #vektor pøírùstkù Wienerova procesu
kappa<-1  #rychlost reverze
mu_r<-0.02 #hodnota rovnovažné polohy
sigma_r<-0.02 #volatilita v Ornstein–Uhlenbeckovì procesu
r0<-mu_r #pocatecni iterace, zatim jen pomocna
ri<-r0        #i-tá iterace
r<-r0        #vektor iterací
#simulace
vahy<-xi/e_k%*%xi
for (i in 1:N){        #cyklus iterací numerické metody
  dx<-c(h*(xi*(xi/e_k%*%xi+ri))+xi*(vol%*%W[i,]))   #násobení po složkách * #(xi/e_k%*%xi) váhy tržního portfolia #kontrola e_k%*%(xi/e_k%*%xi)
  xi<-xi+dx
  x<-cbind(x,xi)
  dr<-kappa*(mu_r-ri)*h+sigma_r*w_r[i] 
  ri<-ri+dr
  r<-cbind(r,ri)
  vahy<-cbind(vahy,xi/e_k%*%xi)
}

t<-seq(from=0, by=h, length=N+1)
matplot (t, t(x), type = "l", lty = 1,  ylim=c(100, 150000), 
         xlab = "t", ylab = "P(t)", main = "price of asset")

plot (t, r, type = "l", lty = 1, xlab = "t", ylab = "r(t)", main = "rate of return")

plot (t, x[1, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ")
plot (t, x[2, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "O2")
plot (t, x[3, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "KB")
plot (t, x[4, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "PM")
plot (t, x[5, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "Pegas")
plot (t, x[6, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CT")
plot (t, x[7, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "NWR")

matplot (t, t(vahy), type = "l", lty = 1, xlim=c(0, 5), xlab = "t", ylab = "X(t)", main = "weight of asset")

