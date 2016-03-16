#Eulerova metoda
#sigma je diagonální, neuvažují se kovariance mezi podkladovými aktivy
n<-771        #počet pozorování z hlediska času
k<-7          #počet podkladových akcií
N<-500        #počet simulací
e_n<-rep(1,times=n)   #jedničový vektor
e_k<-rep(1,times=k)   #jedničový vektor
#r0<-c(t(data[1,])) #posledni pozorovane hodnoty cen akcií
h<-0.001      #časový krok
w_r<-rnorm(N*k,0,h)     #vektor přírůstků Wienerova procesu
W_r<-matrix(w,N,k)      #matice přírůstků Wienerova procesu
i<-0          #index
r0<-mu_r*e_k #pocatecni iterace, zatim jen pomocna
ri<-r0        #i-tá iterace
r<-r0        #matice iterací
kappa<-1  #rychlost reverze
mu_r<-0.02 #hodnota rovnovažné polohy
sigma_r<-0.02 #volatilita v Ornstein–Uhlenbeckově procesu
for (i in 1:N){        #cyklus iterací numerické metody
  #dXt = k(μ − Xt) dt + sdWt
  dr<-c(kappa*(mu_r*e_k-ri)*h)+sigma_r*W_r[i,]   #násobení po složkách *, maticové násobení %*%
  ri<-ri+dr
  r<-cbind(r,ri)
}

t<-seq(from=0, by=h, length=N+1)
matplot (t, t(r), type = "l", lty = 1,  #ylim=c(100, 150000), 
         xlab = "t", ylab = "r(t)", main = "rate of returns")

plot (t, r[1, ], type = "l", lty = 1, xlab = "t", ylab = "r(t)", main = "CEZ")
plot (t, r[2, ], type = "l", lty = 1, xlab = "t", ylab = "r(t)", main = "O2")
plot (t, r[3, ], type = "l", lty = 1, xlab = "t", ylab = "r(t)", main = "KB")
plot (t, r[4, ], type = "l", lty = 1, xlab = "t", ylab = "r(t)", main = "PM")
plot (t, r[5, ], type = "l", lty = 1, xlab = "t", ylab = "r(t)", main = "Pegas")
plot (t, r[6, ], type = "l", lty = 1, xlab = "t", ylab = "r(t)", main = "CT")
plot (t, r[7, ], type = "l", lty = 1, xlab = "t", ylab = "r(t)", main = "NWR")
