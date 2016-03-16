#Eulerova metoda
#sigma je diagonální, neuvažují se kovariance mezi podkladovými aktivy
n<-771        #počet pozorování z hlediska času
k<-7          #počet podkladových akcií
N<-500        #počet simulací
e_n<-rep(1,times=n)   #jedničový vektor
e_k<-rep(1,times=k)   #jedničový vektor
#r0<-c(t(data[1,])) #posledni pozorovane hodnoty cen akcií
h<-1      #časový krok
w_r<-rnorm(N,0,sqrt(h))     #vektor přírůstků Wienerova procesu
i<-0          #index
kappa<-1  #rychlost reverze
mu_r<-0.02 #hodnota rovnovažné polohy
sigma_r<-0.02 #volatilita v Ornstein–Uhlenbeckově procesu
r0<-mu_r #pocatecni iterace, zatim jen pomocna
ri<-r0        #i-tá iterace
r<-r0        #vektor iterací
for (i in 1:N){        #cyklus iterací numerické metody
  #dXt = k(μ − Xt) dt + sdWt
  dr<-kappa*(mu_r-ri)*h+sigma_r*w_r[i] 
  ri<-ri+dr
  r<-cbind(r,ri)
}

t<-seq(from=0, by=h, length=N+1)
plot (t, r, type = "l", lty = 1, xlab = "t", ylab = "r(t)", main = "return of market")