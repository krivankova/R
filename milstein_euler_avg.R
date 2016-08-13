#Milsteinova metoda
#sigma není diagonální, uvažují se kovariance mezi podkladovými aktivy
n<-771        #poèet pozorování z hlediska èasu
k<-7          #poèet podkladových akcií
N<-500        #poèet simulací
sigma<-rcov     #vypíše diagonálu matice do vektoru a následnì vytvoøí diagonální matici se složkami tohoto vektoru na diagonále
e_n<-rep(1,times=n)   #jednièový vektor
e_k<-rep(1,times=k)   #jednièový vektor
x0<-c(t(data[n,])) #posledni pozorovane hodnoty cen akcií
h<-1#0.001      #èasový krok
w<-rnorm(N*k,0,sqrt(h))     #vektor pøírùstkù Wienerova procesu
W<-matrix(w,N,k)      #matice pøírùstkù Wienerova procesu
i<-0          #index
xi<-x0        #i-tá iterace
x<-x0        #matice iterací
#Ornstein–Uhlenbeck process
w_r<-rnorm(N,0,sqrt(h))     #vektor pøírùstkù Wienerova procesu
kappa<-1  #rychlost reverze
mu_r<-0.02/250 #hodnota rovnovažné polohy
sigma_r<-0.02/250 #volatilita v Ornstein–Uhlenbeckovì procesu
r0<-mu_r #pocatecni iterace, zatim jen pomocna
ri<-r0        #i-tá iterace
r<-r0        #vektor iterací
#výpoèet volatility z kovarianèní matice
C<-sigma
l<-eigen(C)$values
P<-eigen(C)$vectors
L<-diag(l)
sl<-sqrt(l) 
sL<-diag(sl)
vol<-P%*%sL
#simulace
vahy<-xi/e_k%*%xi
xmi<-xi
xm<-x
#poèáteèní simulace
for (i in 1:N){        #cyklus iterací numerické metody
  dm<-0.5*xi*(vol%*%e_k*vol%*%e_k)*W[i,]
  dx<-c(h*(xi*(xi/e_k%*%xi*ri))+xi*(vol%*%W[i,]))   #násobení po složkách * #(xi/e_k%*%xi) váhy tržního portfolia #kontrola e_k%*%(xi/e_k%*%xi)
  xi<-xi+dx
  xmi<-xmi+dx+c(dm)
  x<-cbind(x,xi)
  xm<-cbind(xm,xmi)
  dr<-kappa*(mu_r-ri)*h+sigma_r*w_r[i] 
  ri<-ri+dr
  r<-cbind(r,ri)
  vahy<-cbind(vahy,xi/e_k%*%xi)
}
#ostatní simulace
NN<-20 #poèet trajektorií
ax<-x
axm<-xm
#ax<-array(0:0, dim=c(k,N+1,NN))
for (j in 1:NN-1){
 w<-rnorm(N*k,0,sqrt(h))     #vektor pøírùstkù Wienerova procesu
 W<-matrix(w,N,k)      #matice pøírùstkù Wienerova procesu
 i<-0          #index
 xi<-x0        #i-tá iterace
 x<-x0        #matice iterací
 #Ornstein–Uhlenbeck process
 w_r<-rnorm(N,0,sqrt(h))     #vektor pøírùstkù Wienerova procesu
 r0<-mu_r #pocatecni iterace, zatim jen pomocna
 ri<-r0        #i-tá iterace
 r<-r0        #vektor iterací
 #simulace
 vahy<-xi/e_k%*%xi
 xmi<-xi
 xm<-x
   for (i in 1:N){        #cyklus iterací numerické metody
   dm<-0.5*xi*(vol%*%e_k*vol%*%e_k)*W[i,]
   dx<-c(h*(xi*(xi/e_k%*%xi*ri))+xi*(vol%*%W[i,]))   #násobení po složkách * #(xi/e_k%*%xi) váhy tržního portfolia #kontrola e_k%*%(xi/e_k%*%xi)
   xi<-xi+dx
   xmi<-xmi+dx+c(dm)
   x<-cbind(x,xi)
   xm<-cbind(xm,xmi)
   dr<-kappa*(mu_r-ri)*h+sigma_r*w_r[i] 
   ri<-ri+dr
   r<-cbind(r,ri)
   vahy<-cbind(vahy,xi/e_k%*%xi)
               }
 ax<-array(c(ax,x),c(k,N+1,j+1))
 axm<-array(c(axm,xm),c(k,N+1,j+1))
                 }
 #ax<-array(c(x,xm),c(k,N+1,2))

matplot (t, ax[1, , ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ")
matplot (t, axm[1, , ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ")

aaxm<-mean(axm[1,1, ])
for (ii in 1:N){aaxm<-cbind(aaxm,mean(axm[1,ii, ]))}

matplot (t, aaxm[1, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ")

matplot (t, cbind(axm[1, , ],aaxm[1, ]), type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ", col=c(rep(1,times=NN),2))
