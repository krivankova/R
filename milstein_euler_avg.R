#Milsteinova metoda
#sigma nen� diagon�ln�, uva�uj� se kovariance mezi podkladov�mi aktivy
n<-771        #po�et pozorov�n� z hlediska �asu
k<-7          #po�et podkladov�ch akci�
N<-500        #po�et simulac�
sigma<-rcov     #vyp�e diagon�lu matice do vektoru a n�sledn� vytvo�� diagon�ln� matici se slo�kami tohoto vektoru na diagon�le
e_n<-rep(1,times=n)   #jedni�ov� vektor
e_k<-rep(1,times=k)   #jedni�ov� vektor
x0<-c(t(data[n,])) #posledni pozorovane hodnoty cen akci�
h<-1#0.001      #�asov� krok
w<-rnorm(N*k,0,sqrt(h))     #vektor p��r�stk� Wienerova procesu
W<-matrix(w,N,k)      #matice p��r�stk� Wienerova procesu
i<-0          #index
xi<-x0        #i-t� iterace
x<-x0        #matice iterac�
#Ornstein�Uhlenbeck process
w_r<-rnorm(N,0,sqrt(h))     #vektor p��r�stk� Wienerova procesu
kappa<-1  #rychlost reverze
mu_r<-0.02/250 #hodnota rovnova�n� polohy
sigma_r<-0.02/250 #volatilita v Ornstein�Uhlenbeckov� procesu
r0<-mu_r #pocatecni iterace, zatim jen pomocna
ri<-r0        #i-t� iterace
r<-r0        #vektor iterac�
#v�po�et volatility z kovarian�n� matice
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
#po��te�n� simulace
for (i in 1:N){        #cyklus iterac� numerick� metody
  dm<-0.5*xi*(vol%*%e_k*vol%*%e_k)*W[i,]
  dx<-c(h*(xi*(xi/e_k%*%xi*ri))+xi*(vol%*%W[i,]))   #n�soben� po slo�k�ch * #(xi/e_k%*%xi) v�hy tr�n�ho portfolia #kontrola e_k%*%(xi/e_k%*%xi)
  xi<-xi+dx
  xmi<-xmi+dx+c(dm)
  x<-cbind(x,xi)
  xm<-cbind(xm,xmi)
  dr<-kappa*(mu_r-ri)*h+sigma_r*w_r[i] 
  ri<-ri+dr
  r<-cbind(r,ri)
  vahy<-cbind(vahy,xi/e_k%*%xi)
}
#ostatn� simulace
NN<-2 #po�et trajektori�
ax<-x
for (j in 1:NN-1){
for (i in 1:N){        #cyklus iterac� numerick� metody
  dm<-0.5*xi*(vol%*%e_k*vol%*%e_k)*W[i,]
  dx<-c(h*(xi*(xi/e_k%*%xi*ri))+xi*(vol%*%W[i,]))   #n�soben� po slo�k�ch * #(xi/e_k%*%xi) v�hy tr�n�ho portfolia #kontrola e_k%*%(xi/e_k%*%xi)
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
              }
#ax<-array(c(x,xm),c(k,N+1,2))


t<-seq(from=0, by=h, length=N+1)
matplot (t, t(x), type = "l", lty = 1,  ylim=c(0, 15000), 
         xlab = "t", ylab = "P(t)", main = "price of asset")

plot (t, r, type = "l", lty = 1, xlab = "t", ylab = "r(t)", main = "return of market")

matplot (t, cbind(x[1, ], xm[1, ]), type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ")
legend(t[420],(mean(xm[1,])), c("Euler", "Milstein"), col=c(1:2), lty=1,  box.lty=0)
matplot (t, cbind(x[2, ], xm[2, ]), type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "O2")
legend(t[420],(mean(xm[2,])), c("Euler", "Milstein"), col=c(1:2), lty=1,  box.lty=0)
matplot (t, cbind(x[3, ], xm[3, ]), type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "KB")
legend(t[420],(mean(xm[3,])), c("Euler", "Milstein"), col=c(1:2), lty=1,  box.lty=0)
matplot (t, cbind(x[4, ], xm[4, ]), type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "PM")
legend(t[420],(mean(xm[4,])), c("Euler", "Milstein"), col=c(1:2), lty=1,  box.lty=0)
matplot (t, cbind(x[5, ], xm[5, ]), type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "Pegas")
legend(t[420],(mean(xm[5,])), c("Euler", "Milstein"), col=c(1:2), lty=1,  box.lty=0)
matplot (t, cbind(x[6, ], xm[6, ]), type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CT")
legend(t[420],(mean(xm[6,])), c("Euler", "Milstein"), col=c(1:2), lty=1,  box.lty=0)
matplot (t, cbind(x[7, ], xm[7, ]), type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "NWR")
legend(t[420],(mean(xm[7,])), c("Euler", "Milstein"), col=c(1:2), lty=1,  box.lty=0)

matplot (t, t(vahy), type = "l", lty = 1, #xlim=c(0, 300), 
         xlab = "t", ylab = "X(t)", main = "weight of asset")
