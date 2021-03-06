#Milsteinova metoda
#sigma nen� diagon�ln�, uva�uj� se kovariance mezi podkladov�mi aktivy
n<-771        #po�et pozorov�n� z hlediska �asu
k<-7          #po�et podkladov�ch akci�
q<-1          #koeficient kter� ovliv�uje d�lku kroku
N<-500*q        #po�et simulac�
sigma<-rcov     #vyp�e diagon�lu matice do vektoru a n�sledn� vytvo�� diagon�ln� matici se slo�kami tohoto vektoru na diagon�le
e_n<-rep(1,times=n)   #jedni�ov� vektor
e_k<-rep(1,times=k)   #jedni�ov� vektor
x0<-c(t(data[n,])) #posledni pozorovane hodnoty cen akci�
h<-1/q#1#0.001      #�asov� krok
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
NN<-20 #po�et trajektori�
ax<-x
axm<-xm
#ax<-array(0:0, dim=c(k,N+1,NN))
for (j in 1:NN-1){
 w<-rnorm(N*k,0,sqrt(h))     #vektor p��r�stk� Wienerova procesu
 W<-matrix(w,N,k)      #matice p��r�stk� Wienerova procesu
 i<-0          #index
 xi<-x0        #i-t� iterace
 x<-x0        #matice iterac�
 #Ornstein�Uhlenbeck process
 w_r<-rnorm(N,0,sqrt(h))     #vektor p��r�stk� Wienerova procesu
 r0<-mu_r #pocatecni iterace, zatim jen pomocna
 ri<-r0        #i-t� iterace
 r<-r0        #vektor iterac�
 #simulace
 vahy<-xi/e_k%*%xi
 xmi<-xi
 xm<-x
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
 axm<-array(c(axm,xm),c(k,N+1,j+1))
                 }
 #ax<-array(c(x,xm),c(k,N+1,2))

matplot (t, ax[1, , ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ")
matplot (t, axm[1, , ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ")

aaxm<-mean(axm[1,1, ])
for (ii in 1:N){aaxm<-cbind(aaxm,mean(axm[1,ii, ]))}

matplot (t, aaxm[1, ], type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ")

matplot (t, cbind(axm[1, , ],aaxm[1, ]), type = "l", lty = 1, xlab = "t", ylab = "P(t)", main = "CEZ", col=c(rep(1,times=NN),2))
