#Eulerova metoda
#sigma je diagon�ln�, neuva�uj� se kovariance mezi podkladov�mi aktivy
n<-771        #po�et pozorov�n� z hlediska �asu
k<-7          #po�et podkladov�ch akci�
N<-500        #po�et simulac�
sigma<-diag(diag(rcov))     #vyp�e diagon�lu matice do vektoru a n�sledn� vytvo�� diagon�ln� matici se slo�kami tohoto vektoru na diagon�le
e_n<-rep(1,times=n)   #jedni�ov� vektor
e_k<-rep(1,times=k)   #jedni�ov� vektor
x0<-c(t(data[1,])) #posledni pozorovane hodnoty cen akci�
h<-0.001      #�asov� krok
w<-rnorm(N*k,0,h)     #vektor p��r�stk� Wienerova procesu
W<-matrix(w,N,k)      #matice p��r�stk� Wienerova procesu
i<-0          #index
xi<-x0        #i-t� iterace
x1<-x0        #matice iterac�
#v�po�et volatility z kovarian�n� matice
C<-sigma
l<-eigen(C)$values
P<-eigen(C)$vectors
L<-diag(l)
sl<-sqrt(l) 
sL<-diag(sl)
vol<-P%*%sL
#simulace
for (i in 1:N){        #cyklus iterac� numerick� metody
  dx<-c(h*(xi*(sigma%*%xi))+xi*(vol%*%W[i,]))   #n�soben� po slo�k�ch *
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

