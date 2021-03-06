#Eulerova metoda
#sigma nen� diagon�ln�, uva�uj� se kovariance mezi podkladov�mi aktivy
n<-771        #po�et pozorov�n� z hlediska �asu
k<-7          #po�et podkladov�ch akci�
N<-500        #po�et simulac�
sigma<-rcov     #vyp�e diagon�lu matice do vektoru a n�sledn� vytvo�� diagon�ln� matici se slo�kami tohoto vektoru na diagon�le
e_n<-rep(1,times=n)   #jedni�ov� vektor
e_k<-rep(1,times=k)   #jedni�ov� vektor
x0<-c(t(data[1,])) #posledni pozorovane hodnoty cen akci�
h<-0.001      #�asov� krok
w<-rnorm(n*k,0,sqrt(h))     #vektor p��r�stk� Wienerova procesu
W<-matrix(w,n,k)      #matice p��r�stk� Wienerova procesu
i<-0          #index
xi<-x0        #i-t� iterace
x<-x0        #matice iterac�
for (i in 1:N){        #cyklus iterac� numerick� metody
  dx<-c(h*(xi*(sigma%*%xi))+xi*(vol%*%W[i,]))   #n�soben� po slo�k�ch *
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

