#Eulerova metoda
#sigma je diagon�ln�, neuva�uj� se kovariance mezi podkladov�mi aktivy
n<-771        #po�et pozorov�n� z hlediska �asu
k<-7          #po�et podkladov�ch akci�
N<-500        #po�et simulac�
sigma<-diag(diag(rcov))     #vyp�e diagon�lu matice do vektoru a n�sledn� vytvo�� diagon�ln� matici se slo�kami tohoto vektoru na diagon�le
e_n<-rep(1,times=n)   #jedni�ov� vektor
e_k<-rep(1,times=k)   #jedni�ov� vektor
y0<-c(t(1/n*e_n%*%data)) #vektor pr�m�r�, maticov� n�soben� %*%, transpozice vektoru t()
h<-0.001      #�asov� krok
i<-0          #index
yi<-y0        #i-t� iterace
y1<-y0        #matice iterac�
for (i in 1:N){        #cyklus iterac� numerick� metody
  dy<-c(h*((sigma*sigma)%*%e_k+yi*(sigma%*%yi)))   #n�soben� po slo�k�ch *
  yi<-yi+dy
  y1<-cbind(y1,yi)
}

t<-seq(from=0, by=h, length=N+1)
matplot (t, t(y1), type = "l", lty = 1,  ylim=c(100, 150000), xlab = "t", ylab = "E(P)", main = "expected value")

plot (t, y1[1, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "CEZ")
plot (t, y1[2, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "O2")
plot (t, y1[3, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "KB")
plot (t, y1[4, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "PM")
plot (t, y1[5, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "Pegas")
plot (t, y1[6, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "CT")
plot (t, y1[7, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "NWR")

