c<-atan(y0[1]/(rcov[1,1]^0.5))

plot (t, y[1, ], type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "CEZ")
#par(new=TRUE)
plot (t, rcov[1,1]^0.5*tan(rcov[1,1]^(3/2)*t+c), col='red', add=TRUE, type = "l", lty = 1, xlab = "t", ylab = "E(P)", main = "CEZ")

#srovnání grafu pro numerickou metodu a analytickou metodu (s kovariancemi i bez)
graf_cez<-cbind(y[1,],(rcov[1,1]^0.5)*tan(rcov[1,1]^(3/2)*t+c))
matplot(graf_cez, type = "l", lty = 1,   xlab = "t", ylab = "E(P)", main = "CEZ")
graf_cez<-cbind(y1[1,],(rcov[1,1]^0.5)*tan(rcov[1,1]^(3/2)*t+c))
matplot(t,graf_cez, type = "l", lty = 1,   xlab = "t", ylab = "E(P)", main = "CEZ")

nazvy<-c("CEZ","O2","KB","PM","Pegas","CT","NWR")

for (i in 1:7){
  ci<-atan(y0[i]/(rcov[i,i]^0.5))  
  graf_cez<-cbind(y[i,],(rcov[i,i]^0.5)*tan(rcov[i,i]^(3/2)*t+ci))
  matplot(t,graf_cez, type = "l", lty = 1,   xlab = "t", ylab = "E(P)", main = nazvy[i])
}

for (i in 1:7){
  ci<-atan(y0[i]/(rcov[i,i]^0.5))  
  graf_cez<-cbind(y1[i,],(rcov[i,i]^0.5)*tan(rcov[i,i]^(3/2)*t+ci))
  matplot(t,graf_cez, type = "l", lty = 1,   xlab = "t", ylab = "E(P)", main = nazvy[i])
}

for (i in 1:7){
  ci<-atan(x0[i]/(rcov[i,i]^0.5))  
  graf_cez<-cbind(x[i,],(rcov[i,i]^0.5)*tan(rcov[i,i]^(3/2)*t+ci))
  matplot(t,graf_cez, type = "l", lty = 1,   xlab = "t", ylab = "P(t)", main = nazvy[i])
}

for (i in 1:7){
  ci<-atan(x0[i]/(rcov[i,i]^0.5))  
  graf_cez<-cbind(x1[i,],(rcov[i,i]^0.5)*tan(rcov[i,i]^(3/2)*t+ci))
  matplot(t,graf_cez, type = "l", lty = 1,   xlab = "t", ylab = "P(t)", main = nazvy[i])
}

i=7
ci<-atan(x0[i]/(rcov[i,i]^0.5))  
graf<-cbind(y[i,],y1[i,],x[i,],x1[i,],(rcov[i,i]^0.5)*tan(rcov[i,i]^(3/2)*t+ci))
matplot(t,graf, type = "l", lty = 1,   xlab = "t", ylab = "P(t)", main = nazvy[i])
