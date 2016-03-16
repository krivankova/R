m<-matrix(t(y1),N+1)%*%rep(1,times=k)
p<-matrix(t(y1),N+1)
w<-matrix(t(y1),N+1)/(m%*%t(rep(1,times=k)))
