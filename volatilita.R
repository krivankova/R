#x <- scan(file="D:/konference/2014/data_cov.csv", sep=";", what=numeric(), dec=",")
#C<-matrix(x,7)
#C<-cov
C<-sigma
l<-eigen(C)$values
P<-eigen(C)$vectors
L<-diag(l)
#rozklad matice  P%*%L%*%t(P)
sl<-sqrt(l) 
sL<-diag(sl)
vol<-P%*%sL
