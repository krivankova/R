date<- scan(file="U:/R/data/date.csv", sep=";", what=date(), dec=",")
cez <- scan(file="U:/R/data/cez.csv", sep=";", what=numeric(), dec=",")
o2 <- scan(file="U:/R/data/o2.csv", sep=";", what=numeric(), dec=",")
kb <- scan(file="U:/R/data/KB.csv", sep=";", what=numeric(), dec=",")
pm <- scan(file="U:/R/data/PM.csv", sep=";", what=numeric(), dec=",")
pegas <- scan(file="U:/R/data/Pegas.csv", sep=";", what=numeric(), dec=",")
cetv <- scan(file="U:/R/data/cetv.csv", sep=";", what=numeric(), dec=",")
nwr <- scan(file="U:/R/data/NWR.csv", sep=";", what=numeric(), dec=",")


data<-cbind(cez,o2,kb,pm,pegas,cetv,nwr)

cov<-cov(data)
#save(cov, file="cov.txt") #pokus o ulozeni kovariancni matice

P1<-data[1:n-1,]
P0<-data[2:n,]
r<-(P1-P0)/P0
rcov<-cov(r)
