date<- scan(file="D:/Dizertace/R/data/date.csv", sep=";", what=date(), dec=",")
cez <- scan(file="D:/Dizertace/R/data/cez.csv", sep=";", what=numeric(), dec=",")
o2 <- scan(file="D:/Dizertace/R/data/o2.csv", sep=";", what=numeric(), dec=",")
kb <- scan(file="D:/Dizertace/R/data/KB.csv", sep=";", what=numeric(), dec=",")
pm <- scan(file="D:/Dizertace/R/data/PM.csv", sep=";", what=numeric(), dec=",")
pegas <- scan(file="D:/Dizertace/R/data/Pegas.csv", sep=";", what=numeric(), dec=",")
cetv <- scan(file="D:/Dizertace/R/data/cetv.csv", sep=";", what=numeric(), dec=",")
nwr <- scan(file="D:/Dizertace/R/data/NWR.csv", sep=";", what=numeric(), dec=",")


#data<-cbind(cez,o2,kb,pm,pegas,cetv,nwr)
data<-cbind(rev(cez),rev(o2),rev(kb),rev(pm),rev(pegas),rev(cetv),rev(nwr))

cov<-cov(data)
#save(cov, file="cov.txt") #pokus o ulozeni kovariancni matice

P1<-data[1:n-1,]
P0<-data[2:n,]
rr<-(P1-P0)/P0    #relativní vınosnost spoèítaná z ceny
rcov<-cov(rr)     #kovarianèní matice vınosností

#rmean<-mean(rr[,1])
#for (i in 2:7){rmean<-cbind(rmean,mean(rr[,i]))}

#rmeanall<-mean(rmean)