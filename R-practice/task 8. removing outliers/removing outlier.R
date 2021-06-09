import <- read.csv("C:\\Users\\sharm\\Desktop\\ws16\\catalyst_temps.csv", header=T,sep=";",dec=",",fill=T)
head(import)
a<-plot(import$OutletTemp~import$InletTemp)
text(import$OutletTemp~import$InletTemp,labels=1:nrow(import),pos=2)
import<-import[-c(105,190,249),]
a<-plot(import$OutletTemp~import$InletTemp)
text(import$OutletTemp~import$InletTemp,labels=1:nrow(import),pos=2)