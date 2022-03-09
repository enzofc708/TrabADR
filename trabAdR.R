library("triangle")

arq_atl = read.csv("atlantic.csv")
arq_pac = read.csv("pacific.csv")

arq_atl$Date = as.Date(as.character(arq_atl$Date), "%Y%m%d")
arq_pac$Date = as.Date(as.character(arq_pac$Date), "%Y%m%d")


#Testando as distribui??es triangulares

h <- hist(as.integer(format(arq_atl$Date,format="%Y")), breaks = 175, main="Histograma Furacões/ano")
hist(h$counts, freq=FALSE, breaks=20, main="Histograma n? Furac?es no ano")
lines(seq(0,700,by=.5), dtriangle(seq(0,700,by=.5), 0, 700, 200), col="red")
lines(seq(0,800,by=.5), dtriangle(seq(0,800,by=.5), 0, 800, 200), col="blue")
lines(seq(0,750,by=.5), dtriangle(seq(0,750,by=.5), 0, 750, 200), col="green")  #?timo


hist(arq_atl$Maximum.Wind[arq_atl$Maximum.Wind > 0], freq = FALSE, main="Histograma Max_Wind")
lines(seq(10,150,by=.5), dtriangle(seq(10,150,by=.5),10,150,25), col="blue")
lines(seq(10,150,by=.5), dtriangle(seq(10,150,by=.5),10,130,25), col="green")   #?timo
lines(seq(10,150,by=.5), dtriangle(seq(10,150,by=.5),10,110,25), col="red")


hist(arq_atl$Minimum.Pressure[arq_atl$Minimum.Pressure > 0], freq = FALSE, main="Histograma Min_Press")
lines(seq(890,1030,by=.5), dtriangle(seq(890,1030,by=.5),920,1020,1000), col="green")   #?timo
lines(seq(890,1030,by=.5), dtriangle(seq(890,1030,by=.5),900,1030,1000), col="blue")
lines(seq(890,1030,by=.5), dtriangle(seq(890,1030,by=.5),930,1020,1000), col="red")


arq_atl$Latitude = as.numeric(substring(arq_atl$Latitude,1,nchar(arq_atl$Latitude)-1))
longit_list = numeric()
for (d in arq_atl$Longitude) {
  if(substring(d,nchar(d),nchar(d)) == "E"){
    longit_list <- append(longit_list, as.numeric(substring(d,1,nchar(d)-1))) 
  }
  else{
    longit_list <- append(longit_list, -1 * as.numeric(substring(d,1,nchar(d)-1))) 
  }
}

arq_atl$Longitude <- longit_list

hist(arq_atl$Latitude, freq = FALSE, main="Histograma Latitude", breaks=50)
lines(seq(0,80,by=.5), dnorm(seq(0,80,by=.5), mean=mean(arq_atl$Latitude), sd=sd(arq_atl$Latitude)), col="red")

hist(arq_atl$Longitude, freq = FALSE, main="Histograma Longitude", breaks = 50)
lines(seq(-150,50,by=.5), dnorm(seq(-150,50,by=.5), mean=mean(arq_atl$Longitude), sd=sd(arq_atl$Longitude)), col="red")

geraFuracaoMonteCarlo <- function(n=1){
  maxWind <- rtriangle(n,10,130,25)
  minPress <- rtriangle(n,920,1020,1000)
  latitude <- rnorm(n, mean=27.0449, sd=10.07788)
  longitude <- rnorm(n, mean=-65.68253, sd=19.68724)
  furacao <- c(mean(maxWind), mean(minPress), mean(latitude), mean(longitude))
  names(furacao) <- c("Maximum.Wind", "Minimum.Pressure", "Latitude", "Longitude")
  return(furacao)
}

# Previsão de Furacões em um período via TCL
previsaoFuracoes <- function(anos = 10, iter=1){
  mi_z <- anos * mean(h$counts)
  sigma2_z <- anos * var(h$counts)
  z <- rnorm(iter, mean = mi_z, sd = sqrt(sigma2_z))
  return(mean(z))
}