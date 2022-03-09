# Trabalho Final de Análise de Risco - Furacões

# Alunos: Daniel Souza (114143976)
# Enzo Carnevali (118153208)
# João Heitor (118170917)
# Thiago Oliveira (111466197)

# A proposta desse trabalho é gerar modelos de previsão relacionados a furacões
# que atingem a área do Oceano Atlântico. Esses modelos estimam não só o número
# de furacões em um dado período de tempo, mas também algumas características 
# intrínsecas a estes. Para tal, foram analisados dados de mais de 49 mil 
# furacões ocorridos entre os anos de 1851 e 2015, disponibilizados pela NHC
# (National Hurricane Center).

# 1) Preparando o Experimento

# Importando o pacote "triangle" para simularmos distribuições triangulares

library("triangle") 

# Carregando as fontes de dados 

arq_atl = read.csv("atlantic.csv")
arq_atl$Date = as.Date(as.character(arq_atl$Date), "%Y%m%d") # Transformando as
                                                             # datas para o R

# 2) Analisando as distribuições de Probabilidade

# Para gerar nossos modelos de previsão, pegamos alguns atributos do nosso
# conjunto de dados e tentamos aproximar graficamente por uma distribuição
# dada na disciplina. Em alguns casos, mais de uma distribuição é testada e,
# como critério de desempate, comparamos as médias e variâncias da distribuição
# proposta e da real.

# Analisando o número de furacões observados em um ano.

h <- hist(as.integer(format(arq_atl$Date,format="%Y")), breaks = 175, main="Histograma Furacões/ano")

# Histograma do histograma
hist(h$counts, freq=FALSE, breaks=20, main="Histograma # Furacões no ano") 


# Distribuições propostas
lines(seq(0,700,by=.5), dtriangle(seq(0,700,by=.5), 0, 700, 200), col="red")
lines(seq(0,800,by=.5), dtriangle(seq(0,800,by=.5), 0, 800, 200), col="blue")
lines(seq(0,750,by=.5), dtriangle(seq(0,750,by=.5), 0, 750, 200), col="green")

# Comparando as médias:
# Sabe-se que a média e variância de uma distribuição triangular podem ser
# calculadas através de uma fórmula fechada. Sendo assim:

mediaTriang <- function(min, max, mp){
  return((min + max + mp)/3)
}

varTriang <- function(min, max, mp){
  return((min^2 + max^2 + mp^2 - min * max - max * mp - min * mp)/18)
}

tabelaComp <- matrix(c(mean(h$counts), var(h$counts),
                       mediaTriang(0, 700, 200), varTriang(0,700,200),
                       mediaTriang(0, 800, 200), varTriang(0, 800, 200),
                       mediaTriang(0, 750, 200), varTriang(0, 750, 200)),
                     nrow = 4, byrow = T, dimnames = list(
                       c("Original", "Vermelho", "Azul", "Verde"),
                       c("Média","Variância")))
print("Comparação Médias # Furacões")
print(tabelaComp)

# A distribuição triangular verde é a mais equilibrada, possuindo tanto média
# quanto variância bem próximas a real.

# Analisando a velocidade máxima de um furacão.

# Distribuições propostas
hist(arq_atl$Maximum.Wind[arq_atl$Maximum.Wind > 0], freq = FALSE, main="Histograma Max_Wind")
lines(seq(10,150,by=.5), dtriangle(seq(10,150,by=.5),10,150,25), col="blue")
lines(seq(10,150,by=.5), dtriangle(seq(10,150,by=.5),10,130,25), col="green")   #ótimo
lines(seq(10,150,by=.5), dtriangle(seq(10,150,by=.5),10,110,25), col="red")

# Comparando as médias:

tabelaComp <- matrix(c(mean(arq_atl$Maximum.Wind[arq_atl$Maximum.Wind > 0]), var(arq_atl$Maximum.Wind[arq_atl$Maximum.Wind > 0]),
                       mediaTriang(10,150,25), varTriang(10,150,25),
                       mediaTriang(10,130,25), varTriang(10,130,25),
                       mediaTriang(10,110,25), varTriang(10,110,25)),
                     nrow = 4, byrow = T, dimnames = list(
                       c("Original", "Azul", "Verde", "Vermelho"),
                       c("Média","Variância")))
print("Comparação Velocidades Máximas")
print(tabelaComp)

# Novamente, a distribuição triangular verde é a mais equilibrada, possuindo 
# tanto média quanto variância bem próximas a real.


# Analisando a pressão mínima no centro de um furacão.

hist(arq_atl$Minimum.Pressure[arq_atl$Minimum.Pressure > 0], freq = FALSE, main="Histograma Min_Press")

# Distribuições propostas
lines(seq(890,1030,by=.5), dtriangle(seq(890,1030,by=.5),920,1020,1000), col="green")
lines(seq(890,1030,by=.5), dtriangle(seq(890,1030,by=.5),900,1030,1000), col="blue")
lines(seq(890,1030,by=.5), dtriangle(seq(890,1030,by=.5),930,1020,1000), col="red")

tabelaComp <- matrix(c(mean(arq_atl$Minimum.Pressure[arq_atl$Minimum.Pressure > 0]), var(arq_atl$Minimum.Pressure[arq_atl$Minimum.Pressure > 0]),
                       mediaTriang(920,1020,1000), varTriang(920,1020,1000),
                       mediaTriang(900,1030,1000), varTriang(900,1030,1000),
                       mediaTriang(930,1020,1000), varTriang(930,1020,1000)),
                     nrow = 4, byrow = T, dimnames = list(
                       c("Original", "Verde", "Azul", "Vermelho"),
                       c("Média","Variância")))
print("Comparação Pressões Mínimas Furacões")
print(tabelaComp)

# Agora, a distribuição triangular vermelha é a mais equilibrada, possuindo 
# tanto média quanto variância bem próximas a real.


# Analisando a latitude e longitude de um furacão.

# Convertendo para valores numéricos
arq_atl$Latitude = as.numeric(substring(arq_atl$Latitude,1,nchar(arq_atl$Latitude)-1))

longit_list = numeric()

# Convertendo de "E" e "W" para + e -
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

# Nesse caso, tanto a latitude quanto a longitude são bem aproximadas por uma 
# curva normal com média e variância iguais a da distribuição real.

# Com esses modelos construídos, podemos criar uma função capaz de gerar um 
# furacão genérico

geraFuracaoMonteCarlo <- function(n=1){
  maxWind <- rtriangle(n,10,130,25)
  minPress <- rtriangle(n,930,1020,1000)
  latitude <- rnorm(n, mean=27.0449, sd=10.07788)
  longitude <- rnorm(n, mean=-65.68253, sd=19.68724)
  furacao <- c(mean(maxWind), mean(minPress), mean(latitude), mean(longitude))
  names(furacao) <- c("Maximum.Wind", "Minimum.Pressure", "Latitude", "Longitude")
  return(furacao)
}

# Previsão de Furacões em um período via TCL
previsaoFuracoesTCL <- function(anos = 10, iter=1){
  mi_z <- anos * 950 / 3
  sigma2_z <- anos * 452500 / 18
  z <- rnorm(iter, mean = mi_z, sd = sqrt(sigma2_z))
  return(mean(z))
}

# Previsão de Furacões em um período via soma
previsaoFuracoes <- function(anos = 10, iter=1){
  sims <- numeric()
  for (i in 1:iter) {
    z <- sum(rtriangle(anos, 0, 750, 200))
    sims <- append(sims, z)
  }
  return(mean(z))
}