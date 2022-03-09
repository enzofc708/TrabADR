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

# Parte 1) Preparando o Experimento

# Importando o pacote "triangle" para simularmos distribuições triangulares

library("triangle") 

# Carregando as fontes de dados 

arq_atl = read.csv("atlantic.csv")
arq_atl$Date = as.Date(as.character(arq_atl$Date), "%Y%m%d") # Transformando as
                                                             # datas para o R

# Parte 2) Analisando as distribuições de Probabilidade

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
# furacão genérico através do Método de Monte Carlo, ou seja, através de 
# uma média entre múltiplas simulações.

geraFuracaoMonteCarlo <- function(n=1){
  maxWind <- rtriangle(n,10,130,25)
  minPress <- rtriangle(n,930,1020,1000)
  latitude <- rnorm(n, mean=27.0449, sd=10.07788)
  longitude <- rnorm(n, mean=-65.68253, sd=19.68724)
  furacao <- c(mean(maxWind), mean(minPress), mean(latitude), mean(longitude))
  names(furacao) <- c("Maximum.Wind", "Minimum.Pressure", "Latitude", "Longitude")
  return(furacao)
}

# Além disso, podemos rodar uma previsão do número de furacões em um dado número
# de anos. Isso pode ser feito de duas formas:
# 1 - Através da soma de N simulações anuais de furacões. 
# 2 - Através do uso do Teorema Central do Limite.

# Método 1 - Previsão de Furacões em um período via soma
previsaoFuracoes <- function(anos = 10, iter=1){
  sims <- numeric()
  for (i in 1:iter) {
    z <- sum(rtriangle(anos, 0, 750, 200))
    sims <- append(sims, z)
  }
  return(mean(z))
}

# Método 2 - Previsão de Furacões em um período via TCL
previsaoFuracoesTCL <- function(anos = 10, iter=1){
  mi_z <- anos * 950 / 3 # soma de N variáveis aleatórias com mesma média
  sigma2_z <- anos * 452500 / 18# soma de N variáveis aleatórias com mesma var
  z <- rnorm(iter, mean = mi_z, sd = sqrt(sigma2_z))
  return(mean(z))
}

# Parte 3) Casos de Teste

# Como casos de teste, iremos analisar as funções anteriores, que foram criadas
# com base nos experimentos feitos com os dados reais. 

# Experimento 1 - Furacão "médio"
# Através do método de Monte Carlo, iremos simular várias vezes a geração dos
# parâmetros acima para tentar descobrir os parâmetros mais comuns de um 
# furacão.
print("")
print("Experimentos:")
print("")
print("Furacão Médio:")
print(geraFuracaoMonteCarlo(n=10000))

# Experimento 2 - Validação do TCL
# Nesse experimento, iremos demonstrar o poder do Teorema Central do Limite, 
# simulando a soma do número de furacões registrados em um período dado de anos
# via TCL e via soma de N simulações.
print("")
print("Tabela Comparativa TCL/Soma:")
tabelaComp <- matrix(c(previsaoFuracoes(iter=10000), previsaoFuracoesTCL(iter=10000),
                       previsaoFuracoes(anos=50, iter=10000), previsaoFuracoesTCL(anos=50, iter=10000),
                       previsaoFuracoes(anos=100, iter=10000), previsaoFuracoesTCL(anos=100, iter=10000)),
                     nrow = 3, byrow = T, dimnames = list(
                       c("10 anos", "50 anos", "100 anos"),c("Soma","TCL")))
print(tabelaComp)

# Experimento 3 - Relatório Anual de FUracões
# Podemos combinar os experimentos anteriores e prever não só o número de 
# furacões em um ano, mas também suas características.
relatorio <- data.frame()
numFuracoes <- round(previsaoFuracoes(anos=1))
for(i in 1:numFuracoes){
  relatorio <- rbind(relatorio, geraFuracaoMonteCarlo(1))
}
names(relatorio) <- c("Maximum.Wind", "Minimum.Pressure", "Latitude", "Longitude")
print("")
print("Relatório Anual de furacões:")
print(relatorio)

# Parte 4) Conclusões

# Esse trabalho possibilitou ao grupo uma compreensão mais branda sobre o 
# conteúdo dado em aula, visto que tivemos a liberdade de não só escolher um
# tema, mas também de fazer os experimentos que julgássemos mais relevantes.
# Dentre os conceitos visto em aula, os que mais contribuíram para esse trabalho
# foram as distribuições de probabilidade vistas, sobretudo a normal e a 
# triangular e o Teorema Central do Limite. Inicialmente, havia um forte 
# interesse do grupo em calcular possíveis riscos financeiros advindos dos
# ataques de furacão, porém essa previsão rapidamente se mostrou inviável,
# pois os modelos de predição utilizados para tal fim envolvem resolução de 
# equações diferenciais e dependem de dados sócio-geográficos sobre os locais
# costeiros atingidos por esse fenômenos. Mesmo assim, a aplicação dos temas
# visto em aula foi extremamente útil e satisfatória, e todos os membros do
# grupo se sentem seguros para aplicar alguns conceitos cobertos pela 
# disciplina e se aprofundar em outras matérias.
