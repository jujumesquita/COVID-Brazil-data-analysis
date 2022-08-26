#1) Descubra quais são os municípios Brasileiros que não presentaram nenhum caso de covid19 no período de análise.


install.packages(c("writexl"), dependencies = TRUE)
setwd("C:/Users/jujum/Desktop/")

base_covid <- read.csv(file = "HIST_PAINEL_COVIDBR_19out2020.csv",sep = ";", header = TRUE)
dim(base_covid)
colnames(base_covid)

#filtro somente pela última data
b_covid1 <- base_covid[base_covid$data == "2020-10-19", ]
dim(b_covid1)
munic_zero_caso <- b_covid1[b_covid1$casosAcumulado==0, ]
dim(munic_zero_caso)
unique(munic_zero_caso$municipio)

#2) Diga quantos são os municípios onde não houve nenhuma morte de covid 19 no período de análise.

munic_zero_obito <- b_covid1[b_covid1$obitosAcumulado==0, ]
dim(munic_zero_obito)
unique(munic_zero_obito$municipio)



#3) Crie um vetor que contenha a população de cada Estado Brasileiro.

base_covid3<- base_covid[,c("regiao","municipio", "estado", "populacaoTCU2019")]
b_covid3 <- base_covid3[base_covid3$municipio == "",] #filtro as linhas que não possuem municípios
sem_duplicatas3 <- unique(b_covid3)
pop_estado3 <- sem_duplicatas3[2:28, 3:4]
pop_estado3_ord <- pop_estado3[order(pop_estado3$estado), ]
vet_pop_estado3<-pop_estado3_ord$populacaoTCU2019
vet_pop_estado3
sum(vet_pop_estado3)




#4) Calcule a quantidade de casos total de cada Estado Brasileiro no período de análise.
b_covid4 <- base_covid[base_covid$estado != "", ]
dim(b_covid4)
b_covid44 <- b_covid4[b_covid4$municipio == "", ]
dim(b_covid44)
casos_estado4<-tapply(b_covid44$casosNovos, b_covid44$estado, sum, na.rm = TRUE)
casos_estado4


#5) Faça um gráfico de barras com a totalidade de casos de cada Estado Brasileiro no período de análise, organizado em ordem crescente de casos.

casos_estado4_ord<- sort(casos_estado4)
casos_estado4_ord
setwd("C:/Users/jujum/Desktop/") 
pdf(file = "Questão_5.pdf", width = 13, height = 6)
barplot(casos_estado4_ord, main="Total de casos de COVID-19 por estado", xlab="Estado", ylab="Número casos", 
        col="blue", ylim = c(0,1500000), cex.axis=0.75, cex.lab=0.75)
dev.off() 


#6) Faça um gráfico de barras de com a totalidade de casos em relação à população de cada Estado 
#(Percentual). O gráfico deve ser organizado com os estados em ordem crescente de porcentagem.

#criando vetor de casos por estado
vetor_casos_estado <- as.vector(casos_estado4)
vetor_casos_estado
vetor_casos_estado1 <- vetor_casos_estado[c(-1)]
vetor_casos_estado1
sum(vetor_casos_estado1)

#Criando um vetor de estados

casos_estado2 <- as.data.frame.table(casos_estado4)
head(casos_estado2)
casos_estado3 <- casos_estado2[-c(1), ]
casos_estado3
colnames(casos_estado3) <- c("Estado", "Num_casos")
casos_estado3
vetor_estado <- casos_estado3$Estado
vetor_estado

#calculando as porcentagens

percentual_casos_estad <- (vetor_casos_estado1/vet_pop_estado3)*100
percentual_casos_estad

#criando um dataframe com os estados e os percentuais

df_estado_percent <- data.frame(vetor_estado, percentual_casos_estad)
df_estado_percent
df_estado_percent_ord <- df_estado_percent[order(df_estado_percent$percentual_casos_estad), ]
df_estado_percent_ord

#criando vetor de estados ordenados
vetor_estado_ord <- df_estado_percent_ord$vetor_estado
vetor_estado_ord

setwd("C:/Users/jujum/Desktop/") 
pdf(file = "Questão_6.pdf", width = 13, height = 6)
barplot(height=df_estado_percent_ord$percentual_casos_estad, main="% de casos de COVID-19 na população de cada estado", xlab="Estado", ylab="% casos na população", 
        names.arg=vetor_estado_ord, col="blue", ylim = c(0,20), cex.axis=0.75, cex.lab=0.75)
dev.off() 

#7) Repita os gráficos das questões 5 e 6 mostrando a quantidade de óbitos de cada Estado e 
#quantidade percentual de óbitos em relação a população de cada Estado.

#7.1 - ÓBITOS

#Calculando obitos por estado

b_covid7 <- base_covid[base_covid$estado != "", ]
b_covid77 <- b_covid4[b_covid4$municipio == "", ]
obitos_estado7<-tapply(b_covid77$obitosNovos, b_covid77$estado, sum, na.rm = TRUE)
obitos_estado7


#Ordenando os óbitos por estado
obitos_estado_ord <- sort(obitos_estado7)
obitos_estado_ord

#Fazendo o gráfico de barras do número de óbitos por estado

setwd("C:/Users/jujum/Desktop/") 
pdf(file = "Questão_7.1.pdf", width = 13, height = 6)
barplot(obitos_estado_ord,  main="Número de óbitos por COVID-19 na população de cada estado", xlab="Estado")
dev.off()

#7.2 - % ÓBITOS

#Criando vetor de óbitos por estado

vetor_obitos_estado <- as.vector(obitos_estado7)
vetor_obitos_estado
vetor_obitos_estado1 <- vetor_obitos_estado[c(-1)]
vetor_obitos_estado1

#calculando as porcentagens
percentual_obitos_estad <- (vetor_obitos_estado1/vet_pop_estado3)*100
percentual_obitos_estad

#criando um dataframe com os estados e os percentuais
df_estado_percent_obito <- data.frame(vetor_estado, percentual_obitos_estad)
df_estado_percent_obito
df_estado_percent_obito_ord <- df_estado_percent_obito[order(df_estado_percent_obito$percentual_obitos_estad), ]
df_estado_percent_obito_ord


#criando vetor de estados ordenados
vetor_estado_ord_obitos <- df_estado_percent_obito_ord$vetor_estado
vetor_estado_ord_obitos

#Criando o gráfico

setwd("C:/Users/jujum/Desktop/") 
pdf(file = "Questão_7.2.pdf", width = 13, height = 6)
barplot(height=df_estado_percent_obito_ord$percentual_obitos_estad, main="% de óbitos por COVID-19 na população de cada estado", xlab="Estado", ylab="% óbitos na população", 
        names.arg=vetor_estado_ord_obitos, col="blue", ylim = c(0,0.4), cex.axis=0.75, cex.lab=0.75)
dev.off()

#8) Calcule a taxa de letalidade, em porcentagem, de cada Estado. (letalidade = mortes/casos)

vetor_obitos_estado1
vetor_casos_estado1


tx_letalidade <- (vetor_obitos_estado1/vetor_casos_estado1)*100
tx_letalidade

df_letalidade_estado <- data.frame(vetor_estado, tx_letalidade)
df_letalidade_estado

#9) Faça um gráfico de barras com a taxa de letalidade de cada Estado, organizado em ordem crescente de letalidade.

df_letalidade_estado_ord <- df_letalidade_estado[order(df_letalidade_estado$tx_letalidade), ]
df_letalidade_estado_ord


#criando vetor de estados ordenados
vetor_estado_ord_letalidade <- df_letalidade_estado_ord$vetor_estado
vetor_estado_ord_letalidade

#Criando o gráfico

setwd("C:/Users/jujum/Desktop/") 
pdf(file = "Questão_9.pdf", width = 13, height = 6)
barplot(height=df_letalidade_estado_ord$tx_letalidade, main="Taxa de letalidade de COVID-19 por estado (%)", xlab="Estado", ylab="Taxa de letalidade (%)", 
        names.arg=vetor_estado_ord_letalidade, col="blue", ylim = c(0,10), cex.axis=0.75, cex.lab=0.75)
dev.off()

#10) Crie um dataset que contenha a quantidade de novos casos por dia para cada Estado Brasileiro.

colnames(base_covid)
base_covid10<- base_covid[,c("municipio", "estado", "casosNovos", "obitosNovos" , "data", "regiao")]
b_covid11 <- base_covid10[base_covid10$estado != "", ]
b_covid12 <- b_covid11[b_covid11$municipio == "", ]
dim(b_covid12)
casos_novos_dia_estado<- tapply(b_covid12$casosNovos, list(b_covid12$data, b_covid12$estado), sum, na.rm = TRUE )
casos_novos_dia_estado
casos_novos_dia_estado_df <- as.data.frame(casos_novos_dia_estado)
casos_novos_dia_estado_df
dim(casos_novos_dia_estado_df)
View(casos_novos_dia_estado_df)



#11) Crie um dataset que contenha a quantidade de novos óbitos por dia para cada Estado Brasileiro.
obitos_novos_dia_estado<- tapply(b_covid12$obitosNovos, list(b_covid12$data, b_covid12$estado), sum, na.rm = TRUE )
obitos_novos_dia_estado
obitos_novos_dia_estado_df <- as.data.frame(obitos_novos_dia_estado)
obitos_novos_dia_estado_df
dim(obitos_novos_dia_estado_df)
View(obitos_novos_dia_estado_df)



#12) Crie uma função que calcule a média móvel de ordem n de um vetor.

# A ordem n é determinada para a variável n e o vetor input é x

media_movel <- function(x,n) {
vet_media <- c()
for(i in 1:length(x)) {
    if (i>n) {
    vet <- c()
    pos<- seq(from =i, to=i-n+1, by=-1)
    vet <- c(vet, x[pos])
    print(vet)
    media_calc <- ((sum(vet, na.rm=TRUE)/n))
    vet_media <- c(vet_media, media_calc)
    print(vet_media)
  } else {vet_media <- c(0, vet_media )

  }
}
return(vet_media)

}


#13) Crie 2 datasets com as médias móveis de 7 dias para os casos diários de cada estado (q10) e
# para óbitos diários de cada estado (q11)

#Média móvel 7 dias para os casos diários de casa estado

View(casos_novos_dia_estado_df)
vetor_estado

lista13 <- list()

for(i in vetor_estado){
  x <- casos_novos_dia_estado_df[,i]
  output <- media_movel(x,7)
  lista13[[i]] <- output
}

mm7_casos_estados <- data.frame(lista13)
View(mm7_casos_estados)



#Média móvel 7 dias para os óbitos diários de casa estado

View(obitos_novos_dia_estado_df)
vetor_estado

lista132 <- list()

for(i in vetor_estado){
  x <- obitos_novos_dia_estado_df[,i]
  output2 <- media_movel(x,7)
  lista132[[i]] <- output2
}

mm7_obitos_estados <- data.frame(lista132)
View(mm7_obitos_estados)


#14) Faça um gráfico de linhas com a totalidade de casos diários Brasileiros e a sua média móvel de 7 dias.

#total casos diários no BRASIL
View(casos_novos_dia_estado_df)
casos_novos_dia_estado_df$BR<-rowSums(casos_novos_dia_estado_df[,], na.rm = TRUE)
casos_novos_dia_estado_df$data2<-row.names(casos_novos_dia_estado_df[,])
View(casos_novos_dia_estado_df) 

#transformando a coluna data2 em formato data
xplot3 <- as.Date(casos_novos_dia_estado_df$data2)
min_data <- min(xplot3)
min_data
max_data <- max(xplot3)
max_data

plot(x = xplot3, y = casos_novos_dia_estado_df$BR, 
     xlab = "Dia", ylab = "Qtd. casos", type = "l",
     main = "Qtde casos COVID-19 no Brasil em 2020", xlim = c(min_data, max_data), ylim = c(0,70000), col = "blue")


#Calculando a média móvel de 7 dias para o número de casos no Brasil

mm_7_casos_br <- media_movel(casos_novos_dia_estado_df$BR,7)


#Plotando 2 gráficos juntos

setwd("C:/Users/jujum/Desktop/") 
pdf(file = "Questão_14.pdf", width = 13, height = 6)

plot(x = xplot3, y = casos_novos_dia_estado_df$BR, 
     xlab = "Dia", ylab = "Qtd. casos", type = "l",
     main = "Qtde casos COVID-19 no Brasil em 2020 e sua média móvel de 7 dias", xlim = c(min_data, max_data), ylim = c(0,70000), col = "blue")
lines(x = xplot3, y = mm_7_casos_br, type = "l", col = "orange")
legend("topright", legend = c("Qtde de casos", "Média móvel"), 
       pch = 16, col = c("blue", "orange"))

dev.off()




  
#15) Faça um gráfico de linhas com a totalidade de óbitos diários Brasileiros e sua média móvel de 7 dias.

View(obitos_novos_dia_estado_df)
obitos_novos_dia_estado_df$BR<-rowSums(obitos_novos_dia_estado_df[,], na.rm = TRUE)
obitos_novos_dia_estado_df$data2<-row.names(obitos_novos_dia_estado_df[,])
View(obitos_novos_dia_estado_df)  

#transformando a coluna data2 em formato data
xplot4 <- as.Date(obitos_novos_dia_estado_df$data2)
min_data4 <- min(xplot4)
min_data4
max_data4 <- max(xplot4)
max_data4


#gráfico da qtde de óbitos no Brasil
plot(x = xplot4, y = obitos_novos_dia_estado_df$BR, 
     xlab = "Dia", ylab = "Qtd. óbitos", type = "l",
     main = "Qtde óbitos COVID-19 no Brasil", xlim = c(min_data4, max_data4), ylim = c(0,2000), col = "red")


#Calculando a média móvel de 7 dias
mm_7_obitos_br <- media_movel(obitos_novos_dia_estado_df$BR,7)



#grafico da média movel
plot(x = xplot3, y = vet_media15, 
     xlab = "Dia", ylab = "Média móvel", type = "l",
     main = "Média móvel da qtde óbitos COVID-19 no Brasil", xlim = c(min_data, max_data), ylim = c(0,1500), col = "black")


#Plotando 2 gráficos juntos

setwd("C:/Users/jujum/Desktop/") 
pdf(file = "Questão_15.pdf", width = 13, height = 6)

plot(x = xplot4, y = obitos_novos_dia_estado_df$BR, 
     xlab = "Dia", ylab = "Qtd. óbitos", type = "l",
     main = "Qtde óbitos de COVID-19 em 2020 no Brasil e média móvel de 7 dias", xlim = c(min_data4, max_data4), ylim = c(0,2000), col = "red")
lines(x = xplot4, y = mm_7_obitos_br, type = "l", col = "orange")
legend("topright", legend = c("Qtde de casos", "Média móvel"), 
       pch = 16, col = c("red", "orange"))

dev.off()



#16) Salve o dataset de casos diários de cada Estado no formato xlsx.

library(writexl)
write_xlsx(x = casos_novos_dia_estado_df, path = "casos_dia_total.xlsx", col_names = TRUE)

#17) Salve o dataset de óbitos diários de cada Estado no formato xlsx.

library(writexl)
write_xlsx(x = obitos_novos_dia_estado_df, path = "obitos_dia_total.xlsx", col_names = TRUE)


#18) Crie um dataset para a quantidade de casos diários de cada Região Brasileira e um dataset para a quantidade de óbitos diários de cada Região Brasileira.

#Quantidade de casos diários por regiao

base_covid18<- base_covid[,c("regiao", "municipio", "estado", "casosNovos", "obitosNovos" , "data", "regiao")]
b_covid18 <- base_covid18[base_covid18$estado != "", ]
b_covid181 <- b_covid18[b_covid18$municipio == "", ]
dim(b_covid181)
casos_regiao_dia<-tapply(b_covid181$casosNovos, list(b_covid181$data, b_covid181$regiao), sum, na.rm = TRUE)
casos_regiao_dia
casos_regiao_dia_df <- as.data.frame(casos_regiao_dia)
casos_regiao_dia_df<-casos_regiao_dia_df[ ,-1]
View(casos_regiao_dia_df)


#Quantidade de óbitos diários por regiao

obitos_regiao_dia<-tapply(b_covid181$obitosNovos, list(b_covid181$data, b_covid181$regiao), sum, na.rm = TRUE)
obitos_regiao_dia
obitos_regiao_dia_df <- as.data.frame(obitos_regiao_dia)
obitos_regiao_dia_df<-obitos_regiao_dia_df[ ,-1]
View(obitos_regiao_dia_df)



#19) Aplique a média móvel de 7 dias nos dois datasets da questão 18.


# 19.1 Aplicando a média móvel de 7 dias na quantidade de  casos diários por região 

View(casos_regiao_dia_df)
vetor_regiao <- c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")
vetor_regiao

lista191 <- list()

for(i in vetor_regiao){
  x <- casos_regiao_dia_df[,i]
  output191 <- media_movel(x,7)
  lista191[[i]] <- output191
}

mm7_casos_regiao <- data.frame(lista191)
View(mm7_casos_regiao)



# 19.2 Aplicando a média móvel de 7 dias na quantidade de  óbitos diários por região 

View(obitos_regiao_dia_df)

lista192 <- list()

for(i in vetor_regiao){
  x <- obitos_regiao_dia_df[,i]
  output192 <- media_movel(x,7)
  lista192[[i]] <- output192
}

mm7_obitos_regiao <- data.frame(lista192)
View(mm7_obitos_regiao)




#20) Faça um gráfico de linhas com a média móvel de casos diários de cada Região Brasileira.

View(mm7_casos_regiao)
setwd("C:/Users/jujum/Desktop/") 
pdf(file = "Questão_20.pdf", width = 13, height = 6)

plot(x = xplot4, y = mm7_casos_regiao$Centro.Oeste, 
     xlab = "Dia", ylab = "Qtd. casos", type = "l",
     main = "Média móvel de 7 dias da qtde casos COVID-19 nas regiões do Brasil", xlim = c(min_data4, max_data4), ylim = c(0,20000), col = "red")
lines(x = xplot4, y = mm7_casos_regiao$Nordeste, type = "l", col = "orange")
lines(x = xplot4, y = mm7_casos_regiao$Norte, type = "l", col = "black")
lines(x = xplot4, y = mm7_casos_regiao$Sudeste, type = "l", col = "blue")
lines(x = xplot4, y = mm7_casos_regiao$Sul, type = "l", col = "green")
legend("topright", legend = c("CO", "NE", "NO","SE", "SU"), 
       pch = 16, col = c("red", "orange", "black", "blue", "green"))

dev.off()


#21) Faça um gráfico de linhas com a média móvel de óbitos diários de cada Região Brasileira.

View(mm7_obitos_regiao)
setwd("C:/Users/jujum/Desktop/") 
pdf(file = "Questão_21.pdf", width = 13, height = 6)

plot(x = xplot4, y = mm7_obitos_regiao$Centro.Oeste, 
     xlab = "Dia", ylab = "Qtd. óbitos", type = "l",
     main = "Média móvel de 7 dias do número de óbitos de COVID-19 nas regiões do Brasil", xlim = c(min_data4, max_data4), ylim = c(0,800), col = "red")
lines(x = xplot4, y = mm7_obitos_regiao$Nordeste, type = "l", col = "orange")
lines(x = xplot4, y = mm7_obitos_regiao$Norte, type = "l", col = "black")
lines(x = xplot4, y = mm7_obitos_regiao$Sudeste, type = "l", col = "blue")
lines(x = xplot4, y = mm7_obitos_regiao$Sul, type = "l", col = "green")
legend("topright", legend = c("CO", "NE", "NO","SE", "SU"), 
       pch = 16, col = c("red", "orange", "black", "blue", "green"))

dev.off()




