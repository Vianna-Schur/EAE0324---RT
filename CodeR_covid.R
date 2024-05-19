# ----------------------------------
# Leitura dos dados da PNADC - COVID
# Ano 2020 - Mês X
# ----------------------------------

rm(list = ls())

# Selecione o diretorio do seu trabalho -----

setwd("C:\\Trabalho - Econometria I\\Turma 2-20230927") 

getwd()

# Pacotes ------

  # Usando o pacote COVIDIBGE e' possivel fazer o download da
  # base de dados direto do site do IBGE
  # Instale e chame o pacote
  # Requer conexao com a internet

install.packages("COVIDIBGE")

library(COVIDIBGE)
library(tidyverse)

# Download da base de dados ------

  # criamos um vetor com as variaveis que queremos trazer da PNADC - Covid,
  # porque a base completa tem muitas variaveis que nao fazem sentido para as
  # analises propostas

variables<-c("Ano","UF","V1008","V1012","V1013","V1016","V1022","V1023","V1032","A001A",
             "A001B1","A001B2","A001B3","A002","A003","A004","A005","B0011","B0012","B0013",
             "B0014","B0015","B0016","B0017","B0018","B0019","B00110","B00111","B00112",
             "C001","C002","C003","C004","C005","C0051","C0052","C0053","C007","C007B",
             "C007C","C007D","C007E","C007E1","C007E2","C008","C009","C010","C01012","C011A",
             "C011A12","C012","C013","C014","C015","C016","C017A","D0011","D0013","D0021",
             "D0023","D0031","D0033","D0041","D0043","D0051","D0053","D0061","D0063","D0071",
             "D0073","F001","F0021")

# Onde esta X no comando abaixo colocar o Mes 
covid.df <- get_covid(year=2020, 
                      month=11, 
                      labels=TRUE, 
                      deflator=FALSE, 
                      design=FALSE, 
                      vars=variables)

# Mesmo escolhendo as variaveis, o pacote traz algumas que nao iremos usar
# entao fazemos uma selecao para obter uma base com 72 variaveis

covid <- covid.df  %>% select(all_of(variables))

rm(covid.df) # remove o objeto anterior

# Essa e' a base que voce deve utilizar no seu trabalho

# Salvar, ler e detalhes da base ----


# Salvar a base para usar posteriormente (sera salvo no diretorio que foi escolhido)

save(covid,file = "covid.RData")
rm(list = ls())

# Acessando a base que foi salva 

load("covid.RData") 

# Consultando detalhes sobre a base e variaveis que ela possui

class(covid)
str(covid)
glimpse(covid)
show(covid)

unique1_values <- unique(covid$V1022)
unique2_values <- unique(covid$A003)

print(unique1_values)
print(unique2_values)

#Criando novos valores para variáveis categóricas de interesse para controle

#Educação (0 = sem ensino superior; 1 = com ensino superior)

covid$educ <- ifelse(covid$A005 %in% c('Superior Completo', 'Pós-graduação, mestrado ou doutorado'), 1, 0)

#Região (0 = Região Nordeste; 1 = Sudeste)


covid$regiao <- ifelse(covid$UF %in% c('São Paulo', 'Rio de Janeiro', 'Espírito Santo', 'Minas Gerais'), 1, ifelse(covid$UF %in% c('Alagoas', 'Bahia', 'Ceará', 'Maranhão', 'Piauí', 'Sergipe', 'Paraíba', 'Pernambuco', 'Rio Grande do Norte'), 0, NA))

#Situação do Domicílio (0 = Rural, 1 = Urbana)

covid$sit <- ifelse(covid$V1022 %in% c('Urbana'), 1, 0)

#Raça (0 = não-branco, 1 = branco)

covid$raca <- ifelse(covid$A004 %in% c('Branca'), 1, 0)

#Sexo (0 = mulher, 1 = homem)

covid$sexo <- ifelse(covid$A003 %in% c('Homem'), 1, 0)

#Condição no domicílio (1 = responsável pelo domicílio , 0 = outros)

covid$cond <- ifelse(covid$A001A %in% c('Pessoa responsável pelo domicílio'), 1, 0)

#Salário (logaritmo)

covid$logsal <- log10(covid$C01012)

#Idade 

covid$Idade <- covid$A002

dados_selecionados <- subset(covid, select = c(logsal, Idade, sexo, cond, sit, raca, regiao, educ))
dados_consolidados <- na.omit(dados_selecionados)


#Transformando em factor para a regressão

dados_consolidados$sexo = as.factor(dados_consolidados$sexo)
dados_consolidados$cond = as.factor(dados_consolidados$cond)
dados_consolidados$sit = as.factor(dados_consolidados$sit)
dados_consolidados$raca = as.factor(dados_consolidados$raca)
dados_consolidados$regiao = as.factor(dados_consolidados$regiao)
dados_consolidados$educ = as.factor(dados_consolidados$educ)



class(dados_consolidados)
str(dados_consolidados)
glimpse(dados_consolidados)
show(dados_consolidados)
unique1_values <- unique(dados_consolidados$educ)
unique2_values <- unique(dados_consolidados$raca)



modelo <- lm(logsal ~ regiao + raca + cond + sit + educ + Idade + sexo , data = dados_consolidados)
summary(modelo)


install.packages("stargazer")
library(stargazer)

dadoshomem <- subset(dados_consolidados, sexo == 1)
dadosmulher <- subset(dados_consolidados, sexo == 0)


modelohomem <- lm(logsal ~ regiao + raca + cond + sit + educ + Idade , data = dadoshomem)
modelomulher <- lm(logsal ~ regiao + raca + cond + sit + educ + Idade , data = dadosmulher)



stargazer(modelo, type = "text", digits = 12 , report = ("vcpt"))
stargazer(modelohomem, type = "text", digits = 12 , report = ("vcpt"))
stargazer(modelomulher, type = "text", digits = 12 )

stargazer(modelohomem, type = "text", report = ("vcpt"), out="ResultadosSubgrupoHomem.txt", star.cutoffs = c(0.05, 0.01, 0.001))
stargazer(modelomulher, type = "text", report = ("vcpt"), out="ResultadosSubgrupoMulher.txt", star.cutoffs = c(0.05, 0.01, 0.001))
stargazer(modelo, type = "text", report = ("vcpt"), out="Resultados.txt", star.cutoffs = c(0.05, 0.01, 0.001))

install.packages("openxlsx")
library(openxlsx)
write.xlsx(dados_consolidados, file = "dados_consolidados.xlsx")




# Redução do conjunto de dados para fazer o envelope simulado (É uma operação bem custosa computacionalmente)
set.seed(123)  # Definir uma semente para reprodutibilidade
porcentagem <- 0.5  # Definir a porcentagem desejada, e.g., 10%

# Calcular o número de linhas a serem selecionadas
n <- nrow(dados_consolidados)
tamanho_amostra <- round(n * porcentagem)

# Selecionar linhas aleatórias
linhas_selecionadas <- sample(1:n, tamanho_amostra)

# Criar o novo conjunto de dados
dados_pequeno_porte <- dados_consolidados[linhas_selecionadas, ]
modelo_pequeno_porte <- lm(logsal ~ regiao + raca + cond + sit + educ + Idade + sexo , data = dados_pequeno_porte)

summary(modelo_pequeno_porte)

#Definição de envelope simulado

#ENVELOPE SIMULADO
       fit.model <- modelo_pequeno_porte
       attach(dados_consolidados)
       
       par(mfrow=c(1,1))
       X <- model.matrix(fit.model)
       n <- nrow(X)
       p <- ncol(X)
       H <- X%*%solve(t(X)%*%X)%*%t(X)
       h <- diag(H)
       si <- lm.influence(fit.model)$sigma
       r <- resid(fit.model)
       tsi <- r/(si*sqrt(1-h))
       #
       ident <- diag(n)
       epsilon <- matrix(0,n,50)
       e <- matrix(0,n,50)
       e1 <- numeric(n)
       e2 <- numeric(n)
       #
       for(i in 1:50){
         epsilon[,i] <- rnorm(n,0,1)
         e[,i] <- (ident - H)%*%epsilon[,i]
         u <- diag(ident - H)
         e[,i] <- e[,i]/sqrt(u)
         e[,i] <- sort(e[,i]) }
       #
       for(i in 1:n){
         eo <- sort(e[i,])
         e1[i] <- (eo[2]+eo[3])/2
         e2[i] <- (eo[47]+eo[48])/2 }
       #
       med <- apply(e,1,mean)
       faixa <- range(tsi,e1,e2)
       #
       par(pty="s")
       qqnorm(tsi,xlab="Percentil da N(0,1)",
              ylab="Residuo Studentizado", ylim=faixa, pch=16, cex=0.01, main="")
       par(new=TRUE)
       qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
       par(new=TRUE)
       qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
       par(new=TRUE)
       qqnorm(med,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=2, main="")
       
       
       
       
