library(xlsx)
library(openxlsx)
library(tidyverse)
library(AgroR)

### Carregando arquivo de dados
dados <- read.xlsx("DADOS.xlsx")

View(dados)
### ACESSANDO GRUPOS
GRUPOS <- as.factor(dados$PRAGA)
TRAT <- as.factor(dados$Trat)
PONTO <- as.factor(dados$Ponto)
DATA <- as.factor(dados$DATA2)
VAR <- as.integer(dados$N_lagartas)

## COMPILANDO DATAFRAME
DF <- data.frame(GRUPOS, TRAT, PONTO, DATA, VAR)

##################################

### CONTADORESS
#### O código de milhões

### Verbatimtext --  Saida todo o resultado


for(i in 1:length(levels(DF$GRUPOS))){ #1
  j = 1
    l = 1
    for(j in 1:length(levels(DF$DATA))){ ##1
    SAIDA <- NULL
    MEDIAS <- NULL
    RESULT <- NULL
    letras <- NULL 
    
    BASE_FILTRO <- filter(DF, GRUPOS == levels(DF$GRUPOS)[i], DATA == levels(DF$DATA)[j])
    cat("Resultado - ")
    cat("\n")
    MEDIAS <- aggregate(VAR ~ TRAT, FUN = mean, data = BASE_FILTRO)
 
    X <- DBC.glm(BASE_FILTRO$TRAT, BASE_FILTRO$PONTO, BASE_FILTRO$VAR, glm.family = "poisson")
    
    for(l in 1:length(levels(DF$TRAT))){
      letras <- c(letras, data.frame(strsplit(X$data$letra, " "))[2,l])
    }
    
    cat("\n")
    cat("\n")
    cat("Resultado final para:")
    cat("GRUPO: ")
    print(levels(DF$GRUPOS)[i])
    cat("DATA: ")
    print(levels(DF$DATA)[j])
    RESULT <- data.frame(TRAT = X$data$trat, Letras = letras)
    cat("\n")
    cat("\n")
    SAIDA <- merge(MEDIAS, RESULT)
    print(SAIDA)
    }
}


