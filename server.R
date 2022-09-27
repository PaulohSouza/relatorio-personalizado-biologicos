

library(shiny)
library(xlsx)
library(tidyverse)
library(DT)
library(openxlsx)
library(AgroR)

#dados <- read.xlsx("DADOS.xlsx")

shinyServer(function(input, output, session) {
  
  #### Variávei globais reactivas
  vals <- reactiveValues(x = NULL)
  
  
  ############################################################
  #     Ações e eventos                                      #
  ############################################################
  
  observe({
    if(input$ACT_CONJUNTO_TO_BD > 0){
      print("ACT_CONJUNTO_TO_BD")
      session$sendCustomMessage("myCallbackHandler", "ACT_CONJUNTO_TO_BD")
    }
  })
  
  observe({
    if(input$DADOS_PARA_RESULTADOS > 0){
      print("DADOS_PARA_RESULTADOS")
      session$sendCustomMessage("myCallbackHandler", "DADOS_PARA_RESULTADOS")
    }
  })
  
  
  observe({
    if(input$DADOS_PARA_INICIO > 0){
      print("DADOS_PARA_INICIO")
      session$sendCustomMessage("myCallbackHandler", "DADOS_PARA_INICIO")
    }
  })
  
  
  observe({
    if(input$ANALISE_PARA_DADOS > 0){
      print("ANALISE_PARA_DADOS")
      session$sendCustomMessage("myCallbackHandler", "ANALISE_PARA_DADOS")
    }
  })
  
  observe({
    if(input$GRAFICOS_PARA_DADOS > 0){
      print("GRAFICOS_PARA_DADOS")
      session$sendCustomMessage("myCallbackHandler", "GRAFICOS_PARA_DADOS")
    }
  })
  
  observe({
    if(input$ANALISE_PARA_GRAFICOS > 0){
      print("ANALISE_PARA_GRAFICOS")
      session$sendCustomMessage("myCallbackHandler", "ANALISE_PARA_GRAFICOS")
    }
  })
  
  observe({
    if(input$GRAFICOS_PARA_ANALISE > 0){
      print("GRAFICOS_PARA_ANALISE")
      session$sendCustomMessage("myCallbackHandler", "GRAFICOS_PARA_ANALISE")
    }
  })
  
  ################################
  ###### Leitura de arquivo    ###s
  ################################
  
  observe({
    
    if(input$conj=="csv"){
      # busca dados no csv
      req(input$file1)
      # ao ler arquivos separados por ponto-e-virgula,
      # ter um separador de virgula causa `read.csv` erro
      tryCatch(
        {
          df <- read.csv(input$file1$datapath,
                         sep = input$sep,
                         dec = input$dec,
                         fileEncoding="UTF-8-BOM")
        },
        error = function(e) {
          # devolve o erro
          stop(safeError(e))
        }
      )
      
    }
    
    if(input$conj=="colar"){
      # busca dados no csv
      req(input$AREA_DADOS)
      # ao ler arquivos separados por ponto-e-virgula,
      # ter um separador de virgula causa `read.csv` erro
      tryCatch(
        {
          df <- fread(input$AREA_DADOS, sep = input$sep, header = T, dec = input$dec)
          df <- data.frame(df)
        },
        error = function(e) {
          # devolve o erro
          stop(safeError(e))
        }
      )
      
    }
    
    if(input$conj=="xlsx"){
      # busca dados no csv
      req(input$file1)
      tryCatch(
        {
          df <- read.xlsx(input$file1$datapath, 1)
        },
        error = function(e) {
          # devolve o erro
          stop(safeError(e))
        }
      )
      
    }
    vals$x <- df
  })
  ################################################
  ############# Visualizando tabela de dados  ####
  ################################################
  
  output$tabela = DT::renderDataTable(
    vals$x,selection = 'none', rownames = FALSE, edit = TRUE)
  
  proxy = dataTableProxy('tabela')
  
  observeEvent(input$tabela_cell_edit, {
    info = input$tabela_cell_edit
    str(info)
    i = info$row
    j = info$col + 1
    v = info$value
    
    vals$x[i, j] <<- DT:::coerceValue(v, vals$x[i, j])
    replaceData(proxy, vals$x, resetPaging = FALSE, rownames = FALSE)
  })
  
  ######################################################################
  ############# Monta aqui a personalização de input de dados   ########
  ######################################################################
  
  ### Adiciona aqui os menus de seleção que precisar
  
  output$IN_MENU_SELECAO_TRATAMENTO <- renderPrint({
    
    dados <- vals$x
    
    if(!is.null(dados)){
      
      selectInput("IN_TRATAMENTO", label = "Selecione a coluna de tratamentos:",
                  choices = names(dados),
                  selected = names(dados[2]))
    }
  })
  
  output$IN_MENU_SELECAO_DATA <- renderPrint({
    dados <- vals$x
    
    if(!is.null(dados)){
      selectInput("IN_DATA", label = "Selecione a coluna referente a datas:",
                  choices = names(dados),
                  selected = names(dados[4]))
    }
  }) 
  
  output$IN_MENU_SELECAO_REP <- renderPrint({
    
    dados <- vals$x
    if(!is.null(dados)){
      
      selectInput("IN_REP", label = "Selecione a coluna referente a repetições ou blocos",
                  choices = names(dados),
                  selected = names(dados[3]))   
    }
  })
  
  output$IN_MENU_EXIBICAO_GRUPO <- renderPrint({
    dados <- vals$x
    if(!is.null(dados)){
      checkboxInput("IN_EXIBIR_GRUPO", "Analisar por diferentes grupos?", TRUE)
    }
  })
  
  output$IN_MENU_SELECAO_GRUPO <- renderPrint({
    dados <- vals$x
    if(!is.null(dados)){
      
      conditionalPanel(condition = "input.IN_EXIBIR_GRUPO",
                       selectInput("IN_GRUPOS", label = "Qual a coluna de grupos: ",
                                   choices = names(dados),
                                   selected = names(dados[1])))   
    }
  })
  
  
  output$IN_MENU_VARIAVEL <- renderPrint({
    
    dados <- vals$x
    
    if(!is.null(dados)){
      selectInput("IN_VARIAVEL", label = "Selecione a variável que deseja analisar: ",
                  choices = names(dados),
                  selected = names(dados[5]))   
    }
  })
  
 
  output$RESULTADOS_TEXTO <- renderPrint({
    
    dados <- vals$x
    
    TRATAS <- dados[input$IN_TRATAMENTO]
    TRAT <- as.factor(unlist(TRATAS))
    
    REP <- dados[input$IN_REP]
    PONTO <- as.factor(unlist(REP))
    
    
  
    GRUP <- dados[input$IN_GRUPOS]
    GRUPOS <- as.factor(unlist(GRUP))
    

    DAT <- dados[input$IN_DATA]
    DATA <- as.factor(unlist(DAT))
    
    VA <- dados[input$IN_VARIAVEL]   
    VAR <- as.numeric(unlist(VA))

    DF <- data.frame(GRUPOS, TRAT, PONTO, DATA, VAR)

    
    ### Verbatimtext --  Saida todo o resultado
    cat("\n")
    cat("Resumo de análise:")
    cat("\n")
    
    cat("Análise realizada por modelos lineares generalizados considerando distribuição quasipoisson")
    
    for(i in 1:length(levels(DF$GRUPOS))){ #1
      j = 1
      l = 1
      for(j in 1:length(levels(DF$DATA))){ ##1
        SAIDA <- NULL
        MEDIAS <- NULL
        RESULT <- NULL
        letras <- NULL 
        
        BASE_FILTRO <- filter(DF, GRUPOS == levels(DF$GRUPOS)[i], DATA == levels(DF$DATA)[j])

        cat("\n")
        cat("GRUPO: ")
        print(levels(DF$GRUPOS)[i])
        cat("DATA: ")
        print(levels(DF$DATA)[j])
        cat("\n")
        cat("\n")
        
        MEDIAS <- aggregate(VAR ~ TRAT, FUN = mean, data = BASE_FILTRO)
        
        sink("Salvar.txt")
        X <- DBC.glm(BASE_FILTRO$TRAT, BASE_FILTRO$PONTO, BASE_FILTRO$VAR, glm.family = "quasipoisson")
        sink()
        
        for(l in 1:length(levels(DF$TRAT))){
          letras <- c(letras, data.frame(strsplit(X$data$letra, " "))[2,l])
        }
        
        RESULT <- data.frame(TRAT = X$data$trat, Letras = letras)
        cat("\n")
        print(merge(MEDIAS, RESULT))
        
      }
    }
    
    
    
  })
  
  
})
