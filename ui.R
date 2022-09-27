
library(shiny)
library(shinythemes)
library(xlsx)
library(tidyverse)
library(DT)
library(shinycssloaders)

#################################################
#### Modelo base para relatório com input de dados ###
##################################################

ui <-navbarPage("Relatório Biológicos", windowTitle = 'FMT Análises', collapsible = TRUE, theme = shinytheme("paper"),
                
                tabPanel
                (
                  includeCSS("palatino.css"),
                  
                  " ",
                  sidebarLayout
                  (
                    sidebarPanel(
                      
                      tags$hr(),
                      
                      ######################################################
                      #              scripts dos botoes avancar            #
                      ######################################################
                      (tags$head(tags$script('
                                           Shiny.addCustomMessageHandler("myCallbackHandler",
                                           function(typeMessage) {console.log(typeMessage)
                                           if(typeMessage == "ACT_CONJUNTO_TO_BD"){
                                           $("a:contains(BD)").click();
                                           }
                                           
                                          if(typeMessage == "DADOS_PARA_RESULTADOS"){
                                           $("a:contains(ANALISE)").click();
                                           }
                                           
                                          if(typeMessage == "DADOS_PARA_INICIO"){
                                           $("a:contains(INICIO)").click();
                                          }
                                           
                                          if(typeMessage == "ANALISE_PARA_DADOS"){
                                           $("a:contains(BD)").click();
                                           }
                                           
                                         if(typeMessage == "ANALISE_PARA_GRAFICOS"){
                                           $("a:contains(GRAFICOS)").click();
                                           }
                                           
                                           
                                          if(typeMessage == "GRAFICOS_PARA_ANALISE"){
                                           $("a:contains(ANALISE)").click();
                                          }
                                           
                                          if(typeMessage == "GRAFICOS_PARA_DADOS"){
                                           $("a:contains(BD)").click();
                                           }
                                           
                                           });
                                           '))
                      ),
                      
                      radioButtons("conj", "O Conjunto de dados analisados será obtido via:",
                                   choices = c('Importação de arquivo csv [Recomendado]' = "csv",
                                               'Importação de arquivo xlsx' = 'xlsx',
                                               'colar' = 'colar'),
                                   selected = "csv"),
                      
                      # Linha horizontal ----
                      tags$hr(),
                      
                      
                      ######################################################
                      #     botao avancar e seus alinhamentos em html      #
                      ######################################################
                      tags$div(align="center", border="0",
                               
                               actionButton("ACT_CONJUNTO_TO_BD", label = "Avançar", class="btn btn-success", icon= icon("angle-right"), width="100")
                      ),
                      
                      conditionalPanel(condition = "input.conj == 'colar'",
                                       textAreaInput("AREA_DADOS", "Copie e cole abaixo sua base de dados", width = 500, height = 500),
                      ),
                      
                      tags$head(tags$style(HTML('
                          #AREA_DADOS{
                           background-color: #cee6cf !important;
                           border: none;
                           }')))
                    ),
                    mainPanel(tags$div(align="center", valign="top", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='560'), br()),
                              br()
                    )
                  )
                ),
                navbarMenu("Banco de Dados", icon=icon("arrow-up"),
                           tabPanel
                           (
                             "BD", icon=icon("arrow-up"),
                             sidebarLayout
                             (
                               sidebarPanel
                               
                               ######################################################
                               #              scripts dos botoes avancar            #
                               ######################################################
                               (
                                 
                                 # Input: Seleciona o arquivo ----
                                 fileInput("file1", "Escolha o arquivo: ",
                                           multiple = FALSE,
                                           buttonLabel = "Procurar...",
                                           placeholder = "Dados não selecionados"),
                                 
                                 # Input: Seleciona o separador ----
                                 radioButtons("sep", "Separador de valores",
                                              choices = c('Virgula [Arquivo csv separado por virgula' = ",",
                                                          'Ponto e virgula [Normalmente arquivos .csv]' = ";",
                                                          'Tabulação [Normalmente quando você copia e cola]' = "\t"),
                                              selected = ";"),
                                 
                                 # Input: Select  ----
                                 radioButtons("dec", "Separador Decimal",
                                              choices = c('Ponto' = ".",
                                                          "Virgula" = ","),
                                              selected = ','),
                                 
                                 ### Exibições personalizadas
                                 htmlOutput("IN_MENU_EXIBICAO_GRUPO"),
                                 htmlOutput("IN_MENU_SELECAO_GRUPO"),
                                 htmlOutput("IN_MENU_SELECAO_TRATAMENTO"),
                                 htmlOutput("IN_MENU_SELECAO_REP"),
                                 htmlOutput("IN_MENU_SELECAO_DATA"),
                                 htmlOutput("IN_MENU_VARIAVEL"),
                                 
                                 
                                 
                                 ######################################################
                                 #     botao avancar e seus alinhamentos em html      #
                                 ######################################################
                                 
                                 tags$div(align="center",  border="0",
                                          actionButton("DADOS_PARA_INICIO", label = "< Voltar", class="btn btn-success", width="100"),
                                          actionButton("DADOS_PARA_RESULTADOS", label = "Avançar >", class="btn btn-success", width="100")
                                 ),
                                 
                                 br()
                                 
                               ),
                               
                               mainPanel(
                                 tabsetPanel(
                                   tabPanel(
                                     DT::dataTableOutput("tabela")
                                   )
                                 )
                               )
                               
                             ),
                             # FOOTER CREDITOS
                             hr(),
                             tags$div(align="center", valign="bottom", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                             tags$div( align="center",
                                       HTML("Relatório Personalizado - 2022")
                             ),
                             br()
                             
                           )
                ),
                
                navbarMenu("RESULTADOS", icon=icon("bars"),
                           tabPanel
                           (
                             "ANALISE", icon=icon("edit"),
                             sidebarLayout
                             (
                               sidebarPanel
                               
                               ######################################################
                               #              scripts dos botoes avancar            #
                               ######################################################
                               (
                                 
                                 ######################################################
                                 #     botao avancar e seus alinhamentos em html      #
                                 ######################################################
                                 
                                 tags$div(align="center",  border="0",
                                          actionButton("ANALISE_PARA_DADOS", label = "< Voltar", class="btn btn-success", width="100"),
                                          actionButton("ANALISE_PARA_GRAFICOS", label = "Gráficos >", class="btn btn-success", width="100")
                                 ),
                                 
                                 br()
                                 
                               ),
                               mainPanel(
                                 
                                 withSpinner(verbatimTextOutput("RESULTADOS_TEXTO"))
                               )
                               
                               
                             ),
                             # FOOTER CREDITOS
                             hr(),
                             tags$div(align="center", valign="bottom", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                             tags$div( align="center",
                                       HTML("Relatório Personalizado - 2022")
                             ),
                             br()
                             
                           ),
                           
                           
                           tabPanel
                           (
                             "GRAFICOS", icon=icon("edit"),
                             sidebarLayout
                             (
                               sidebarPanel
                               
                               ######################################################
                               #              scripts dos botoes avancar            #
                               ######################################################
                               (
                                 
                                 ######################################################
                                 #     botao avancar e seus alinhamentos em html      #
                                 ######################################################
                                 
                                 tags$div(align="center",  border="0",
                                          actionButton("GRAFICOS_PARA_DADOS", label = "< Dados", class="btn btn-success", width="100"),
                                          actionButton("GRAFICOS_PARA_ANALISE", label = " < Analise >", class="btn btn-success", width="100")
                                 ),
                                 
                                 br()
                                 
                               ),
                               mainPanel()
                               
                               
                             ),
                             # FOOTER CREDITOS
                             hr(),
                             tags$div(align="center", valign="bottom", img(src='https://raw.githubusercontent.com/PaulohSouza/imagens/main/logo.png', width='160'), br()),
                             tags$div( align="center",
                                       HTML("Relatório Personalizado - 2022")
                             ),
                             br()
                             
                           )
                )
)
