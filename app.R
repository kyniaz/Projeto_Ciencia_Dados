library(shiny)
library(readr)
library(ggplot2)
library(data.table)
library(dplyr)
library(bs4Dash)
library(shinybusy)
options(scipen = 9999)

#Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Histórico Hospitalizações"),
  dashboardSidebar(
            selectizeInput("ano",
                        "Selecione o ano:",
                        choices = c("2022","2021","2020"),
            )
        ),
        dashboardBody(
            uiOutput("escolhe_info"), box(width = 12,plotOutput("teste"))
        )
  )

dataset = reactiveValues()


# Define server logic required to draw a histogram
server <- function(input, output) {

  observeEvent(input$ano, {
    
    show_modal_gif(
      src = "https://c.tenor.com/7GfS_1bH1FAAAAAC/lux-lol.gif",
      width = "250px", height = "250px",
      modal_size = "s",
      text = "Fazendo o download da base..."
    )
    
    ## Upa database do ano e joga no environment
    if(input$ano == '2020'){
      dataset$dados = fread('https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/LEITOS/2022-03-26/esus-vepi.LeitoOcupacao_2020.csv', 
                    sep=",", dec=".", quote="\"", encoding = "UTF-8")
    }
    else if(input$ano == '2021'){
      dataset$dados = fread('https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/LEITOS/2022-03-26/esus-vepi.LeitoOcupacao_2021.csv', 
                    sep=",", dec=".", quote="\"", encoding = "UTF-8")
    }
    else {
      dataset$dados = fread('https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/LEITOS/2022-03-26/esus-vepi.LeitoOcupacao_2022.csv', 
                    sep=",", dec=".", quote="\"", encoding = "UTF-8")
    }
    
    dataset$dados = dataset$dados |> select(dataNotificacao, ocupacaoSuspeitoCli, ocupacaoSuspeitoUti,
                            ocupacaoConfirmadoCli, ocupacaoConfirmadoUti,
                            ocupacaoCovidUti, ocupacaoCovidCli,
                            ocupacaoHospitalarUti, ocupacaoHospitalarCli,
                            saidaSuspeitaObitos, saidaSuspeitaAltas,
                            saidaConfirmadaObitos, saidaConfirmadaAltas,
                            estado, estadoNotificacao, municipio, municipioNotificacao)
    
    dataset$dados$Safra = as.factor(year(dataset$dados$dataNotificacao)*100 + month(dataset$dados$dataNotificacao))
    dataset$dados$Semana = as.factor(year(dataset$dados$dataNotificacao)*10000 + month(dataset$dados$dataNotificacao)*100 + 
    week(dataset$dados$dataNotificacao))
    
    output$escolhe_info = renderUI(
      tagList(
        box(width = 12,
            column(6, selectizeInput("data_tipo",
                       "Selecione a formatação da Data:",
                       choices = c("Por Mês",
                                   "Por Semana"))),
            column(6, selectizeInput("info",
                       "Selecione a informação:",
                       choices = c("ocupacaoSuspeitoCli",
                                   "ocupacaoSuspeitoUti",
                                   "ocupacaoConfirmadoCli",
                                   "ocupacaoConfirmadoUti",
                                   "ocupacaoCovidUti",
                                   "ocupacaoCovidCli",
                                   "ocupacaoHospitalarUti",
                                   "ocupacaoHospitalarCli",
                                   "saidaSuspeitaObitos",
                                   "saidaSuspeitaAltas",
                                   "saidaConfirmadaObitos",
                                   "saidaConfirmadaAltas"))))
        
      )
    )
  })
  
  eventos = reactive({
    list(input$data_tipo, input$info)
  })
  
  observeEvent(eventos(), {
    if(!is.null(input$info) & !is.null(input$data_tipo)){

      if(input$data_tipo == "Por Semana") var_data = "Semana"
      else var_data = "Safra"

      variavel = input$info
      
      eval(parse(text = paste0('Y = dataset$dados |> dplyr::select(',var_data,',',variavel,') |> 
        dplyr::group_by(',var_data,') |> 
        dplyr::summarise(Total = sum(',variavel,', na.rm = T)) |> 
        as.data.frame()')))
      
      maiusculas_eixo_y = regmatches(variavel, gregexpr("[A-Z]", variavel))
      
      eixo_y = variavel
      
      for(i in 1:length(maiusculas_eixo_y[[1]])){
        eixo_y = sub(maiusculas_eixo_y[[1]][i], paste(' ',maiusculas_eixo_y[[1]][i]), eixo_y)
      }
      
      eixo_y = gsub(strsplit(eixo_y,'')[[1]][1], toupper(strsplit(eixo_y,'')[[1]][1]), eixo_y)
      
      eval(parse(text = paste0('total_algo_safra = ggplot(Y) +
        geom_line(size = 1, aes(x = ',var_data,', y = Total, group = 1), col = "springgreen3") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=90, hjust=1)) + 
        xlab("Tempo") +
        ylab(eixo_y)
      ')))
      remove_modal_gif()
      
      output$teste = renderPlot(total_algo_safra)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
