library(shiny)
library(readr)
library(ggplot2)
library(data.table)
library(dplyr)
library(bs4Dash)
library(shinybusy)
library(readr)
library(fpp3)
library(fpp2)
library(forecast)
library(fable)
library(dplyr)
library(tsibble)
library(ggplot2)
library(gridExtra)
library(randtests)
library(xts)
library(zoo)
library(TSA)
library(astsa)
options(scipen = 9999)

#SAIDASUSPEITALTAS
#SAIDASUSPEITAOBITOS
#OCUPACAOCOVIDCLI
#OCUPACAOCOVIDUTI


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
            uiOutput("escolhe_info"), box(width = 12,plotOutput("teste"), box(width = 12,plotOutput("serie_temporal")))
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
    remove_modal_gif()
    
    dataset$dados = dataset$dados %>% select(dataNotificacao, ocupacaoSuspeitoCli, ocupacaoSuspeitoUti,
                            ocupacaoConfirmadoCli, ocupacaoConfirmadoUti,
                            ocupacaoCovidUti, ocupacaoCovidCli,
                            ocupacaoHospitalarUti, ocupacaoHospitalarCli,
                            saidaSuspeitaObitos, saidaSuspeitaAltas,
                            saidaConfirmadaObitos, saidaConfirmadaAltas,
                            estado, estadoNotificacao, municipio, municipioNotificacao)
    
    dataset$dados$Safra = as.factor(year(dataset$dados$dataNotificacao)*100 + month(dataset$dados$dataNotificacao))
    dataset$dados$Semana = as.factor(year(dataset$dados$dataNotificacao)*10000 + month(dataset$dados$dataNotificacao)*100 + 
    week(dataset$dados$dataNotificacao))
    dataset$dados$Dia = as.factor(yday(dataset$dados$dataNotificacao))
    
    output$escolhe_info = renderUI(
      tagList(
        box(width = 12,
            column(6, selectizeInput("data_tipo",
                       "Selecione a formatação da Data:",
                       choices = c("Por Mês",
                                   "Por Semana", 'Por Dia'))),
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
      else if(input$data_tipo == "Por Dia") var_data='Dia'
      else var_data = "Safra"

      variavel = input$info
      
      eval(parse(text = paste0('Y = dataset$dados %>% dplyr::select(',var_data,',',variavel,') %>% 
        dplyr::group_by(',var_data,') %>% 
        dplyr::summarise(Total = sum(',variavel,', na.rm = T)) %>% 
        as.data.frame()')))
       
      maiusculas_eixo_y = regmatches(variavel, gregexpr("[A-Z]", variavel))
      
      eixo_y = variavel
      
      unicos_maiusculas = unique(maiusculas_eixo_y[[1]])
      
      for(i in 1:length(unicos_maiusculas)){
        eixo_y = gsub(unicos_maiusculas[i], paste(' ',unicos_maiusculas[i]), eixo_y)
      }
      
      eixo_y = sub(strsplit(eixo_y,'')[[1]][1], toupper(strsplit(eixo_y,'')[[1]][1]), eixo_y)
      
      if(input$data_tipo == "Por Dia")
      eval(parse(text = paste0('total_algo_safra = ggplot(Y) +
        geom_line(size = 1, aes(x = ',var_data,', y = Total, group = 1), col = "springgreen3") +
        theme_minimal() +
        theme(axis.text.x = element_blank()) + 
        xlab("Tempo") +
        ylab(eixo_y)
      ')))
      else 
        eval(parse(text = paste0('total_algo_safra = ggplot(Y) +
        geom_line(size = 1, aes(x = ',var_data,', y = Total, group = 1), col = "springgreen3") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=90,hjust=1)) + 
        xlab("Tempo") +
        ylab(eixo_y)
      ')))
      
      if(input$data_tipo == "Por Dia")
      eval(parse(text = paste0("serie <- ts(Y$Total, start = 1, frequency=7)")))
      else if (input$data_tipo == "Por semana")
      eval(parse(text = paste0("serie <- ts(Y$Total, start = 1, frequency=30)")))
      else 
      eval(parse(text = paste0("serie <- ts(Y$Total, start = 1, frequency=1)"))) 
      
      fit = auto.arima(serie)
      
      serie_temporal <- 
        autoplot(forecast(fit, level=0.9)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=90,hjust=1), legend.position = 'bottom') + 
        xlab("Tempo") +
        ylab(eixo_y)
        
      
      
      output$teste = renderPlot(total_algo_safra)
      output$serie_temporal = renderPlot(serie_temporal)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
