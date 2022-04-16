library(shiny)
library(readr)
library(ggplot2)
library(data.table)
library(dplyr)
library(bs4Dash)
library(shinybusy)
library(forecast)
library(zoo)
library(plotly)
library(leaflet)
#remotes::install_github("rpradosiqueira/brazilmaps")
library(brazilmaps)
#devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
library(geobr)

options(scipen = 9999)

#UI
ui <- dashboardPage(
  dashboardHeader(title = "Histórico Hospitalizações"),
  dashboardSidebar(
            selectizeInput("ano",
                        "Selecione o ano:",
                        choices = c("2022","2021","2020"),
            ),
            selectizeInput("data_tipo",
                           "Selecione a formatação da Data:",
                           choices = c("Por Mês",
                                       "Por Semana", 'Por Dia')),
            selectizeInput("info",
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
                                       "saidaConfirmadaAltas")),
            sliderInput("previsao_prazo",
                        "Selecione a previsão(apropriado para 2022 nas visões de Dia e Semana):",
                          min = 1, max = 30, value = 1)
        ,skin = 'light'),
        dashboardBody(
          tabsetPanel(
            id = "tabcard",
            tabPanel(
              title = "Informação Temporal", 
              plotlyOutput("serie_temporal")),
            tabPanel(
              title = "Informação Espacial",
              #tmapOutput("mapa")
              leafletOutput("mapa")
            )
          )
        )
  )

load('brasil_estados.RData')
dataset = reactiveValues()
dataset$brasil_estados = brasil_estados

#Servidor
server <- function(input, output) {

  observeEvent(input$ano, {
    
    ## Upa database do ano e joga no environment
    if(input$ano == '2020'){
      load('dados2020_lista.RData')
      dataset$dados = NULL
      dataset$dados = dados2020_lista
    }
    else if(input$ano == '2021'){
      load('dados2021_lista.RData')
      dataset$dados = NULL
      dataset$dados = dados2021_lista
    }
    else {
      load('dados2022_lista.RData')
      dataset$dados = NULL
      dataset$dados = dados2022_lista
    }
    
    #remove_modal_gif()
    
  })
  
  eventos = reactive({
    list(input$ano, input$data_tipo, input$info, input$previsao_prazo)
  })
  
  observeEvent(eventos(), {
    if(!is.null(input$info) & !is.null(input$data_tipo)){

      if(input$data_tipo == "Por Semana") var_data = "Semana"
      else if(input$data_tipo == "Por Dia") var_data = "Dia"
      else var_data = "Safra"

      variavel = input$info
      
      ###Formatação por data (dia, mes ou semana) ----
      eval(parse(text = paste0('Y = dataset$dados$',paste0(var_data,'_',variavel))))
  
      maiusculas_eixo_y = regmatches(variavel, gregexpr("[A-Z]", variavel))
      
      eixo_y = variavel
      
      unicos_maiusculas = unique(maiusculas_eixo_y[[1]])
      
      for(i in 1:length(unicos_maiusculas)){
        eixo_y = gsub(unicos_maiusculas[i], paste(' ',unicos_maiusculas[i]), eixo_y)
      }
      
      eixo_y = sub(strsplit(eixo_y,'')[[1]][1], toupper(strsplit(eixo_y,'')[[1]][1]), eixo_y)
      
      # if(input$data_tipo == "Por Dia")
      
      dia_mais_recente = dataset$dados$dia_mais_recente
      
      if(input$data_tipo == "Por Dia"){
        prev_prazo = input$previsao_prazo
        if( input$ano == '2022')
          eval(parse(text = paste0("serie = zoo(Y$Total, seq(from = as.Date('",input$ano,"-01-01'), to = dia_mais_recente, by = 1))")))
        else{
          eval(parse(text = paste0("serie = zoo(Y$Total, seq(from = as.Date('",input$ano,"-01-01'), to = as.Date('",input$ano,"-12-31'), by = 1))")))
        }
      }
      else if (input$data_tipo == "Por Semana"){
        prev_prazo = input$previsao_prazo
        if( input$ano == '2022')
          eval(parse(text = paste0("serie = zoo(Y$Total, seq(from = as.Date('",input$ano,"-01-01'), to = dia_mais_recente, by = 7))")))
        else{
          eval(parse(text = paste0("serie = zoo(Y$Total, seq(from = as.Date('",input$ano,"-01-01'), to = as.Date('",input$ano,"-12-31'), by = 7))")))
        }
      }
      else {
        prev_prazo = 1
        if( input$ano == '2022')
          eval(parse(text = paste0("serie = zoo(Y$Total, seq(from = as.Date('",input$ano,"-01-01'), to = dia_mais_recente, by = 30))")))
        else{
          eval(parse(text = paste0("serie = zoo(Y$Total, seq(from = as.Date('",input$ano,"-01-01'), to = as.Date('",input$ano,"-12-31'), by = 30))")))
        }
      }

      fit = auto.arima(serie)
      
      if(input$ano == '2022'){

        predicao = forecast(fit, h = prev_prazo)
        datas_predicao = row.names(print(predicao))
        datas = as.Date(row.names(data.frame(serie)))
        
        serie_temporal <- 
          ggplot() +
          geom_line(aes(x = datas, y = Y$Total, colour = "Dados observados")) +
          geom_line(aes(x = datas, y = fit$fitted, colour = "Modelo SARIMA")) + 
          geom_line(aes(x = as.Date(as.numeric(datas_predicao)), y = predicao$mean, colour = "Predição")) +
          geom_ribbon(aes(x = as.Date(as.numeric(datas_predicao)), 
                        ymin = predicao$lower[,2], ymax = predicao$upper[,2], fill = "Intervalo"), alpha = 0.5) +
          theme_minimal() +
          theme(axis.text.x  = element_text(angle=90,hjust=1), legend.position = 'bottom') + 
          xlab("Tempo") +
          scale_colour_manual(name = "Cor:", values = c("royalblue","springgreen4","red2"), 
                              labels = c("Observado","Modelo ARIMA ajustado","Previsão")) +
          scale_fill_manual(name = NULL, values = "red2", labels = "Predição (95%)") +
          ylab(eixo_y)        
      }
      else {
        serie_temporal <- 
          autoplot(serie) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle=90,hjust=1), legend.position = 'bottom') + 
          xlab("Tempo") +
          ylab(eixo_y)
      }
      
      output$serie_temporal = renderPlotly(ggplotly(serie_temporal) %>%
                                           layout(legend = list(orientation = "h", x = 0.1, y = -0.2)))
    }
  })
  
eventos_mapa = reactive({
  list(input$ano, input$info)
})
  
observeEvent(eventos_mapa(), {
  ###Formação por Mapa - apenas estados ----
  output$mapa = NULL
  
  variavel = input$info
  
  #Por estado
  eval(parse(text = paste0('Y_estado = dataset$dados$',paste0('estado_',variavel))))
  
  
  #dataset$brasil_estados = read_state(code_state = "all", year = 2020)
  
  estados = read.csv('pop_por_estado2020.csv', header = T)
  
  estados$abbrev_state = c("AC", "AL", "AP","AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS",
                           "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO",
                           "RR", "SC", "SP", "SE", "TO")
  
  dataset$brasil_estados = dataset$brasil_estados[,c(1:6)]
  
  dataset$brasil_estados$name_state = ifelse(dataset$brasil_estados$name_state == 'Amazônas','Amazonas',dataset$brasil_estados$name_state)
  
  dataset$brasil_estados$name_state = toupper(trimws(dataset$brasil_estados$name_state)) 
  
  Y_estado$estado = toupper(trimws(unique(Y_estado$estado)))
  
  dataset$brasil_estados = left_join(dataset$brasil_estados, estados, by = ('abbrev_state' = 'abbrev_state'))
  dataset$brasil_estados = left_join(dataset$brasil_estados, Y_estado, by = c('name_state' = 'estado'))
  
  dataset$brasil_estados$Taxa = (dataset$brasil_estados$Total/dataset$brasil_estados$Populacao) * 100000 
  
  reds = colorNumeric("Reds", domain = dataset$brasil_estados$Taxa)
  
  mapa = leaflet(data = dataset$brasil_estados)
  mapa = mapa %>% 
    addPolygons(weight = 0.1, fillColor = ~reds(Taxa),
                color = "green",fillOpacity = 0.9,
                smoothFactor = 0.5,
                popup = paste0(dataset$brasil_estados$name_state,":  ",
                               dataset$brasil_estados$Taxa)) %>%
    addLegend(position = "bottomright", pal = reds, values = ~Taxa)
  
  output$mapa = renderLeaflet(mapa)
})

}

# Run the application 
shinyApp(ui = ui, server = server)
