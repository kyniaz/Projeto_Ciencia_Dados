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
library(tmap)
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
              tmapOutput("mapa")
            )
          )
        )
  )

dataset = reactiveValues()


#Servidor
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
    dataset$dados$Semana = as.factor(week(dataset$dados$dataNotificacao))
    dataset$dados$Dia = as.factor(yday(dataset$dados$dataNotificacao))
    
  })
  
  eventos = reactive({
    list(input$data_tipo, input$info, input$previsao_prazo)
  })
  
  observeEvent(eventos(), {
    if(!is.null(input$info) & !is.null(input$data_tipo)){

      if(input$data_tipo == "Por Semana") var_data = "Semana"
      else if(input$data_tipo == "Por Dia") var_data = "Dia"
      else var_data = "Safra"

      variavel = input$info
      
      ###Formatação por data (dia, mes ou semana) ----
      eval(parse(text = paste0('Y = dataset$dados %>% dplyr::select(,',var_data,',',variavel,') %>% 
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
      
      # if(input$data_tipo == "Por Dia")
      
      dia_mais_recente = as.Date(max(dataset$dados$dataNotificacao),'%Y-%m-%d')
      
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
  
observeEvent(input$info, {
  ###Formação por Mapa - apenas estados ----
  variavel = input$info
  
  #Por estado
  eval(parse(text = paste0('Y_estado = dataset$dados %>% dplyr::select(estado,',variavel,') %>% 
        dplyr::group_by(estado) %>% 
        dplyr::summarise(Total = sum(',variavel,', na.rm = T)) %>% 
        as.data.frame()')))
  
  
  brasil_estados = read_state(code_state = "all", year = 2020)
  
  tmap_mode("plot")
  
  estados = read.csv('pop_por_estado2020.csv', header = T)
  
  estados$abbrev_state = c("AC", "AL", "AP","AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS",
                           "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO",
                           "RR", "SC", "SP", "SE", "TO")
  
  brasil_estados$name_state = ifelse(brasil_estados$name_state == 'Amazônas','Amazonas',brasil_estados$name_state)
  
  brasil_estados$name_state = toupper(trimws(brasil_estados$name_state)) 
  
  Y_estado$estado = toupper(trimws(unique(Y_estado$estado)))
  
  brasil_estados = left_join(brasil_estados, estados, by = ('abbrev_state' = 'abbrev_state'))
  brasil_estados = left_join(brasil_estados, Y_estado, by = c('name_state' = 'estado'))
  
  brasil_estados$Taxa = (brasil_estados$Total/brasil_estados$Populacao) * 100000 
  mapa = tm_shape(brasil_estados) +
    tm_polygons("Taxa") + 
    tm_text("abbrev_state", scale=1)  +
    tm_layout("Taxa por 100.000",
              legend.title.size = 1,
              legend.text.size = 0.6,
              legend.position = c("left","bottom"),
              legend.bg.color = "white",
              legend.bg.alpha = 1)
  
  output$mapa = renderTmap(mapa)
})

}

# Run the application 
shinyApp(ui = ui, server = server)
