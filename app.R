library(shiny)
library(readr)
library(ggplot2)
library(data.table)
library(dplyr)
library(bs4Dash)
library(shinybusy)
library(forecast)
library(zoo)
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
            uiOutput("escolhe_info"), #box(width = 12,plotOutput("teste"), 
            box(width = 12,plotOutput("serie_temporal"))
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
    dataset$dados$Semana = as.factor(week(dataset$dados$dataNotificacao))
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
                                   "saidaConfirmadaAltas"))),
            column(6, sliderInput("previsao_prazo",
                                  "Selecione a previsão(para 2022 nas visões de Dia e Semana):",
                                  min = 1, max = 60, value = 1))
            )
        
      )
    )
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
      
      # if(input$data_tipo == "Por Dia")
      # eval(parse(text = paste0('total_algo_safra = ggplot(Y) +
      #   geom_line(size = 1, aes(x = ',var_data,', y = Total, group = 1), col = "springgreen3") +
      #   theme_minimal() +
      #   theme(axis.text.x = element_blank()) + 
      #   xlab("Tempo") +
      #   ylab(eixo_y)
      # ')))
      # else 
      #   eval(parse(text = paste0('total_algo_safra = ggplot(Y) +
      #   geom_line(size = 1, aes(x = ',var_data,', y = Total, group = 1), col = "springgreen3") +
      #   theme_minimal() +
      #   theme(axis.text.x = element_text(angle=90,hjust=1)) + 
      #   xlab("Tempo") +
      #   ylab(eixo_y)
      # ')))
      
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
          geom_line(aes(x = datas, y = Y$Total, colour = "a")) +
          geom_line(aes(x = datas, y = fit$fitted, colour = "b")) + 
          geom_line(aes(x = as.Date(as.numeric(datas_predicao)), y = predicao$mean, colour = "c")) +
          geom_ribbon(aes(x = as.Date(as.numeric(datas_predicao)), 
                        ymin = predicao$lower[,2], ymax = predicao$upper[,2], fill = "a"), alpha = 0.5) +
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
      
      #output$teste = renderPlot(total_algo_safra)
      output$serie_temporal = renderPlot(serie_temporal)
    }
  }, ignoreInit = T)
}

# Run the application 
shinyApp(ui = ui, server = server)
