#### 2020 ----------------

dados2020 = fread('https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/LEITOS/2022-03-26/esus-vepi.LeitoOcupacao_2020.csv', 
              sep=",", dec=".", quote="\"", encoding = "UTF-8")

dados2020$Safra = as.factor(year(dados2020$dataNotificacao)*100 + month(dados2020$dataNotificacao))
dados2020$Semana = as.factor(week(dados2020$dataNotificacao))
dados2020$Dia = as.factor(yday(dados2020$dataNotificacao))

var_data = c("Semana","Dia","Safra")
variavel = c("ocupacaoSuspeitoCli", "ocupacaoSuspeitoUti",
                                         "ocupacaoConfirmadoCli", "ocupacaoConfirmadoUti",
                                         "ocupacaoCovidUti", "ocupacaoCovidCli",
                                         "ocupacaoHospitalarUti", "ocupacaoHospitalarCli",
                                         "saidaSuspeitaObitos", "saidaSuspeitaAltas",
                                         "saidaConfirmadaObitos", "saidaConfirmadaAltas")

dados2020_lista = vector(mode = "list", length = length(var_data)*length(variavel))
cont_lista = 1

for(i in 1:length(var_data)){
  for(j in 1:length(variavel)){
    eval(parse(text = paste0(var_data[i],'_',variavel[j],' =
        dados2020 %>% dplyr::select(,',var_data[i],',',variavel[j],') %>% 
        dplyr::group_by(',var_data[i],') %>% 
        dplyr::summarise(Total = sum(',variavel[j],', na.rm = T)) %>% 
        as.data.frame()')))    
    
    eval(parse(text = paste0('dados2020_lista$',var_data[i],'_',variavel[j],' = ', var_data[i],'_',variavel[j])))
    cont_lista = cont_lista + 1
  }
}


dados2020_lista$dia_mais_recente = as.Date(max(dados2020$dataNotificacao),'%Y-%m-%d')

for(k in 1:length(variavel)){
  eval(parse(text = paste0('estado_',variavel[k],' = 
          dados2020 %>% dplyr::select(estado,',variavel[k],') %>% 
          dplyr::group_by(estado) %>% 
          dplyr::summarise(Total = sum(',variavel[k],', na.rm = T)) %>% 
          as.data.frame()')))
  eval(parse(text = paste0('dados2020_lista$estado','_',variavel[k],' = ', 'estado_',variavel[k])))
  
}

save(dados2020_lista,  file = 'dados2020_lista.RData')

#### 2021 ----------------

dados2021 = fread('https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/LEITOS/2022-03-26/esus-vepi.LeitoOcupacao_2021.csv', 
                  sep=",", dec=".", quote="\"", encoding = "UTF-8")

dados2021$Safra = as.factor(year(dados2021$dataNotificacao)*100 + month(dados2021$dataNotificacao))
dados2021$Semana = as.factor(week(dados2021$dataNotificacao))
dados2021$Dia = as.factor(yday(dados2021$dataNotificacao))

var_data = c("Semana","Dia","Safra")
variavel = c("ocupacaoSuspeitoCli", "ocupacaoSuspeitoUti",
             "ocupacaoConfirmadoCli", "ocupacaoConfirmadoUti",
             "ocupacaoCovidUti", "ocupacaoCovidCli",
             "ocupacaoHospitalarUti", "ocupacaoHospitalarCli",
             "saidaSuspeitaObitos", "saidaSuspeitaAltas",
             "saidaConfirmadaObitos", "saidaConfirmadaAltas")

dados2021_lista = vector(mode = "list", length = length(var_data)*length(variavel))
cont_lista = 1

for(i in 1:length(var_data)){
  for(j in 1:length(variavel)){
    eval(parse(text = paste0(var_data[i],'_',variavel[j],' =
        dados2021 %>% dplyr::select(,',var_data[i],',',variavel[j],') %>% 
        dplyr::group_by(',var_data[i],') %>% 
        dplyr::summarise(Total = sum(',variavel[j],', na.rm = T)) %>% 
        as.data.frame()')))    
    
    eval(parse(text = paste0('dados2021_lista$',var_data[i],'_',variavel[j],' = ', var_data[i],'_',variavel[j])))
    cont_lista = cont_lista + 1
  }
}


dados2021_lista$dia_mais_recente = as.Date(max(dados2021$dataNotificacao),'%Y-%m-%d')


for(k in 1:length(variavel)){
  eval(parse(text = paste0('estado_',variavel[k],' = 
          dados2021 %>% dplyr::select(estado,',variavel[k],') %>% 
          dplyr::group_by(estado) %>% 
          dplyr::summarise(Total = sum(',variavel[k],', na.rm = T)) %>% 
          as.data.frame()')))
  eval(parse(text = paste0('dados2021_lista$estado','_',variavel[k],' = ', 'estado_',variavel[k])))
  
}

save(dados2021_lista,  file = 'dados2021_lista.RData')

#### 2022 ----------------

dados2022 = fread('https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/LEITOS/2022-03-26/esus-vepi.LeitoOcupacao_2022.csv', 
                  sep=",", dec=".", quote="\"", encoding = "UTF-8")

dados2022$Safra = as.factor(year(dados2022$dataNotificacao)*100 + month(dados2022$dataNotificacao))
dados2022$Semana = as.factor(week(dados2022$dataNotificacao))
dados2022$Dia = as.factor(yday(dados2022$dataNotificacao))

var_data = c("Semana","Dia","Safra")
variavel = c("ocupacaoSuspeitoCli", "ocupacaoSuspeitoUti",
             "ocupacaoConfirmadoCli", "ocupacaoConfirmadoUti",
             "ocupacaoCovidUti", "ocupacaoCovidCli",
             "ocupacaoHospitalarUti", "ocupacaoHospitalarCli",
             "saidaSuspeitaObitos", "saidaSuspeitaAltas",
             "saidaConfirmadaObitos", "saidaConfirmadaAltas")

dados2022_lista = vector(mode = "list", length = length(var_data)*length(variavel))
cont_lista = 1

for(i in 1:length(var_data)){
  for(j in 1:length(variavel)){
    eval(parse(text = paste0(var_data[i],'_',variavel[j],' =
        dados2022 %>% dplyr::select(,',var_data[i],',',variavel[j],') %>% 
        dplyr::group_by(',var_data[i],') %>% 
        dplyr::summarise(Total = sum(',variavel[j],', na.rm = T)) %>% 
        as.data.frame()')))    
    
    eval(parse(text = paste0('dados2022_lista$',var_data[i],'_',variavel[j],' = ', var_data[i],'_',variavel[j])))
    cont_lista = cont_lista + 1
  }
}


dados2022_lista$dia_mais_recente = as.Date(max(dados2022$dataNotificacao),'%Y-%m-%d')

dados2022_lista$dia_mais_recente = as.Date(max(dados2022$dataNotificacao),'%Y-%m-%d')

for(k in 1:length(variavel)){
  eval(parse(text = paste0('estado_',variavel[k],' = 
          dados2022 %>% dplyr::select(estado,',variavel[k],') %>% 
          dplyr::group_by(estado) %>% 
          dplyr::summarise(Total = sum(',variavel[k],', na.rm = T)) %>% 
          as.data.frame()')))
  eval(parse(text = paste0('dados2022_lista$estado','_',variavel[k],' = ', 'estado_',variavel[k])))
  
}

save(dados2022_lista,  file = 'dados2022_lista.RData')
