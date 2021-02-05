

# Estructura de datos

estructurar_datos_st <- function(Movimiento,reportMov =T){
# crear un ID para separar las horas de datos basado en la posicion de la fecha
# de acuerdo al número de caracteres
  
  nrdc <- nrow(Movimiento)
  
  Movimiento <- data.table(Dato = Movimiento[,1],
                                position = row.names(Movimiento))
  
  nc    <- nchar(Movimiento$Dato.V1)

  serie <- array(1,nrow(Movimiento))
  
  Tipo <- Movimiento$Dato.V1 %>% map_int(.f =function(x){sum(grep(':',x))})
  
  aum   <- 0

  # Potencial para optimizar
  
  for(i in 2:nrow(Movimiento)){
  
    if(Tipo[i-1]>0){
      aum <- aum + 1
      } 
    serie[i] <- serie[i] + aum
  
  }

  Movimiento$serie <- serie
  Movimiento$Tipo <- Tipo
  
  # Colocar la fecha como una de las columnas

  Fecha <- Movimiento %>% dplyr::filter(Tipo == 1) %>% select(Dato.V1,serie) %>% 
          tbl_df
    
  names(Fecha)[1] <- 'Fecha'
  
  Registros <- Movimiento %>% dplyr::filter(Tipo==0) %>% select(-Tipo) %>% tbl_df 
  
  Registros <- Registros %>% right_join(Fecha,by="serie")
  
  # Separar la notaciones de los datos


    Registros <- Registros %>% separate(Dato.V1,c('x','y','z')," ")
    Registros <- Registros %>% mutate(x=as.numeric(x),y=as.numeric(y),z=as.numeric(z))
    Registros <- Registros %>% mutate(Cuadrado = as.numeric(x)^2+as.numeric(y)^2+as.numeric(z)^2)
    
      if(reportMov){
        Registros <- Registros %>% separate(Fecha,c('Fecha','Hora','Num_movimiento','Movimiento'),' ')
      }else{
        Registros <- Registros %>% separate(Fecha,c('Fecha','Hora'),' ')
      }
    
    # Acomodar fecha
    
    
    Registros$Fecha <- as.Date(Registros$Fecha,"%d.%m.%Y")
    
    Registros$completTime <- as.POSIXct(paste(Registros$Fecha,Registros$Hora),format = "%Y-%m-%d %H:%M:%S")
    
    Registros
  }
  



# Estimar movimiento

estimar_actividad <- function(registros,Fac_R1 =0.95, Fac_R2 = 1.05, Fac_C0 = -0.25){
  
  #Cuadrado 
  registros <- registros %>% mutate(Cuadrado = as.numeric(X)^2+as.numeric(Y)^2+as.numeric(Z)^2)
  #
   registros <-
   registros %>% mutate(
     actividad = as.factor(map_chr(.x= 1:nrow(registros) ,.f= function(w){
  
       line <- registros[w,]
  
       if(line$Cuadrado>Fac_R1 & line$Cuadrado < Fac_R2){
         'Reposo'
       }else if(line$Y < Fac_C0){
         'Comiendo'
       }else{'Movimiento'}
     }
     )
   ))

  registros

}



agrupar_actividades <-  function(registros,tiempo='orig'){
  

  require(pracma)
  
  if(!tiempo %in% c('orig','10mins','20mins','30mins','horas')){return(cat('Error ',tiempo,'no es una posibilidad para agrupar'))}
  
  if(tiempo!='orig'){
  
    registros <- registros %>% separate(Hora,c('hora','minutos','segundos'))

    if(tiempo=='10mins'){
      
    registros <- registros %>% mutate(minutos = paste0(substring(registros$minutos,1,1),0))
    
    }else if(tiempo=='20mins'){
    
    registros <-  registros %>% mutate(
      minutos = map_chr(.x= minutos , 
                    .f= function(min){if(min < 20) {
                      '00'
                      }else if(min >= 20 & min < 40){
                        '20'
                        }else if( min >= 40 & min < 60){'40'
                        }else{NA}
                      }
                    )
    )
    }else if(tiempo=='30mins'){
      
      registros <-  registros %>% mutate(
        minutos = map_chr(.x= minutos , 
                          .f= function(min){
                            if(min < 30) {
                              '00'
                            }else if(min >= 30){
                              '30'
                            }else{NA}
                          }
        )
      )
    }else if(tiempo=='horas'){
      registros$minutos = '00'
    }
    
    registros <- registros %>% mutate(Hora=paste0(hora,':',minutos,':00')) %>% 
      select(-hora,-minutos,-segundos)
  }

  #Fecha aux 
  
  estimar_actividades$Fecha_aux <- as.Date(substr(estimar_actividades$Fecha, 1, 10), format = "%Y-%m-%d")
  
    registros <- estimar_actividades %>% mutate(
      completTime = as.POSIXct(paste(Fecha_aux, Hora),
                               format = "%Y-%m-%d %H:%M:%S"))
    
    
    summarise_actividad <- registros %>% select(completTime,actividad) %>%
      dplyr::group_by(completTime) %>% dplyr::summarise(Principal_actividad =Mode(actividad))

  
  #registros <- registros %>% right_join(summarise_actividad,by = "completTime")
  
  #registros <- registros %>% select(-serie)
  
   summarise_actividad$Finca <- unique(registros$Finca)
   summarise_actividad$Vaca <- unique(registros$Vaca)
   
    summarise_actividad
}

# Graficos de movimiento

graficos_movimiento <- function(registros,escala='dia', Nombre_archivo= '',dirFol=getwd()){
  
  time <- paste(registros$completTime[1],'-',registros$completTime[length(registros$completTime)])
  
  table <- registros %>% group_by(Principal_actividad) %>%
           dplyr::summarise(conteo=dplyr::n())  
  
  table$porcentaje <- round(table$conteo/sum(table$conteo)*100,2)
  
  actRes <- ggplot(table,aes(x=Principal_actividad,y=porcentaje))+geom_bar(stat='identity')+
    geom_text(aes(label=porcentaje), vjust=1.6, color="white", size=3.5)+theme_bw()+
    theme(plot.title = element_text(hjust = 0.5,size = 10))+
    ggtitle(paste(Nombre_archivo,'- Actividades cada',  escala,'\n', '(',time,')'))+
    xlab('Principal actividad')+ylab('Porcentaje')
  
  
  ggsave(paste0(dirFol,Nombre_archivo,'_porcentaje_actividades.png'),
         actRes,width = 5,height =3 )
  
  # Analisis por 30 mins u hora
  
  registros <- registros %>% mutate(hour= hour(completTime),minutes = minute(completTime),
                                        minutes2 = 0)
  
  
  registros$minutes2[registros$minutes>30] <- 30
  
  if(escala == '30mins'){
    registros <- registros %>% mutate(newDate = as.POSIXct(paste0(lubridate::date(completTime),' ',hour,':',minutes2),format = "%Y-%m-%d %H:%M"))
  }else if(escala == 'dia'){
    registros <- registros %>% mutate(newDate = as.POSIXct(paste0(lubridate::date(completTime),' ',00,':',00),format = "%Y-%m-%d %H:%M"))
  }else if(escala == 'hora'){
    registros <- registros %>% mutate(newDate = as.POSIXct(paste0(lubridate::date(completTime),' ',hour,':',00),format = "%Y-%m-%d %H:%M"))
  }
  newMatFinal2 <- registros %>% dplyr::group_by(newDate,Principal_actividad) %>% dplyr::summarise(frecuencia =n())
  
  newMatFinal3 <- registros %>% dplyr::group_by(newDate) %>% dplyr::summarise(frecuencia_total = n())
  
  totalTable <- right_join(newMatFinal2,newMatFinal3,'newDate')
  
  finalTable <- totalTable %>% mutate(porcAct = frecuencia/frecuencia_total*100)
  
  grafico <- finalTable %>% ggplot(aes(x=newDate,y=porcAct))+geom_point(aes(colour=Principal_actividad))+
    geom_line(aes(colour=Principal_actividad))+labs(colour='Actividad principal')+
    xlab('Tiempo')+ylab('Porcentaje')+theme_bw()+
    theme(plot.title = element_text(hjust = 0.5,size = 10))+
    ggtitle(paste(Nombre_archivo,'- Actividades cada',  escala,'\n', '(',time,')'))
  
  ggsave(paste0(dirFol,Nombre_archivo,'_distribucion_actividades_fecha.png'),
         grafico,width = 5,height =3 )
  
  
  if(escala!='dia'){
    
    plot_hora <- registros %>% mutate(
      newDate = as.POSIXct(paste('2019-08-24',
                                            format(registros$newDate,'%H:%M:%S')),
                               format = "%Y-%m-%d %H:%M:%S"),Fecha = format(completTime,'%Y-%m-%d'))
    
    plot_hora <- plot_hora %>% dplyr::group_by(Fecha,hour,Principal_actividad) %>% dplyr::summarise(long = n())
    
    total <- plot_hora %>% dplyr::group_by(Fecha,hour) %>% dplyr::summarise(Total = sum(long))
    
    plot_hora <- plot_hora %>% right_join( total  ,by=c('Fecha', 'hour')) %>% mutate(porcentaje=long/Total*100)
    
    gra <- ggplot(plot_hora,aes(x=factor(hour),y=Total))+
      geom_bar(stat="identity",aes(fill=Principal_actividad))+theme_bw()+
      ylab('Total observaciones')+xlab('Hora')
    
    ggsave(paste0(dirFol,Nombre_archivo,'_distribucion_actividades_hora.png'),
           gra,width = 7,height =3 )
    
  }
  
}




graficos_movimiento_diario <- function (data_celator_new, validacion)
{
  data_celator_new <- data_celator_new %>% mutate(hour= hour(completTime),minutes = minute(completTime),
                                                  minutes2 = 0)

  
   data_celator_new$minutes2[data_celator_new$minutes>30] <- 30
   
   data_celator_new <- data_celator_new %>% mutate(newDate = as.POSIXct(paste0(lubridate::date(completTime),' ',hour,':',minutes2),format = "%Y-%m-%d %H:%M"))

   data_celator_new 

   data_celator_new2 <- data_celator_new %>% dplyr::group_by(newDate,Principal_actividad) %>% dplyr::summarize(frecuencia =n())

   
   data_celator_new3 <- data_celator_new %>% group_by(newDate) %>% dplyr::summarize(frecuencia_total = n())

   totalTable <- right_join(data_celator_new2,data_celator_new3,'newDate')

   finalTable <- totalTable %>% mutate(porcAct = frecuencia/frecuencia_total*100)

   finalTable$Day <- format(finalTable$newDate, "%Y-%m-%d")
   finalTable$Hour <- format(finalTable$newDate, "%H:%M")

   
   validacion <- validacion %>% mutate(hour= hour(completTime),minutes = minute(completTime),
                                                   minutes2 = 0)
   
   
   validacion$minutes2[validacion$minutes>30] <- 30
   
   validacion <- validacion %>% mutate(newDate = as.POSIXct(paste0(lubridate::date(completTime),' ',hour,':',minutes2),format = "%Y-%m-%d %H:%M"))
   
   validacion 
   
   validacion2 <- validacion %>% dplyr::group_by(newDate,Principal_actividad) %>% dplyr::summarize(frecuencia =n())
   
   validacion3 <- validacion %>% group_by(newDate) %>% dplyr::summarize(frecuencia_total = n())
   
   totalTable_va <- right_join(validacion2,validacion3,'newDate')
   
   finalTable_va <- totalTable_va %>% mutate(porcAct = frecuencia/frecuencia_total*100)
   
   finalTable_va$Day <- format(finalTable_va$newDate, "%Y-%m-%d")
   finalTable_va$Hour <- format(finalTable_va$newDate, "%H:%M")   
   
   finalTable_va$Principal_actividad <- as.character(finalTable_va$Principal_actividad)
   
   grantotal = rbind(finalTable_va, finalTable)
   
   
   
   finalTable = grantotal 

   dias <- split(finalTable, as.Date(finalTable$Day))
   dias
  
   lapply(dias, function(x) grafico <- x %>% ggplot(aes(x=format(x$newDate, "%H:%M"),y=porcAct, group = Principal_actividad))+geom_point(aes(colour=Principal_actividad))+
            geom_line(aes(colour=Principal_actividad))+labs(colour='Actividad principal')+
            xlab('Hora')+ylab('Porcentaje')+theme_bw()+
            theme(plot.title = element_text(hjust = 0.5,size = 10)) +
            ggtitle(paste("Finca: ", unique(data_celator_new$Finca), "Vaca: ", unique(data_celator_new$Vaca), "\n", unique(x$Day),'- Actividades cada 30 mins\n'))
           +
  
            ggsave(paste0(paste(unique(data_celator_new$Finca), "_", unique(data_celator_new$Vaca), "_", unique(x$Day), ".png")),width = 25,height =3))
  
}


#Funcion Graficar por dias
#Grafica la validacion y los datos de los sensores por dia.
grafica_validacion_sensor <- function(actividades_agrupadas, validacion)
{
  actividades_agrupadas <- actividades_agrupadas %>% mutate(hour= hour(completTime),minutes = minute(completTime),
                                                  minutes2 = 0)
  
  
  actividades_agrupadas$minutes2 <- 0
  actividades_agrupadas$minutes2[actividades_agrupadas$minutes>30] <- 30
  
  actividades_agrupadas <- actividades_agrupadas %>% mutate(newDate = as.POSIXct(paste0(lubridate::date(completTime),' ',hour,':',minutes2),format = "%Y-%m-%d %H:%M"))
  
  actividades_agrupadas 
  
  data_celator_new2 <- actividades_agrupadas %>% dplyr::group_by(newDate,Principal_actividad) %>% dplyr::summarize(frecuencia =n())
  
  
  data_celator_new3 <- actividades_agrupadas %>% group_by(newDate) %>% dplyr::summarize(frecuencia_total = n())
  
  totalTable <- right_join(data_celator_new2,data_celator_new3,'newDate')
  
  finalTable <- totalTable %>% mutate(porcAct = frecuencia/frecuencia_total*100)
  
  finalTable$Day <- format(finalTable$newDate, "%Y-%m-%d")
  finalTable$Hour <- format(finalTable$newDate, "%H:%M")
  
  #Graficar 
  
  finalTable %>% select (porcAct, Hour) %>% ggplot()+  geom_line(aes(x = Hour, y= porcAct))
  
  
  
  
}

#actividades_agrupadas <-NULL

#Funcion Graficar por dias
#Grafica la validacion y los datos de los sensores por dia.
grafica_validacion_sensor_V1 <- function(actividades_agrupadas_1)
{
  
  actividades_agrupadas_aux <- actividades_agrupadas_1 %>% mutate(hour= hour(completTime),minutes = minute(completTime))
  
  #Inicializar minutes2 para asignar valores a 30, los minutos mayores a 30
  #Si los minutes es igual a 0 entonces se le asigna a 30
  actividades_agrupadas_aux$minutes2 <- 0 
  actividades_agrupadas_aux$minutes2[actividades_agrupadas_aux$minutes >30 | actividades_agrupadas_aux$minutes == 0] <- 30
  actividades_agrupadas_aux <- actividades_agrupadas_aux %>% mutate(newDate = as.POSIXct(paste0(lubridate::date(completTime),' ',hour,':',minutes2),format = "%Y-%m-%d %H:%M"))
  
  #Cantidad de datos por cada vaca y actividad
  data_celator_new2 <- actividades_agrupadas_aux %>% dplyr::group_by(Finca, Vaca, newDate,Principal_actividad) %>% dplyr::summarize(frecuencia =n())
  
  #Cantidad de datos total 
  data_celator_new3 <- actividades_agrupadas_aux %>% group_by(Finca, Vaca, newDate) %>% dplyr::summarize(frecuencia_total = n())
   
  #Unir ambos conjuntos de datos
  totalTable <- right_join(data_celator_new2,data_celator_new3,c('Finca', 'Vaca', 'newDate'))
  
  #Conjunto de datos final
  finalTable <- totalTable %>% mutate(porcAct = frecuencia/frecuencia_total*100)
  finalTable
  # 
  finalTable$Day <- format(finalTable$newDate, "%Y-%m-%d")
  finalTable$Hour <- format(finalTable$newDate, "%H:%M")
  
  finalTable$ID <- paste0(finalTable$Finca, "_",finalTable$Vaca, "_", finalTable$Day)

  dias <- split(finalTable, finalTable$ID)

  lapply(dias, function(x) grafico <- x %>% ggplot(aes(x=format(x$newDate, "%H:%M"),y=porcAct, group = Principal_actividad))+geom_point(aes(colour=Principal_actividad))+
           geom_line(aes(colour=Principal_actividad))+labs(colour='Actividad principal')+
           xlab('Hora')+ylab('Porcentaje')+theme_bw()+ scale_y_continuous(breaks=seq(0,100,10)) +
           theme(plot.title = element_text(hjust = 0.5,size = 10)) +
            ggtitle(paste(unique(x$Finca), "_",unique(x$Vaca), "_",unique(x$Day),'- Actividades cada 30 mins\n'))+

           #ggsave(paste0(unique(x$Vaca),'distribucion_actividades.png'),width = 20,height =3 ))

            ggsave(paste0('Validacion/',"_", unique(x$Finca),"_",unique(x$Vaca), unique(x$Day),'distribucion_actividades.png'),width = 20,height =3 ))
           #ggsave(paste0('distribucion_actividades.png'),width = 20,height =3 ))

  return (finalTable)

}

agrupar_actividades_V1 <-  function(registros,tiempo='orig'){
  
  
  require(pracma)
  
  if(!tiempo %in% c('orig','10mins','20mins','30mins','horas')){return(cat('Error ',tiempo,'no es una posibilidad para agrupar'))}
  
  if(tiempo!='orig'){
    
    registros <- registros %>% separate(Hora,c('hora','minutos','segundos'))
    
    if(tiempo=='10mins'){
      
      registros <- registros %>% mutate(minutos = paste0(substring(registros$minutos,1,1),0))
      
    }else if(tiempo=='20mins'){
      
      registros <-  registros %>% mutate(
        minutos = map_chr(.x= minutos , 
                          .f= function(min){if(min < 20) {
                            '00'
                          }else if(min >= 20 & min < 40){
                            '20'
                          }else if( min >= 40 & min < 60){'40'
                          }else{NA}
                          }
        )
      )
    }else if(tiempo=='30mins'){
      
      registros <-  registros %>% mutate(
        minutos = map_chr(.x= minutos , 
                          .f= function(min){
                            if(min < 30) {
                              '00'
                            }else if(min >= 30){
                              '30'
                            }else{NA}
                          }
        )
      )
    }else if(tiempo=='horas'){
      registros$minutos = '00'
    }
    
    registros <- registros %>% mutate(Hora=paste0(hora,':',minutos,':00')) %>% 
      select(-hora,-minutos,-segundos)
  }
  
  #Fecha aux 
  estimar_actividades_aux <- registros
  estimar_actividades_aux$Fecha_aux <- as.Date(substr(estimar_actividades_aux$Fecha, 1, 10), format = "%Y-%m-%d")
  # 
   registros <- estimar_actividades_aux %>% mutate(
     completTime = as.POSIXct(paste(Fecha_aux, Hora),
                              format = "%Y-%m-%d %H:%M:%S"))

   summarise_actividad <- registros %>% dplyr::group_by(Finca, Vaca, completTime) %>% dplyr::summarise(Principal_actividad =Mode(actividad))

  return(summarise_actividad )
}


separarfechayhora <- function(datos)
{
  
  datos_guardar <- datos %>% mutate(Hora=paste0(hour(completTime), ":",minute(completTime)), Fecha = lubridate::date(completTime))
  return(datos_guardar)
}

