#Juan Camilo Rivera 
# 

library(dplyr)
library(tidyverse)
library(purrr)
library("PerformanceAnalytics")
rm(list=ls())

load("Data/Produccion_Clima_Agrosavia.RData")



descripcionPorFinca <- function(produccion_agrosavia, nombrefinca)
{
  
#produccion_hatico <- 
#produccion_agrosavia <- produccion_hatico
#Unir todos los datos
#union_datos <- do.call(rbind, produccion_agrosavia)
union_datos <- produccion_agrosavia
union_datos$Dia <- as.Date(union_datos$FechaHora)

index <- which(is.na(union_datos$Dia)==T)
if(length(index)!= 0)
{
  union_datos <- union_datos[-index, ]
}
#union_datos$Dia <- as.factor(union_datos$Dia)
#summary(union_datos)

union_datos$Dia <- as.factor(union_datos$Dia)
union_datos$Vaca <- as.factor(union_datos$Vaca)

produccion_agrosavia$FechaHora_Cta <- as.factor(produccion_agrosavia$FechaHora)

colnames(union_datos) <- gsub("Humedad Relativa", "Humedad_Relativa",colnames(union_datos))
df_datos <- union_datos %>% group_by(FechaHora_Cta, Vaca) %>% dplyr::summarize(Temp_Pr_0 = mean(Temperatura), HR_Pr_0 = mean(Humedad_Relativa),
                                             Prec_Pr_0 = sum(Precipitacion), SR_Pr_0 = mean(SR), Veloc_viento_0= mean(VientoVelocidad),
                                             TempTim_Pr_0 = mean(Temp_Timpani), Temp_Mx_0 = max(Temperatura), Temp_Mn_0 = min(Temperatura),
                                             Hr_Max_0= max(Humedad_Relativa), Hr_Min_0 = min(Humedad_Relativa),Prec_Mx_0 = max(Precipitacion),
                                             Prec_Mn_0 = min(Precipitacion),SR_Mx_0 = max(SR), SR_Mn_0 = min(SR), Veloc_vientoMx_0 = max(VientoVelocidad),
                                             Veloc_vientoMn_0 = min(VientoVelocidad), TempTim_Mx_0 = max(Temp_Timpani), TempTim_Mn_0 = min(Temp_Timpani),
                                             Prod = mean(as.numeric(Producion)))




df_datos <- mutate (.data = df_datos, ITH = 0.8*Temp_Pr_0 + (HR_Pr_0*(Temp_Pr_0-14.4)) + 46.4)

#Graficar por finca
graficar_porFinca <- df_datos[, which(colnames(df_datos)%in% c("Dia", "Temp_Pr_0","HR_Pr_0", "Prec_Pr_0", "SR_Pr_0", "Veloc_viento_0"))]
graficar_porFinca <- unique(graficar_porFinca)
graficar_porFinca <- na.omit(graficar_porFinca)

print(summary(graficar_porFinca))

colnames(graficar_porFinca) <- c("Dia", "Temp (C)", "HR (0 - 100)", "Pre (mm)", "SR (cal/cm2)", "V_V(mps)")


#Graficas
gr <- gather(graficar_porFinca, "Variable", "Valor", -c(Dia))
gr$Dia <- as.Date(gr$Dia)

bp <- ggplot(gr, aes(x=Dia, y=Valor, group = 1)) +
  geom_line() + theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank())

grafica <- bp + facet_grid(Variable~ .,  scales="free")


grafica

#nombrefinca <- "agrosavia"
nombre <- paste0("Graficos/Articulo/", nombrefinca, ".jpeg")
ggsave(nombre, units ="cm", width = 10, height = 11)

df_datos$Finca <- nombrefinca
return(df_datos)
}

#Analisis por finca 

load("Data/Produccion_Clima_Agrosavia.RData")
load("Data/Produccion_Clima_Elhatico.RData")
load("Data/Produccion_Clima_garzonero.RData")
load("Data/Produccion_Clima_TempTim_Marruecos.RData")
load("Data/Produccion_Clima_TempTim_Elhatico.RData")
load("Data/Produccion_Clima_TempTimAgrosavia.RData")
load("Data/Produccion_Clima_TempTim_garzonero.RData")




agrosavia <- descripcionPorFinca(produccion_agrosavia, "Agrosavia")
garzonero <- descripcionPorFinca(produccion_garzonero, "Garzonero")
elhatico <- descripcionPorFinca(produccion_hatico, "ElHatico")
marruecos <- descripcionPorFinca(produccion_marruecos_2, "Marruecos")


#Modificar el data frame
#tipo = 1 para producción
#tipo = 2 para temperatura timpanica por dia y vaca
#tipo = 3 para temperatura timpanica por vaca

boxplot <- function(agrosavia, finca, tipo = 1, numeracionfinca)
{

if(tipo == 1)
{
  vacas_produccion <- agrosavia[,c("Vaca", "Prod")]
  vacas_produccion <- na.omit(vacas_produccion)
  vacas_produccion$Vaca <- as.factor(vacas_produccion$Vaca)
  r_aux <- ggplot(vacas_produccion, aes(x= Vaca, y = Prod)) 
  stat_box_data <- function(y, upper_limit = max(vacas_produccion$Prod)*1.15) {
    return( 
      data.frame(
        y = 0.95*upper_limit,
        label = paste('obs =', length(y), '\n',
                      'media =', round(mean(y), 1), '\n')
      )
    )
  }
  
  
}
if (tipo == 2)
{
  vacas_produccion <- agrosavia[,c("Vaca","Temp_Timpani")]
  vacas_produccion <- na.omit(vacas_produccion)
  vacas_produccion$Vaca <- as.factor(vacas_produccion$Vaca)
  r_aux <- ggplot(vacas_produccion, aes(x= Vaca, y = Temp_Timpani)) 
  
  stat_box_data <- function(y, upper_limit = max(vacas_produccion$Temp_Timpani)*1.15) {
    return( 
      data.frame(
        y = 0.95*upper_limit,
        label = paste('obs =', length(y), '\n',
                      'media =', round(mean(y), 1), '\n')
      )
    )
  
}
}

r <- r_aux + geom_boxplot() +
  stat_summary(
                fun.data = stat_box_data, 
                geom = "text", 
                hjust = 0.5,
                vjust = 0.9
              ) +  theme_light() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  ggtitle(numeracionfinca) +  theme(plot.title = element_text(hjust = 0.5))


r
nombre <- paste0("Graficos/Articulo/Boxplot_", finca, ".jpeg")
ggsave( nombre, width = 20, height = 10, units ="cm")

}



#------------------------------Prueba de Hipotesis----------------------------------------------
#Agrosavia
#125204 vs 13502 
vaca_125204 <- filter (agrosavia, Vaca== "vaca_12504")
vaca_13502 <- filter (agrosavia, Vaca== "vaca_13502" )
vaca_125204  <- select(vaca_125204, Prod)
vaca_13502   <- select(vaca_13502 , Prod)
wilcox.test(vaca_125204$Prod, vaca_13502$Prod, alternative = "greater", mu =0)


#125204 vs 13502 
vaca_125204 <- filter (agrosavia, Vaca== "vaca_12504")
vaca_7301 <- filter (agrosavia, Vaca== "vaca_7301" )
vaca_125204  <- select(vaca_125204, Prod)
vaca_7301  <- select(vaca_7301 , Prod)
wilcox.test(vaca_125204$Prod, vaca_7301$Prod, alternative = "greater", mu =0)


#125204 vs #301 
vaca_125204 <- filter (agrosavia, Vaca== "vaca_12504")
vaca_E30 <- filter (agrosavia, Vaca== "vaca_E30" )
vaca_125204  <- select(vaca_125204, Prod)
vaca_E30  <- select(vaca_E30 , Prod)
wilcox.test(vaca_125204$Prod, vaca_E30$Prod, alternative = "greater", mu =0)

#13502 vs #7301 
vaca_13502 <- filter (agrosavia, Vaca== "vaca_13502")
vaca_7301  <- filter (agrosavia, Vaca== "vaca_7301" )
vaca_13502   <- select(vaca_13502, Prod)
vaca_7301  <- select(vaca_7301 , Prod)
wilcox.test(vaca_13502$Prod, vaca_7301$Prod, alternative = "greater", mu =0)

#13502 vs #E30 
vaca_13502 <- filter (agrosavia, Vaca== "vaca_13502")
vaca_E30   <- filter (agrosavia, Vaca== "vaca_E30" )
vaca_13502   <- select(vaca_13502, Prod)
vaca_E30  <- select(vaca_E30 , Prod)
wilcox.test(vaca_E30$Prod, vaca_13502$Prod, alternative = "greater", mu =0)

#13502 vs #E30 
vaca_3395 <- filter (agrosavia, Vaca== "vaca_3395")
vaca_7301   <- filter (agrosavia, Vaca== "vaca_7301" )
vaca_3395   <- select(vaca_3395, Prod)
vaca_7301  <- select(vaca_7301, Prod)
wilcox.test(vaca_3395$Prod, vaca_7301$Prod, alternative = "greater", mu =0)


#3395 vs #E30 
vaca_3395 <- filter (agrosavia, Vaca== "vaca_3395")
vaca_E30   <- filter (agrosavia, Vaca== "vaca_E30" )
vaca_3395   <- select(vaca_3395, Prod)
vaca_E30  <- select(vaca_E30, Prod)
wilcox.test(vaca_3395$Prod, vaca_E30$Prod, alternative = "greater", mu =0)


#7301 vs #E30 
vaca_7301 <- filter (agrosavia, Vaca== "vaca_7301")
vaca_E30   <- filter (agrosavia, Vaca== "vaca_E30" )
vaca_7301   <- select(vaca_7301, Prod)
vaca_E30  <- select(vaca_E30, Prod)
wilcox.test(vaca_7301$Prod, vaca_E30$Prod, alternative = "greater", mu =0)


#-----------------------------------------------------------------------------------
#El HAtico
#4209 vs 4271 
vaca_4209 <- filter (elhatico, Vaca== "vaca_4209")
vaca_4271  <- filter (elhatico, Vaca== "vaca_4271" )
vaca_4209  <- select(vaca_4209, Prod)
vaca_4271  <- select(vaca_4271 , Prod)
wilcox.test(vaca_4209$Prod, vaca_4271$Prod, alternative = "greater", mu =0)


#El HAtico
#4209 vs 4273 
vaca_4209 <- filter (elhatico, Vaca== "vaca_4209")
vaca_4273  <- filter (elhatico, Vaca== "vaca_4273" )
vaca_4209  <- select(vaca_4209, Prod)
vaca_4273  <- select(vaca_4273 , Prod)
wilcox.test(vaca_4209$Prod, vaca_4273$Prod, alternative = "greater", mu =0)


#El HAtico
#4209 vs 4330 
vaca_4209 <- filter (elhatico, Vaca== "vaca_4209")
vaca_4330   <- filter (elhatico, Vaca== "vaca_4330" )
vaca_4209  <- select(vaca_4209, Prod)
vaca_4330  <- select(vaca_4330 , Prod)
wilcox.test(vaca_4209$Prod, vaca_4330$Prod, alternative = "greater", mu =0)

#El HAtico
#4209 vs 4340 
vaca_4209 <- filter (elhatico, Vaca== "vaca_4209")
vaca_4340    <- filter (elhatico, Vaca== "vaca_4340" )
vaca_4209  <- select(vaca_4209, Prod)
vaca_4340   <- select(vaca_4340  , Prod)
wilcox.test(vaca_4209$Prod, vaca_4340$Prod, alternative = "greater", mu =0)

#El HAtico
#4209 vs 4340 
vaca_4209 <- filter (elhatico, Vaca== "vaca_4209")
vaca_4378    <- filter (elhatico, Vaca== "vaca_4378" )
vaca_4209  <- select(vaca_4209, Prod)
vaca_4378   <- select(vaca_4378  , Prod)
wilcox.test(vaca_4209$Prod, vaca_4378$Prod, alternative = "greater", mu =0)

#El HAtico
#4209 vs 4427 
vaca_4209 <- filter (elhatico, Vaca== "vaca_4209")
vaca_4427    <- filter (elhatico, Vaca== "vaca_4427" )
vaca_4209  <- select(vaca_4209, Prod)
vaca_4427   <- select(vaca_4427  , Prod)
wilcox.test(vaca_4209$Prod, vaca_4427$Prod, alternative = "greater", mu =0)

#4209 vs 4427 
vaca_4271 <- filter (elhatico, Vaca== "vaca_4271")
vaca_4427    <- filter (elhatico, Vaca== "vaca_4427" )
vaca_4209  <- select(vaca_4209, Prod)
vaca_4271   <- select(vaca_4271  , Prod)
wilcox.test(vaca_4271$Prod, vaca_4427$Prod, alternative = "greater", mu =0)

#4378 vs 4427 
vaca_4378 <- filter (elhatico, Vaca== "vaca_4378")
vaca_4427    <- filter (elhatico, Vaca== "vaca_4427" )
vaca_4378  <- select(vaca_4378, Prod)
vaca_4427  <- select(vaca_4427  , Prod)
wilcox.test(vaca_4378$Prod, vaca_4427$Prod, alternative = "greater", mu =0)

#4271 vs 4340 
vaca_4271 <- filter (elhatico, Vaca== "vaca_4271")
vaca_4340    <- filter (elhatico, Vaca== "vaca_4340" )
vaca_4271  <- select(vaca_4271, Prod)
vaca_4340  <- select(vaca_4340  , Prod)
wilcox.test(vaca_4271$Prod, vaca_4340$Prod, alternative = "greater", mu =0)

#4271 vs 4330 
vaca_4271 <- filter (elhatico, Vaca== "vaca_4271")
vaca_4330  <- filter (elhatico, Vaca== "vaca_4330" )
vaca_4271  <- select(vaca_4271, Prod)
vaca_4330  <- select(vaca_4330  , Prod)
wilcox.test(vaca_4271$Prod, vaca_4330$Prod, alternative = "greater", mu =0)

#4271 vs 4273  
vaca_4271 <- filter (elhatico, Vaca== "vaca_4271")
vaca_4273  <- filter (elhatico, Vaca== "vaca_4273" )
vaca_4271  <- select(vaca_4271, Prod)
vaca_4273  <- select(vaca_4273  , Prod)
wilcox.test(vaca_4271$Prod, vaca_4273$Prod, alternative = "greater", mu =0)

#4330 vs 4273  
vaca_4330 <- filter (elhatico, Vaca== "vaca_4330")
vaca_4273  <- filter (elhatico, Vaca== "vaca_4273" )
vaca_4330  <- select(vaca_4330, Prod)
vaca_4273  <- select(vaca_4273  , Prod)
wilcox.test(vaca_4330$Prod, vaca_4273$Prod, alternative = "greater", mu =0)


#4340 vs 4273  
vaca_4340 <- filter (elhatico, Vaca== "vaca_4340")
vaca_4273  <- filter (elhatico, Vaca== "vaca_4273" )
vaca_4340  <- select(vaca_4340, Prod)
vaca_4273  <- select(vaca_4273  , Prod)
wilcox.test(vaca_4340$Prod, vaca_4273$Prod, alternative = "greater", mu =0)

#4378 vs 4273  
vaca_4378 <- filter (elhatico, Vaca== "vaca_4378")
vaca_4273  <- filter (elhatico, Vaca== "vaca_4273" )
vaca_4378  <- select(vaca_4378, Prod)
vaca_4273  <- select(vaca_4273  , Prod)
wilcox.test(vaca_4378$Prod, vaca_4273$Prod, alternative = "greater", mu =0)

#4427 vs 4273  
vaca_4427 <- filter (elhatico, Vaca== "vaca_4427")
vaca_4273  <- filter (elhatico, Vaca== "vaca_4273" )
vaca_4427  <- select(vaca_4427, Prod)
vaca_4273  <- select(vaca_4273  , Prod)
wilcox.test(vaca_4378$Prod, vaca_4273$Prod, alternative = "greater", mu =0)

#4427 vs 4330  
vaca_4427 <- filter (elhatico, Vaca== "vaca_4427")
vaca_4330  <- filter (elhatico, Vaca== "vaca_4330" )
vaca_4427  <- select(vaca_4427, Prod)
vaca_4330  <- select(vaca_4330  , Prod)
wilcox.test(vaca_4330$Prod, vaca_4427$Prod, alternative = "greater", mu =0)

#4378 vs 4330  
vaca_4378 <- filter (elhatico, Vaca== "vaca_4378")
vaca_4330  <- filter (elhatico, Vaca== "vaca_4330" )
vaca_4378 <- select(vaca_4378, Prod)
vaca_4330  <- select(vaca_4330  , Prod)
wilcox.test(vaca_4330$Prod, vaca_4378$Prod, alternative = "greater", mu =0)

#4427 vs 4330  
vaca_4427 <- filter (elhatico, Vaca== "vaca_4427")
vaca_4330  <- filter (elhatico, Vaca== "vaca_4330" )
vaca_4427<- select(vaca_4427, Prod)
vaca_4330  <- select(vaca_4330  , Prod)
wilcox.test(vaca_4330$Prod, vaca_4427$Prod, alternative = "greater", mu =0)

#4427 vs 4340  
vaca_4378 <- filter (elhatico, Vaca== "vaca_4378")
vaca_4340  <- filter (elhatico, Vaca== "vaca_4340" )
vaca_4378<- select(vaca_4378, Prod)
vaca_4340  <- select(vaca_4340  , Prod)
wilcox.test(vaca_4340$Prod, vaca_4378$Prod, alternative = "greater", mu =0)

#4427 vs 4378  
vaca_4427 <- filter (elhatico, Vaca== "vaca_4427")
vaca_4378   <- filter (elhatico, Vaca== "vaca_4378" )
vaca_4427<- select(vaca_4427, Prod)
vaca_4378   <- select(vaca_4378   , Prod)
wilcox.test(vaca_4378$Prod, vaca_4427$Prod, alternative = "greater", mu =0)

#-----------------------------------------------------------------------------------
#garzonero

#0102 vs 0109 
vaca_0102 <- filter (garzonero, Vaca== "vaca_0102")
vaca_0109  <- filter (garzonero, Vaca== "vaca_0109" )
vaca_0102  <- select(vaca_0102, Prod)
vaca_0109  <- select(vaca_0109 , Prod)
wilcox.test(vaca_0109$Prod, vaca_0102$Prod, alternative = "greater", mu =0)

#0102 vs 0109 
vaca_0102 <- filter (garzonero, Vaca== "vaca_0102")
vaca_2016  <- filter (garzonero, Vaca== "vaca_2016" )
vaca_0102  <- select(vaca_0102, Prod)
vaca_2016  <- select(vaca_2016 , Prod)
wilcox.test(vaca_0102$Prod, vaca_2016$Prod, alternative = "greater", mu =0)

#0102 vs 0109 
vaca_2037 <- filter (garzonero, Vaca== "vaca_2037")
vaca_0102  <- filter (garzonero, Vaca== "vaca_0102" )
vaca_0102  <- select(vaca_0102, Prod)
vaca_2037  <- select(vaca_2037 , Prod)
wilcox.test(vaca_0102$Prod, vaca_2037$Prod, alternative = "greater", mu =0)

#0102 vs 2064 
vaca_2064 <- filter (garzonero, Vaca== "vaca_2064")
vaca_0102  <- filter (garzonero, Vaca== "vaca_0102" )
vaca_0102  <- select(vaca_0102, Prod)
vaca_2064  <- select(vaca_2064 , Prod)
wilcox.test(vaca_0102$Prod, vaca_2064$Prod, alternative = "greater", mu =0)


#0102 vs 2086 
vaca_2086 <- filter (garzonero, Vaca== "vaca_2086")
vaca_0102  <- filter (garzonero, Vaca== "vaca_0102" )
vaca_0102  <- select(vaca_0102, Prod)
vaca_2086  <- select(vaca_2086 , Prod)
wilcox.test(vaca_0102$Prod, vaca_2086$Prod, alternative = "greater", mu =0)

#0102 vs 7078 
vaca_7078 <- filter (garzonero, Vaca== "vaca_7078")
vaca_0102  <- filter (garzonero, Vaca== "vaca_0102" )
vaca_0102  <- select(vaca_0102, Prod)
vaca_7078  <- select(vaca_7078 , Prod)
wilcox.test(vaca_0102$Prod, vaca_7078$Prod, alternative = "greater", mu =0)


#0109 vs 2086 
vaca_2086 <- filter (garzonero, Vaca== "vaca_2086")
vaca_0109  <- filter (garzonero, Vaca== "vaca_0109" )
vaca_0109  <- select(vaca_0109, Prod)
vaca_2086  <- select(vaca_2086 , Prod)
wilcox.test(vaca_0109$Prod, vaca_2086$Prod, alternative = "greater", mu =0)

#0109 vs 2064 
vaca_2064 <- filter (garzonero, Vaca== "vaca_2064")
vaca_0109  <- filter (garzonero, Vaca== "vaca_0109" )
vaca_0109  <- select(vaca_0109, Prod)
vaca_2064  <- select(vaca_2064 , Prod)
wilcox.test(vaca_0109$Prod, vaca_2064$Prod, alternative = "greater", mu =0)

#0109 vs 2037 
vaca_2037 <- filter (garzonero, Vaca== "vaca_2037")
vaca_0109  <- filter (garzonero, Vaca== "vaca_0109" )
vaca_0109  <- select(vaca_0109, Prod)
vaca_2037  <- select(vaca_2037 , Prod)
wilcox.test(vaca_0109$Prod, vaca_2037$Prod, alternative = "greater", mu =0)

#0109 vs 2016  
vaca_2016 <- filter (garzonero, Vaca== "vaca_2016")
vaca_0109  <- filter (garzonero, Vaca== "vaca_0109" )
vaca_0109  <- select(vaca_0109, Prod)
vaca_2016  <- select(vaca_2016 , Prod)
wilcox.test(vaca_0109$Prod, vaca_2016$Prod, alternative = "greater", mu =0)

#2064 vs 2016  
vaca_2064 <- filter (garzonero, Vaca== "vaca_2064")
vaca_2016  <- filter (garzonero, Vaca== "vaca_2016" )
vaca_2064  <- select(vaca_2064, Prod)
vaca_2016  <- select(vaca_2016 , Prod)
wilcox.test(vaca_2064$Prod, vaca_2016$Prod, alternative = "greater", mu =0)


#2086 vs 2016  
vaca_2086 <- filter (garzonero, Vaca== "vaca_2086")
vaca_2016  <- filter (garzonero, Vaca== "vaca_2016" )
vaca_2086  <- select(vaca_2086, Prod)
vaca_2016  <- select(vaca_2016 , Prod)
wilcox.test(vaca_2086$Prod, vaca_2016$Prod, alternative = "greater", mu =0)

#2086 vs 2016  
vaca_7078 <- filter (garzonero, Vaca== "vaca_7078")
vaca_2016  <- filter (garzonero, Vaca== "vaca_2016" )
vaca_7078  <- select(vaca_7078, Prod)
vaca_2016  <- select(vaca_2016 , Prod)
wilcox.test(vaca_7078$Prod, vaca_2016$Prod, alternative = "greater", mu =0)

#2037 vs 7078  
vaca_7078 <- filter (garzonero, Vaca== "vaca_7078")
vaca_2037  <- filter (garzonero, Vaca== "vaca_2037" )
vaca_7078  <- select(vaca_7078, Prod)
vaca_2037  <- select(vaca_2037, Prod)
wilcox.test(vaca_7078$Prod, vaca_2037$Prod, alternative = "greater", mu =0)

#2037 vs 2086  
vaca_2086  <- filter (garzonero, Vaca== "vaca_2086")
vaca_2037  <- filter (garzonero, Vaca== "vaca_2037" )
vaca_2086  <- select(vaca_2086, Prod)
vaca_2037  <- select(vaca_2037, Prod)
wilcox.test(vaca_2086$Prod, vaca_2037$Prod, alternative = "greater", mu =0)

#2037 vs 2064  
vaca_2064  <- filter (garzonero, Vaca== "vaca_2064")
vaca_2037  <- filter (garzonero, Vaca== "vaca_2037" )
vaca_2064  <- select(vaca_2064, Prod)
vaca_2037  <- select(vaca_2037, Prod)
wilcox.test(vaca_2064$Prod, vaca_2037$Prod, alternative = "greater", mu =0)

#2086 vs 2064  
vaca_2086  <- filter (garzonero, Vaca== "vaca_2086")
vaca_2037  <- filter (garzonero, Vaca== "vaca_2037" )
vaca_2086  <- select(vaca_2086, Prod)
vaca_2037  <- select(vaca_2037, Prod)
wilcox.test(vaca_2086$Prod, vaca_2037$Prod, alternative = "greater", mu =0)

#7078 vs 2064  
vaca_7078  <- filter (garzonero, Vaca== "vaca_7078")
vaca_2064  <- filter (garzonero, Vaca== "vaca_2064" )
vaca_7078  <- select(vaca_7078, Prod)
vaca_2064  <- select(vaca_2064, Prod)
wilcox.test(vaca_2064$Prod, vaca_7078$Prod, alternative = "greater", mu =0)

#7078 vs 2086  
vaca_7078  <- filter (garzonero, Vaca== "vaca_7078")
vaca_2086  <- filter (garzonero, Vaca== "vaca_2086" )
vaca_7078  <- select(vaca_7078, Prod)
vaca_2086   <- select(vaca_2086 , Prod)
wilcox.test(vaca_2086$Prod, vaca_7078$Prod, alternative = "greater", mu =0)


boxplot(agrosavia , "agrosavia", tipo = 1)
boxplot(garzonero , "garzonero", tipo = 1)
boxplot(elhatico, "elhatico", tipo = 1)


boxplot(produccion_agrosavia_2 , "agrosavia_TP", tipo = 2, numeracionfinca = "Finca 4")
boxplot(produccion_garzonero_2, "garzonero_TP", tipo = 2, numeracionfinca = "Finca 1")
boxplot(produccion_hatico_2, "elhatico_TP", tipo = 2, numeracionfinca = "Finca 3")
boxplot(produccion_marruecos_2, "marruecos_TP", tipo = 2, numeracionfinca = "Finca 2")

#--------------------------------Correlacion------------------------------------------------------------

library("PerformanceAnalytics") 

 
#agrosavia_corre <- filter (agrosavia, Vaca!="vaca_12500" | Vaca!="vaca_42-3")
agrosavia_corre <- agrosavia
jpeg('Graficos/Articulo/Cor_Agrosavia_0_pt1.jpeg', width = 600, height = 600)
corr_agrosavia_0_1 <- agrosavia_corre[, c("Temp_Pr_0","HR_Pr_0", "Prec_Pr_0", "SR_Pr_0", "Veloc_viento_0", "TempTim_Pr_0", "Prod")]
chart.Correlation(corr_agrosavia_0_1, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Agrosavia_0_pt2.jpeg', width = 600, height = 600)
corr_agrosavia_0_2 <- agrosavia_corre[, c("Temp_Mx_0","Temp_Mn_0", "Hr_Max_0", "Hr_Min_0", "Prec_Mx_0", "Prec_Mn_0", "Prod")]
chart.Correlation(corr_agrosavia_0_2, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Agrosavia_0_pt3.jpeg', width = 600, height = 600)
corr_agrosavia_0_3 <- agrosavia_corre[, c("SR_Mx_0", "SR_Mn_0", "Veloc_vientoMx_0", "Veloc_vientoMn_0", "TempTim_Mx_0", "TempTim_Mn_0", "Prod")]
chart.Correlation(corr_agrosavia_0_3, histogram=TRUE, pch=19)
dev.off()
corr_agrosavia_0_3 <- cbind(corr_agrosavia_0_3, agrosavia_corre[, c("Finca", "Vaca")])


jpeg('Graficos/Articulo/Cor_Garzonero_0_pt1.jpeg', width = 600, height = 600)
corr_garzonero_0_1 <- garzonero[, c("Temp_Pr_0","HR_Pr_0", "Prec_Pr_0", "SR_Pr_0", "Veloc_viento_0", "TempTim_Pr_0", "Prod")]
chart.Correlation(corr_garzonero_0_1, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Garzonero_0_pt2.jpeg', width = 600, height = 600)
corr_garzonero_0_2 <- garzonero[, c("Temp_Mx_0","Temp_Mn_0", "Hr_Max_0", "Hr_Min_0", "Prec_Mx_0", "Prec_Mn_0", "Prod")]
chart.Correlation(corr_garzonero_0_2, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Garzonero_0_pt3.jpeg', width = 600, height = 600)
corr_garzonero_0_3 <- garzonero[, c("SR_Mx_0", "SR_Mn_0", "Veloc_vientoMx_0", "Veloc_vientoMn_0", "TempTim_Mx_0", "TempTim_Mn_0", "Prod")]
chart.Correlation(corr_garzonero_0_3, histogram=TRUE, pch=19)
dev.off()

corr_garzonero_0_3 <- cbind(corr_garzonero_0_3, garzonero[, c("Finca", "Vaca")])


jpeg('Graficos/Articulo/Cor_Elhatico_0_pt1.jpeg', width = 600, height = 600)
corr_elhatico_0_1  <- elhatico[, c("Temp_Pr_0","HR_Pr_0", "Prec_Pr_0", "SR_Pr_0", "Veloc_viento_0", "TempTim_Pr_0", "Prod")]
chart.Correlation(corr_elhatico_0_1, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Elhatico_0_pt2.jpeg', width = 600, height = 600)
corr_elhatico_0_2 <- elhatico[, c("Temp_Mx_0","Temp_Mn_0", "Hr_Max_0", "Hr_Min_0", "Prec_Mx_0", "Prec_Mn_0", "Prod")]
chart.Correlation(corr_elhatico_0_2, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Elhatico_0_pt3.jpeg', width = 600, height = 600)
corr_elhatico_0_3 <- elhatico[, c("SR_Mx_0", "SR_Mn_0", "Veloc_vientoMx_0", "Veloc_vientoMn_0", "TempTim_Mx_0", "TempTim_Mn_0", "Prod")]
chart.Correlation(corr_elhatico_0_3, histogram=TRUE, pch=19)
dev.off()

corr_elhatico_0_3 <- cbind(corr_elhatico_0_3, elhatico[, c("Finca", "Vaca")])





#------------------------------------Indicadores de clima-------------------------------------------------------

x <-c(1, 5, 4, 5, 7, 8, 9, 2, 1)

zoo::rollapply(x, 3, by = 3, sum)

#x vector de valores
#n numero de dias antes

suma <- function(x, n)
{
  rollapply(x, n, sum)
}

agrosavia_corre4209 <- filter (aq, Vaca == "vaca_4209")
agrosavia_4209 <- filter (elhatico, Vaca == "vaca_4209")
agrosavia_4209
agrosavia_corre4209




indicadores <- function (agrosavia, numerodiasantes)
{



lista <- split(agrosavia, agrosavia$Vaca)
resultado_SR <- list()
resultado_TM <- list()
resultado_HR <- list()
resultado_TT <- list()
resultado_VV <- list()
resultado_PP <- list()
resultado_SR_Mx <- list()
resultado_SR_Mn <- list()
resultado_TM_Mx <- list()
resultado_TM_Mn <- list()
resultado_HR_Mx <- list()
resultado_HR_Mn <- list()
resultado_TT_Mx <- list()
resultado_TT_Mn <- list()
resultado_VV_Mx <- list()
resultado_VV_Mn <- list()
resultado_PP_Mx <- list()
resultado_PP_Mn <- list()


for (i in 1:length(lista ))
{
  
  resultado_SR[[i]] <- zoo::rollapply(lista[[i]]$SR_Pr_0, numerodiasantes , mean, fill=NA, align='right')
  resultado_TM[[i]] <- zoo::rollapply(lista[[i]]$Temp_Pr_0, numerodiasantes , mean, fill=NA, align='right')
  resultado_HR[[i]] <- zoo::rollapply(lista[[i]]$HR_Pr_0, numerodiasantes , mean, fill=NA, align='right')
  resultado_TT[[i]] <- zoo::rollapply(lista[[i]]$TempTim_Pr_0, numerodiasantes , mean, fill=NA, align='right')
  resultado_VV[[i]] <- zoo::rollapply(lista[[i]]$Veloc_viento_0, numerodiasantes , mean, fill=NA, align='right')
  resultado_PP[[i]] <- zoo::rollapply(lista[[i]]$Prec_Pr_0, numerodiasantes , sum, fill=NA, align='right')
  
  resultado_SR_Mx[[i]] <- zoo::rollapply(lista[[i]]$SR_Pr_0, numerodiasantes , max, fill=NA, align='right')
  resultado_SR_Mn[[i]] <- zoo::rollapply(lista[[i]]$SR_Pr_0, numerodiasantes , min, fill=NA, align='right')
  
  
  resultado_TM_Mx[[i]] <- zoo::rollapply(lista[[i]]$Temp_Pr_0, numerodiasantes , max, fill=NA, align='right')
  resultado_TM_Mn[[i]] <- zoo::rollapply(lista[[i]]$Temp_Pr_0, numerodiasantes , min, fill=NA, align='right')
  
  resultado_HR_Mx[[i]] <- zoo::rollapply(lista[[i]]$HR_Pr_0, numerodiasantes , max, fill=NA, align='right')
  resultado_HR_Mn[[i]] <- zoo::rollapply(lista[[i]]$HR_Pr_0, numerodiasantes , min, fill=NA, align='right')
  
  resultado_TT_Mx[[i]] <- zoo::rollapply(lista[[i]]$TempTim_Pr_0, numerodiasantes , max, fill=NA, align='right')
  resultado_TT_Mn[[i]] <- zoo::rollapply(lista[[i]]$TempTim_Pr_0, numerodiasantes , min, fill=NA, align='right')
  
  resultado_VV_Mx[[i]] <- zoo::rollapply(lista[[i]]$Veloc_viento_0, numerodiasantes , max, fill=NA, align='right')
  resultado_VV_Mn[[i]] <- zoo::rollapply(lista[[i]]$Veloc_viento_0, numerodiasantes , min, fill=NA, align='right')
  
  resultado_PP_Mx[[i]] <- zoo::rollapply(lista[[i]]$Prec_Pr_0, numerodiasantes , max, fill=NA, align='right')
  resultado_PP_Mn[[i]] <- zoo::rollapply(lista[[i]]$Prec_Pr_0, numerodiasantes , min, fill=NA, align='right')


  lista[[i]]$SR_Pr_0 <-  resultado_SR[[i]]
  lista[[i]]$SR_Mx_0 <-  resultado_SR_Mx[[i]]
  lista[[i]]$SR_Mn_0 <-  resultado_SR_Mn[[i]]


  lista[[i]]$Temp_Pr_0 <-  resultado_TM[[i]]
  lista[[i]]$Temp_Mx_0 <-  resultado_TM_Mx[[i]]
  lista[[i]]$Temp_Mn_0 <-  resultado_TM_Mn[[i]]

  lista[[i]]$HR_Pr_0 <-  resultado_HR[[i]]
  lista[[i]]$Hr_Max_0 <-  resultado_HR_Mx[[i]]
  lista[[i]]$Hr_Min_0 <-  resultado_HR_Mn[[i]]



  lista[[i]]$TempTim_Pr_0 <-  resultado_TT[[i]]
  lista[[i]]$TempTim_Mx_0 <-  resultado_TT_Mx[[i]]
  lista[[i]]$TempTim_Mn_0 <-  resultado_TT_Mn[[i]]



  lista[[i]]$Veloc_viento_0 <-  resultado_VV[[i]]
  lista[[i]]$Veloc_vientoMx_0 <-  resultado_VV_Mx[[i]]
  lista[[i]]$Veloc_vientoMn_0 <-  resultado_VV_Mn[[i]]



  lista[[i]]$Prec_Pr_0 <-  resultado_PP[[i]]
  lista[[i]]$Prec_Mx_0 <-  resultado_PP_Mx[[i]]
  lista[[i]]$Prec_Mn_0<-  resultado_PP_Mn[[i]]


}


lis_final <- do.call(rbind, lista)
return(lista)

}


#-----------------------------------1 dia antes-----------------------------------------------------------------------------#

#agrosavia 1 antes

agrosavia_1antes <- do.call (rbind, indicadores(agrosavia, 1))
agrosavia_corre <- agrosavia_1antes
jpeg('Graficos/Articulo/Cor_Agrosavia_1_pt1.jpeg', width = 600, height = 600)
corr_agrosavia_1_1 <- agrosavia_corre[, c("Temp_Pr_0","HR_Pr_0", "Prec_Pr_0", "SR_Pr_0", "Veloc_viento_0", "TempTim_Pr_0", "Prod")]
chart.Correlation(corr_agrosavia_1_1, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Agrosavia_1_pt2.jpeg', width = 600, height = 600)
corr_agrosavia_1_2 <- agrosavia_corre[, c("Temp_Mx_0","Temp_Mn_0", "Hr_Max_0", "Hr_Min_0", "Prec_Mx_0", "Prec_Mn_0", "Prod")]
chart.Correlation(corr_agrosavia_1_2, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Agrosavia_1_pt3.jpeg', width = 600, height = 600)
corr_agrosavia_1_3 <- agrosavia_corre[, c("SR_Mx_0", "SR_Mn_0", "Veloc_vientoMx_0", "Veloc_vientoMn_0", "TempTim_Mx_0", "TempTim_Mn_0", "Prod")]
chart.Correlation(corr_agrosavia_1_3, histogram=TRUE, pch=19)
dev.off()
corr_agrosavia_1_3 <- cbind(corr_agrosavia_1_3, agrosavia_corre[, c("Finca", "Vaca")])


#garzonero 1 antes
garzonero_1antes <- do.call (rbind, indicadores(garzonero, 1))
garzonero <- garzonero_1antes 
jpeg('Graficos/Articulo/Cor_Garzonero_1_pt1.jpeg', width = 600, height = 600)
corr_garzonero_1_1 <- garzonero[, c("Temp_Pr_0","HR_Pr_0", "Prec_Pr_0", "SR_Pr_0", "Veloc_viento_0", "TempTim_Pr_0", "Prod")]
chart.Correlation(corr_garzonero_1_1, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Garzonero_1_pt2.jpeg', width = 600, height = 600)
corr_garzonero_1_2 <- garzonero[, c("Temp_Mx_0","Temp_Mn_0", "Hr_Max_0", "Hr_Min_0", "Prec_Mx_0", "Prec_Mn_0", "Prod")]
chart.Correlation(corr_garzonero_1_2, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Garzonero_1_pt3.jpeg', width = 600, height = 600)
corr_garzonero_1_3 <- garzonero[, c("SR_Mx_0", "SR_Mn_0", "Veloc_vientoMx_0", "Veloc_vientoMn_0", "TempTim_Mx_0", "TempTim_Mn_0", "Prod")]
chart.Correlation(corr_garzonero_1_3, histogram=TRUE, pch=19)
dev.off()
corr_garzonero_1_3 <- cbind(corr_garzonero_1_3, garzonero[, c("Finca", "Vaca")])

#Hatico 1 dia antes
elhatico_1antes <- do.call (rbind, indicadores(elhatico, 1))
elhatico <- elhatico_1antes 
jpeg('Graficos/Articulo/Cor_Elhatico_1_pt1.jpeg', width = 600, height = 600)
corr_elhatico_1_1 <- elhatico[, c("Temp_Pr_0","HR_Pr_0", "Prec_Pr_0", "SR_Pr_0", "Veloc_viento_0", "TempTim_Pr_0", "Prod")]
chart.Correlation(corr_elhatico_1_1, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Elhatico_1_pt2.jpeg', width = 600, height = 600)
corr_elhatico_1_2 <- elhatico[, c("Temp_Mx_0","Temp_Mn_0", "Hr_Max_0", "Hr_Min_0", "Prec_Mx_0", "Prec_Mn_0", "Prod")]
chart.Correlation(corr_elhatico_1_2, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Elhatico_1_pt3.jpeg', width = 600, height = 600)
corr_elhatico_1_3 <- elhatico[, c("SR_Mx_0", "SR_Mn_0", "Veloc_vientoMx_0", "Veloc_vientoMn_0", "TempTim_Mx_0", "TempTim_Mn_0", "Prod")]
chart.Correlation(corr_elhatico_1_3, histogram=TRUE, pch=19)
dev.off()
corr_elhatico_1_3 <- cbind(corr_elhatico_1_3, elhatico[, c("Finca", "Vaca")])

#---------------------------------------------------------2 dias antes----------------------------------------------------------------

#agrosavia 2 antes

agrosavia_2antes <- do.call (rbind, indicadores(agrosavia, 2))
agrosavia_corre <- agrosavia_2antes
jpeg('Graficos/Articulo/Cor_Agrosavia_2_pt1.jpeg', width = 600, height = 600)
corr_agrosavia_2_1 <- agrosavia_corre[, c("Temp_Pr_0","HR_Pr_0", "Prec_Pr_0", "SR_Pr_0", "Veloc_viento_0", "TempTim_Pr_0", "Prod")]
chart.Correlation(corr_agrosavia_2_1 , histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Agrosavia_2_pt2.jpeg', width = 600, height = 600)
corr_agrosavia_2_2  <- agrosavia_corre[, c("Temp_Mx_0","Temp_Mn_0", "Hr_Max_0", "Hr_Min_0", "Prec_Mx_0", "Prec_Mn_0", "Prod")]
chart.Correlation(corr_agrosavia_2_2 , histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Agrosavia_2_pt3.jpeg', width = 600, height = 600)
corr_agrosavia_2_3 <- agrosavia_corre[, c("SR_Mx_0", "SR_Mn_0", "Veloc_vientoMx_0", "Veloc_vientoMn_0", "TempTim_Mx_0", "TempTim_Mn_0", "Prod")]
chart.Correlation(corr_agrosavia_2_3 , histogram=TRUE, pch=19)
dev.off()

corr_agrosavia_2_3 <- cbind(corr_agrosavia_2_3, agrosavia_corre[, c("Finca", "Vaca")])



#garzonero 2 antes
garzonero_2antes <- do.call (rbind, indicadores(garzonero, 2))
garzonero <- garzonero_2antes 
jpeg('Graficos/Articulo/Cor_Garzonero_2_pt1.jpeg', width = 600, height = 600)
corr_garzonero_2_1 <- garzonero[, c("Temp_Pr_0","HR_Pr_0", "Prec_Pr_0", "SR_Pr_0", "Veloc_viento_0", "TempTim_Pr_0", "Prod")]
chart.Correlation(corr_garzonero_2_1, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Garzonero_2_pt2.jpeg', width = 600, height = 600)
corr_garzonero_2_2 <- garzonero[, c("Temp_Mx_0","Temp_Mn_0", "Hr_Max_0", "Hr_Min_0", "Prec_Mx_0", "Prec_Mn_0", "Prod")]
chart.Correlation(corr_garzonero_2_2, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Garzonero_2_pt3.jpeg', width = 600, height = 600)
corr_garzonero_2_3 <- garzonero[, c("SR_Mx_0", "SR_Mn_0", "Veloc_vientoMx_0", "Veloc_vientoMn_0", "TempTim_Mx_0", "TempTim_Mn_0", "Prod")]
chart.Correlation(corr_garzonero_2_3, histogram=TRUE, pch=19)
dev.off()

corr_garzonero_2_3 <- cbind(corr_garzonero_2_3, garzonero[, c("Finca", "Vaca")])




#Hatico 2 dia antes
elhatico_2antes <- do.call (rbind, indicadores(elhatico, 2))
elhatico <- elhatico_2antes 
jpeg('Graficos/Articulo/Cor_Elhatico_2_pt1.jpeg', width = 600, height = 600)
corr_elhatico_2_1 <- elhatico[, c("Temp_Pr_0","HR_Pr_0", "Prec_Pr_0", "SR_Pr_0", "Veloc_viento_0", "TempTim_Pr_0", "Prod")]
chart.Correlation(corr_elhatico_2_1, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Elhatico_2_pt2.jpeg', width = 600, height = 600)
corr_elhatico_2_2 <- elhatico[, c("Temp_Mx_0","Temp_Mn_0", "Hr_Max_0", "Hr_Min_0", "Prec_Mx_0", "Prec_Mn_0", "Prod")]
chart.Correlation(corr_elhatico_2_2, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Elhatico_2_pt3.jpeg', width = 600, height = 600)
corr_elhatico_2_3 <- elhatico[, c("SR_Mx_0", "SR_Mn_0", "Veloc_vientoMx_0", "Veloc_vientoMn_0", "TempTim_Mx_0", "TempTim_Mn_0", "Prod")]
chart.Correlation(corr_elhatico_2_3, histogram=TRUE, pch=19)
dev.off()

corr_elhatico_2_3 <- cbind(corr_elhatico_2_3, elhatico[, c("Finca", "Vaca")])

#----------------------------------------------------------3 dias antes-------------------------------------------------------

#agrosavia 3 antes

agrosavia_3antes <- do.call (rbind, indicadores(agrosavia, 3))
agrosavia_corre <- agrosavia_3antes
jpeg('Graficos/Articulo/Cor_Agrosavia_3_pt1.jpeg', width = 600, height = 600)
corr_agrosavia_3_1 <- agrosavia_corre[, c("Temp_Pr_0","HR_Pr_0", "Prec_Pr_0", "SR_Pr_0", "Veloc_viento_0", "TempTim_Pr_0", "Prod")]
chart.Correlation(corr_agrosavia_3_1 , histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Agrosavia_3_pt2.jpeg', width = 600, height = 600)
corr_agrosavia_3_2  <- agrosavia_corre[, c("Temp_Mx_0","Temp_Mn_0", "Hr_Max_0", "Hr_Min_0", "Prec_Mx_0", "Prec_Mn_0", "Prod")]
chart.Correlation(corr_agrosavia_3_2 , histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Agrosavia_3_pt3.jpeg', width = 600, height = 600)
corr_agrosavia_3_3  <- agrosavia_corre[, c("SR_Mx_0", "SR_Mn_0", "Veloc_vientoMx_0", "Veloc_vientoMn_0", "TempTim_Mx_0", "TempTim_Mn_0", "Prod")]
chart.Correlation(corr_agrosavia_3_3, histogram=TRUE, pch=19)
dev.off()

corr_agrosavia_3_3 <- cbind(corr_agrosavia_3_3, agrosavia_corre[, c("Finca", "Vaca")])


#garzonero 3 antes
garzonero_3antes <- do.call (rbind, indicadores(garzonero, 3))
garzonero <- garzonero_3antes 
jpeg('Graficos/Articulo/Cor_Garzonero_3_pt1.jpeg', width = 600, height = 600)
corr_garzonero_3_1 <- garzonero[, c("Temp_Pr_0","HR_Pr_0", "Prec_Pr_0", "SR_Pr_0", "Veloc_viento_0", "TempTim_Pr_0", "Prod")]
chart.Correlation(corr_garzonero_3_1 , histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Garzonero_3_pt2.jpeg', width = 600, height = 600)
corr_garzonero_3_2  <- garzonero[, c("Temp_Mx_0","Temp_Mn_0", "Hr_Max_0", "Hr_Min_0", "Prec_Mx_0", "Prec_Mn_0", "Prod")]
chart.Correlation(corr_garzonero_3_2 , histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Garzonero_3_pt3.jpeg', width = 600, height = 600)
corr_garzonero_3_3 <- garzonero[, c("SR_Mx_0", "SR_Mn_0", "Veloc_vientoMx_0", "Veloc_vientoMn_0", "TempTim_Mx_0", "TempTim_Mn_0", "Prod")]
chart.Correlation(corr_garzonero_3_3, histogram=TRUE, pch=19)
dev.off()
corr_garzonero_3_3 <- cbind(corr_garzonero_3_3, garzonero[, c("Finca", "Vaca")])

#Hatico 3 dia antes
elhatico_3antes <- do.call (rbind, indicadores(elhatico, 3))
elhatico <- elhatico_3antes 
jpeg('Graficos/Articulo/Cor_Elhatico_3_pt1.jpeg', width = 600, height = 600)
corr_elhatico_3_1  <- elhatico[, c("Temp_Pr_0","HR_Pr_0", "Prec_Pr_0", "SR_Pr_0", "Veloc_viento_0", "TempTim_Pr_0", "Prod")]
chart.Correlation(corr_elhatico_3_1 , histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Elhatico_3_pt2.jpeg', width = 600, height = 600)
corr_elhatico_3_2 <- elhatico[, c("Temp_Mx_0","Temp_Mn_0", "Hr_Max_0", "Hr_Min_0", "Prec_Mx_0", "Prec_Mn_0", "Prod")]
chart.Correlation(corr_elhatico_3_2, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Elhatico_3_pt3.jpeg', width = 600, height = 600)
corr_elhatico_3_3 <- elhatico[, c("SR_Mx_0", "SR_Mn_0", "Veloc_vientoMx_0", "Veloc_vientoMn_0", "TempTim_Mx_0", "TempTim_Mn_0", "Prod")]
chart.Correlation(corr_elhatico_3_3, histogram=TRUE, pch=19)
dev.off()
corr_elhatico_3_3 <- cbind(corr_elhatico_3_3, elhatico[, c("Finca", "Vaca")])

#---------------------------------- 5 dias antes---------------------------------------------------------#

#agrosavia 5 antes

agrosavia_5antes <- do.call (rbind, indicadores(agrosavia, 5))
agrosavia_corre <- agrosavia_5antes
jpeg('Graficos/Articulo/Cor_Agrosavia_5_pt1.jpeg', width = 600, height = 600)
corr_agrosavia_5_1 <- agrosavia_corre[, c("Temp_Pr_0","HR_Pr_0", "Prec_Pr_0", "SR_Pr_0", "Veloc_viento_0", "TempTim_Pr_0", "Prod")]
chart.Correlation(corr_agrosavia_5_1, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Agrosavia_5_pt2.jpeg', width = 600, height = 600)
corr_agrosavia_5_2 <- agrosavia_corre[, c("Temp_Mx_0","Temp_Mn_0", "Hr_Max_0", "Hr_Min_0", "Prec_Mx_0", "Prec_Mn_0", "Prod")]
chart.Correlation(corr_agrosavia_5_2, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Agrosavia_5_pt3.jpeg', width = 600, height = 600)
corr_agrosavia_5_3 <- agrosavia_corre[, c("SR_Mx_0", "SR_Mn_0", "Veloc_vientoMx_0", "Veloc_vientoMn_0", "TempTim_Mx_0", "TempTim_Mn_0", "Prod")]
chart.Correlation(corr_agrosavia_5_3, histogram=TRUE, pch=19)
dev.off()


corr_agrosavia_5_3 <- cbind(corr_agrosavia_5_3, agrosavia_corre[, c("Finca", "Vaca")])


#garzonero 5 antes
garzonero_5antes <- do.call (rbind, indicadores(garzonero, 5))
garzonero <- garzonero_5antes 
jpeg('Graficos/Articulo/Cor_Garzonero_5_pt1.jpeg', width = 600, height = 600)
corr_garzonero_5_1 <- garzonero[, c("Temp_Pr_0","HR_Pr_0", "Prec_Pr_0", "SR_Pr_0", "Veloc_viento_0", "TempTim_Pr_0", "Prod")]
chart.Correlation(corr_garzonero_5_1, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Garzonero_5_pt2.jpeg', width = 600, height = 600)
corr_garzonero_5_2 <- garzonero[, c("Temp_Mx_0","Temp_Mn_0", "Hr_Max_0", "Hr_Min_0", "Prec_Mx_0", "Prec_Mn_0", "Prod")]
chart.Correlation(corr_garzonero_5_2, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Garzonero_5_pt3.jpeg', width = 600, height = 600)
corr_garzonero_5_3 <- garzonero[, c("SR_Mx_0", "SR_Mn_0", "Veloc_vientoMx_0", "Veloc_vientoMn_0", "TempTim_Mx_0", "TempTim_Mn_0", "Prod")]
chart.Correlation(corr_garzonero_5_3, histogram=TRUE, pch=19)
dev.off()

corr_garzonero_5_3 <- cbind(corr_garzonero_5_3, garzonero[, c("Finca", "Vaca")])


#Hatico 5 dia antes
elhatico_5antes <- do.call (rbind, indicadores(elhatico, 5))
elhatico <- elhatico_5antes 
jpeg('Graficos/Articulo/Cor_Elhatico_5_pt1.jpeg', width = 600, height = 600)
corr_elhatico_5_1 <- elhatico[, c("Temp_Pr_0","HR_Pr_0", "Prec_Pr_0", "SR_Pr_0", "Veloc_viento_0", "TempTim_Pr_0", "Prod")]
chart.Correlation(corr_elhatico_5_1, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Elhatico_5_pt2.jpeg', width = 600, height = 600)
corr_elhatico_5_2 <- elhatico[, c("Temp_Mx_0","Temp_Mn_0", "Hr_Max_0", "Hr_Min_0", "Prec_Mx_0", "Prec_Mn_0", "Prod")]
chart.Correlation(corr_elhatico_5_2, histogram=TRUE, pch=19)
dev.off()

jpeg('Graficos/Articulo/Cor_Elhatico_5_pt3.jpeg', width = 600, height = 600)
corr_elhatico_5_3 <- elhatico[, c("SR_Mx_0", "SR_Mn_0", "Veloc_vientoMx_0", "Veloc_vientoMn_0", "TempTim_Mx_0", "TempTim_Mn_0", "Prod")]
chart.Correlation(corr_elhatico_5_3, histogram=TRUE, pch=19)
dev.off()

corr_elhatico_5_3 <- cbind(corr_elhatico_5_3, elhatico[, c("Finca", "Vaca")])










#------------------------------Unificacion para los Modelos--------------------------------------------------------------##



unir_por_dias <- function (corr_agrosavia_0_1, corr_agrosavia_0_2, corr_agrosavia_0_3, corr_garzonero_0_1, corr_garzonero_0_2, corr_garzonero_0_3, corr_elhatico_0_1, 
                           corr_elhatico_0_2, corr_elhatico_0_3)
{
  

primera_parte <- data.frame (rbind(corr_agrosavia_0_1, corr_garzonero_0_1, corr_elhatico_0_1))
segunda_parte <- data.frame (rbind(corr_agrosavia_0_2, corr_garzonero_0_2, corr_elhatico_0_2))
tercera_parte <- data.frame (rbind(corr_agrosavia_0_3, corr_garzonero_0_3, corr_elhatico_0_3))

aux <- cbind(primera_parte, segunda_parte)
#Quitar Prod
index <- which(colnames(aux)=="Prod")
aux <- aux[, -index]

total <- cbind(aux, tercera_parte)
total <- na.omit(total)
colnames(total)


total$ITH <- total$Temp_Pr_0 + (total$HR_Pr_0*(total$Temp_Pr_0-14.4)) + 46.4

return(total)
}


dia_0 <- unir_por_dias(corr_agrosavia_0_1, corr_agrosavia_0_2, corr_agrosavia_0_3, corr_garzonero_0_1, corr_garzonero_0_2, corr_garzonero_0_3, corr_elhatico_0_1, 
              corr_elhatico_0_2, corr_elhatico_0_3)

dia_1 <- unir_por_dias(corr_agrosavia_1_1, corr_agrosavia_1_2, corr_agrosavia_1_3, corr_garzonero_1_1, corr_garzonero_1_2, corr_garzonero_1_3, corr_elhatico_1_1, 
                       corr_elhatico_1_2, corr_elhatico_1_3)

dia_2 <- unir_por_dias(corr_agrosavia_2_1, corr_agrosavia_2_2, corr_agrosavia_2_3, corr_garzonero_2_1, corr_garzonero_2_2, corr_garzonero_2_3, corr_elhatico_2_1, 
                       corr_elhatico_2_2, corr_elhatico_2_3)

dia_3 <- unir_por_dias(corr_agrosavia_3_1, corr_agrosavia_3_2, corr_agrosavia_3_3, corr_garzonero_3_1, corr_garzonero_3_2, corr_garzonero_3_3, corr_elhatico_3_1, 
                       corr_elhatico_3_2, corr_elhatico_3_3)

dia_5 <- unir_por_dias(corr_agrosavia_5_1, corr_agrosavia_5_2, corr_agrosavia_5_3, corr_garzonero_5_1, corr_garzonero_5_2, corr_garzonero_3_3, corr_elhatico_3_1, 
                       corr_elhatico_5_2, corr_elhatico_5_3)


save(dia_0, file= "Data/dia_0_VacaFinca.RData")
save(dia_1, file= "Data/dia_1_VacaFinca.RData")
save(dia_2, file= "Data/dia_2_VacaFinca.RData")
save(dia_3, file= "Data/dia_3_VacaFinca.RData")
save(dia_5, file= "Data/dia_5_VacaFinca.RData")

#----------------------Temperatura Timpanica--------------------------------------------------#



Graficas_Correlacion_Temptim <- function (produccion_marruecos_2, path = "Graficos/Temperatura Timpanica/Correlacion/", finca= "Marruecos")
{
    #union_datos <- do.call(rbind, produccion_marruecos_2)
    union_datos <-produccion_marruecos_2
    jpeg(paste0(path,finca, "_Corr.jpeg"), width = 600, height = 600)
    corr_elhatico_5_1 <- union_datos[, c("Temp_Timpani","Temperatura", "Humedad Relativa", "Precipitacion", "SR", "VientoVelocidad")]
    chart.Correlation(corr_elhatico_5_1, histogram=TRUE, pch=19)
    dev.off()
    
  
}

load("Data/Produccion_Clima_TempTim_Marruecos.RData")
load("Data/Produccion_Clima_TempTim_Elhatico.RData")
load("Data/Produccion_Clima_TempTim_garzonero.RData")
load("Data/Produccion_Clima_TempTimAgrosavia.RData")


Graficas_Correlacion_Temptim (produccion_agrosavia_2, finca = "agrosavia")  
Graficas_Correlacion_Temptim (produccion_garzonero_2, finca = "garzonero")  
Graficas_Correlacion_Temptim (produccion_hatico_2, finca = "hatico")  
Graficas_Correlacion_Temptim (produccion_marruecos_2, finca = "marruecos")  


aux1 <- rbind (produccion_garzonero_2, produccion_agrosavia_2)
aux2 <- rbind (produccion_hatico_2, produccion_marruecos_2)
total <- rbind(aux1, aux2)
# garzonero <- do.call(rbind, produccion_garzonero_2)
# hatico <- do.call(rbind, produccion_hatico_2)
# marruecos <- do.call(rbind, produccion_marruecos_2)
# 
# aux_1 <- rbind(agrosava, garzonero )
# aux_2 <- rbind(hatico, marruecos )
# total <- rbind(aux_1 , aux_2 )

save(total, file= "Data/Total_Fincas_TemperaturaTimp_Clima.RData")
