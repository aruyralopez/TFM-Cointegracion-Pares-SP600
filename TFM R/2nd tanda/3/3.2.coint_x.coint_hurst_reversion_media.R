library(quantmod)
library(lubridate)
library(pracma)
library(dplyr)
library(ggplot2)
library(reshape)
library(grid)
library(PerformanceAnalytics)
library(car)
library(urca)

filtro_activos2 <- read.csv("/Users/Alejandro/OneDrive/Escritorio/TFM-2/datos/filtro_activos2.csv")
sectores <- read.csv("/Users/Alejandro/OneDrive/Escritorio/TFM-2/datos/sp600.csv", sep = ";", header = TRUE)
path_output= "/Users/Alejandro/OneDrive/Escritorio/TFM-2/datos"

## Left Join
# le cambiamos el nombre a la columna para poder realizar el join
colnames(filtro_activos2)[colnames(filtro_activos2) == "x"] <- "symbol"
spcoin <- merge(filtro_activos2, sectores, by = "symbol", all.x = TRUE)


#############################################################################
sector_function <- function(activo) {
  activox= activo[,c("symbol")]
  activoy= activo[,c("symbol")]
  result <- data.frame()
  
  for(Activox in activox){
    for(Activoy in activoy){
      if(Activox != Activoy) {
        stocks<- c(Activox,Activoy)
        
        date_begin <- as.Date("2005-01-01")
        date_end <- as.Date("2022-10-31")
        
        # retrive data of all stocks
        tickers <- getSymbols(stocks, from=date_begin, to=date_end, auto.assign=TRUE)
        
        # combine the adjusted close values in one (xts) data.frame
        ETFS <- Cl(to.daily(Ad(get(tickers[1]))))
        
        for (s in 2:length(tickers)) { ETFS <- merge(ETFS, Cl(to.daily(Ad(get(tickers[s]))))) }
        
        colnames(ETFS) <- c(Activox,Activoy)
        ETFS=na.omit(ETFS)
        ETFS <- ETFS[ETFS[,2]>=0,]
        
        returns.data <- CalculateReturns(ETFS,method="discrete")
        colnames(returns.data)=c(paste0("Rentab_",Activox),paste0("Rentab_",Activoy))
        ETFS=cbind(ETFS,returns.data)
        
        ETFS=as.data.frame(ETFS)
        
        #Creación de variable fecha
        ETFS$Date=rownames(ETFS)
        ETFS$Date <- ymd(ETFS$Date)
        ETFS$year = year(ETFS$Date)
        ETFS$semester=semester(ETFS$Date)
        ETFS$fecha=paste0(year(ETFS$Date),ETFS$semester)
        
        
        # Creación de tablas por años
        vector_fechas= c("20051","20052","20061","20062","20071","20072","20081","20082","20091","20092","20101","20102",
                         "20111","20112","20121","20122","20131","20132","20141","20142","20151","20152","20161","20162",
                         "20171","20172","20181","20182","20191","20192","20201","20202","20211","20212","20221","20222")
        
        for (f in 1:length(vector_fechas)) {
          vector_fechas[f]
          
          # creamos una tabla con los registros correspondientes a esa fecha
          tabla=ETFS[ETFS$fecha==vector_fechas[f],]
          
          # regresión entre ambos activos
          modelo = lm(tabla[,2]~tabla[,1], data=tabla)
          
          # capturamos residuos de la regresión
          residuales=modelo$residuals
          
          #Test de dicky-fuller (automativo con 10 lags. La función selecciona automaticamente el numero de lags optimo)
          df=ur.df(residuales,lags=10,selectlags = "AIC")
          
          # Captura de la pendiente y el intercepto
          intercept=modelo$coefficients[1]
          slope=modelo$coefficients[2]
          
          # Creación tabla resumen para la fecha
          vector=c(modelo$coefficients[1],modelo$coefficients[2],
                   df@teststat[1],df@cval[1])
          
          # conversión en data.frame y transponer tabla
          vector=as.data.frame(vector)
          vector <- t(vector[,1:1])
          colnames(vector)=c("intercepto","pendiente","t","t1")
          vector=as.data.frame(vector)
          
          # evaluacion de cointegracion en el año
          vector$Res_t1 <- case_when(vector$t1 < vector$t ~ "ko", # 
                                     vector$t1 > vector$t ~ "ok" , TRUE ~ "na") # se rechaza Ho=existencia de raiz unitaria (tendencia), es decir,
          
          vector$par=paste0(Activoy,"_",Activox)
          vector$pary=Activoy
          vector$parx=Activox
          vector$fecha=vector_fechas[f]
          if (f == 1){
            tab_f <- vector
            
          }else if (f != 1 ) {
            tab_f <- rbind(tab_f,vector)
          }
        }
        
        tab_f$fecha=as.numeric(tab_f$fecha)
        
        tab_f$interceptolag= dplyr::lag(tab_f$intercepto,n=1)
        tab_f$pendientelag=  dplyr::lag(tab_f$pendiente,n=1)
        
        #Join de ETFS con tab_f para incorporar interceptos y pendientes
        ETFS$fecha=as.numeric(ETFS$fecha)
        ETFS = left_join(ETFS, tab_f ,by = c("fecha"))
        rm(tab_f)
        row.names(ETFS)=ETFS$Date
        
        # Calculo de la variable cointegrada
        ETFS$Var_cointeg= ETFS$interceptolag+ETFS$pendientelag*ETFS[,1]
        
        # Diferencia entre cointegracion y valor real (spread)
        
        ETFS$diff_coint_real= ETFS$Var_cointeg-ETFS[,2]
        
        # Media spread. La que se usa para num veces que revierte y duracion hasta que revierte
        ETFS$media_spread=c(NA,SMA(ETFS$diff_coint_real,120, ))[1:nrow(ETFS)]
        
        ETFS$diff_conmedia=ETFS$diff_coint_real-ETFS$media_spread
        
        ETFS$CONT=0
        ETFS$aux=0
        ETFS$aux2=0
        ETFS$dias=0
        
        for (fila in 2:nrow(ETFS)) {
          ETFS$CONT[fila]= ifelse((ETFS$diff_conmedia[(fila-1)]>0 & ETFS$diff_conmedia[(fila)]<0) | ETFS$diff_conmedia[(fila-1)]<0 & ETFS$diff_conmedia[(fila)]>0,1,0)
          ETFS$aux[fila]=ifelse(ETFS$CONT[(fila-1)]==1,1,0)
          ETFS$aux2[fila]=ifelse(ETFS$aux[fila]==1,1,ETFS$aux2[(fila-1)]+1)
          ETFS$dias[fila]=ifelse(ETFS$CONT[fila]==1,ETFS$aux2[fila],0)
        }
        
        #tabla cointegracion
        tc=ETFS[,c("intercepto", "pendiente","fecha","t","t1","Res_t1")]
        tc$par=paste0(Activoy,"_",Activox)
        tc$pary=paste0(Activoy)
        tc$parx=paste0(Activox)
        
        #quitar duplicados
        tc=distinct(tc)
        
        ###############
        #hurst no aplica a resultados finales
        
        ETFS$spread=ETFS$diff_coint_real+10
        vector_fechas
        #se empieza en el 2 semestre ya que el primero tien missing data
        for (f in 2:length(vector_fechas)) {
          print(f)
          
          tabla_hurst=ETFS[ETFS$fecha==vector_fechas[f],]
          
          hurst_30=hurstexp(tabla_hurst$spread, d = 30)
          hurst_60=hurstexp(tabla_hurst$spread, d = 60)
          hurst_90=hurstexp(tabla_hurst$spread, d = 90)
          hurst_120=hurstexp(tabla_hurst$spread, d = 120)
          
          par=paste0(Activoy,"_",Activox)
          th=t(c(par,vector_fechas[f],hurst_30$Ht,hurst_60$Ht,hurst_90$Ht,hurst_120$Ht))
          
          colnames(th)=c("par","fecha","hurst_30","hurst_60","hurst_90","hurst_120")
          th=as.data.frame(th)
          
          if (f == 2){
            th_comp <- th
          }
          else if (f != 2 ) {
            th_comp <- rbind(th_comp,th)
          }
        }
        #####################################################
        
        # Creacion tabla semestral para reversion a media y duracion
        tm=ETFS
        tm$CONT[is.na(tm$CONT)] = 0
        tm$dias[is.na(tm$dias)] = 0
        tab_dur_media=ETFS[tm$dias>0,]
        
        num_veces <- tm %>%  group_by(fecha) %>%  summarize(num_veces=sum(CONT))
        
        dur_media <- tab_dur_media %>%  group_by(fecha) %>% summarize(dur_media=mean(dias))         
        tm=full_join(num_veces,dur_media,by="fecha")
        tm$dur_media[is.na(tm$dur_media)] = 0
        
        
        tm$par <- paste0(Activoy,"_",Activox)
        tm=tm[,c("par","fecha","num_veces","dur_media")]
        
        #consolidacion
        tc$fecha=as.character(tc$fecha)
        tm$fecha=as.character(tm$fecha)
        
        tcons=left_join(tc,th_comp,by=c("par","fecha"))
        tcons=left_join(tcons,tm,by=c("par","fecha"))
        
        result <- rbind(result, tcons)
      }# cierre del for del activoy
    }# cierre del for del activox
  }# cierre if
  return(result)
}# cierre funcion sector_function
#############################################################################

## 11 sectores
table(spcoin$sector)

#1 Communication Services (7 index)
Communication_Services <- subset(spcoin, sector == "Communication Services")
result_Communication_Services <- sector_function(Communication_Services)
write.csv(result_Communication_Services,file.path(path_output,"result_Communication_Services.csv"),row.names = F)


#2 Consumer Discretionary (57 index)
Consumer_Discretionary <- subset(spcoin, sector == "Consumer Discretionary")
result_Consumer_Discretionary <- sector_function(Consumer_Discretionary)
write.csv(result_Consumer_Discretionary,file.path(path_output,"result_Consumer_Discretionary.csv"),row.names = F)


#3 Consumer Staples (23 index)
Consumer_Staples <- subset(spcoin, sector == "Consumer Staples")
result_Consumer_Staples <- sector_function(Consumer_Staples)
write.csv(result_Consumer_Staples,file.path(path_output,"result_Consumer_Staples.csv"),row.names = F)


#4 Energy (15 index)
Energy <- subset(spcoin, sector == "Energy")
result_Energy <- sector_function(Energy)
write.csv(result_Energy,file.path(path_output,"result_Energy.csv"),row.names = F)


#5 Financials (66 index)
Financials <- subset(spcoin, sector == "Financials")
result_Financials <- sector_function(Financials)
write.csv(result_Financials,file.path(path_output,"result_Financials.csv"),row.names = F)


#6 Health Care (40 index)
Health_Care <- subset(spcoin, sector == "Health Care")
result_Health_Care <- sector_function(Health_Care)
write.csv(result_Health_Care,file.path(path_output,"result_Health_Care.csv"),row.names = F)


#7 Industrials (71 index)
Industrials <- subset(spcoin, sector == "Industrials")
result_Industrials <- sector_function(Industrials)
write.csv(result_Industrials,file.path(path_output,"result_Industrials.csv"),row.names = F)


#8 Information Technology (51 index)
Information_Technology <- subset(spcoin, sector == "Information Technology")
result_Information_Technology <- sector_function(Information_Technology)
write.csv(result_Information_Technology,file.path(path_output,"result_Information_Technology.csv"),row.names = F)


#9 Materials (19 index)
Materials <- subset(spcoin, sector == "Materials")
result_Materials <- sector_function(Materials)
write.csv(result_Materials,file.path(path_output,"result_Materials.csv"),row.names = F)


#10 Real Estate (19 index)
Real_Estate <- subset(spcoin, sector == "Real Estate")
result_Real_Estate <- sector_function(Real_Estate)
write.csv(result_Real_Estate,file.path(path_output,"result_Real_Estate.csv"),row.names = F)


#11 Utilities (8 index)
Utilities <- subset(spcoin, sector == "Utilities")
result_Utilities <- sector_function(Utilities)
write.csv(result_Utilities,file.path(path_output,"result_Utilities.csv"),row.names = F)

