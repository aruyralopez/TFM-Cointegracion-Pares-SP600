library(quantmod)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape)
library(grid)
library(PerformanceAnalytics)
library(car)
library(urca)
library(lubridate)

#filtro_activos <- read.csv("/Users/Alejandro/OneDrive/Escritorio/TFM-32/datos/filtro_activos.csv", sep = ";", header = TRUE)
filtro_activos2 <- read.csv("/Users/Alejandro/OneDrive/Escritorio/TFM-32/datos/filtro_activos2.csv")
sectores <- read.csv("/Users/Alejandro/OneDrive/Escritorio/TFM-32/datos/sp600.csv", sep = ";", header = TRUE)
path_output= "/Users/Alejandro/OneDrive/Escritorio/TFM-32/datos"

## Left Join
# le cambiamos el nombre a la columna para poder realizar el join
colnames(filtro_activos2)[colnames(filtro_activos2) == "x"] <- "symbol"
spcoin <- merge(filtro_activos2, sectores, by = "symbol", all.x = TRUE)


#############################################################################
## Funcion Sectores

sector_function <- function(activo) {
  Activox= activo[,c("symbol")]
  Activoy= activo[,c("symbol")]
  result <- data.frame()
  for (activo1 in Activox) { 
    for (activo2 in Activoy) { 
      if(activo1 != activo2) {
        stocks<- c(activo1,activo2)
        date_begin <- as.Date("2005-01-01")
        date_end <- as.Date("2022-10-31")
        
        # retrive data of all stocks
        tickers <- getSymbols(stocks, from=date_begin, to=date_end, auto.assign=TRUE)
        
        # combine the adjusted close values in one (xts) data.frame
        ETFS <- Cl(to.daily(Ad(get(tickers[1]))))
        for (i in 2:length(tickers)) { ETFS <- merge(ETFS, Cl(to.daily(Ad(get(tickers[i]))))) }
        colnames(ETFS) <- c(activo1,activo2)
        
        returns.data <- CalculateReturns(ETFS,method="discrete")
        colnames(returns.data)=c(paste0("Rent_",activo1),paste0("Rent_",activo2))
        ETFS=cbind(ETFS,returns.data)
        ETFS=as.data.frame(ETFS)
        
        ETFS$Date=rownames(ETFS)
        ETFS$Date <- ymd(ETFS$Date)
        
        ETFS$mes=month(ETFS$Date)
        ETFS$semester=semester(ETFS$Date)
        ETFS$fechaaux=paste0(year(ETFS$Date),ETFS$semester)
        
        # Creación de tablas por años
        vector_fechas= c("20051","20052","20061","20062","20071","20072","20081","20082","20091","20092","20101","20102",
                         "20111","20112","20121","20122","20131","20132","20141","20142","20151","20152","20161","20162",
                         "20171","20172","20181","20182","20191","20192","20201","20202","20211","20212","20221","20222")
        
        for (f in 1:length(vector_fechas)) {
          
          # creamos una tabla con los registros correspondientes a esa fecha
          tabla=ETFS[ETFS$fechaaux==vector_fechas[f],]
          
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
                   df@teststat[1],df@cval[1],df@cval[2],df@cval[3])
          
          # conversión en data.frame y transponer tabla
          vector=as.data.frame(vector)
          vector <- t(vector[,1:1])
          colnames(vector)=c("intercepto","pendiente","t","t1","t5","t10")
          vector=as.data.frame(vector)
          
          # evaluacion de cointegracion en el año
          vector$Res_t1 <- case_when(vector$t1 < vector$t ~ "ko", # 
                                     vector$t1 > vector$t ~ "ok" , TRUE ~ "na") # se rechaza Ho=existencia de raiz unitaria (tendencia), es decir,
          
          #la relacion entre las series es estacionaria
          vector$Res_t5 <- case_when(vector$t5 < vector$t ~ "ko", 
                                     vector$t5 > vector$t ~ "ok" , TRUE ~ "na")
          
          vector$Res_t10 <- case_when(vector$t10 < vector$t ~ "ko", 
                                      vector$t10 > vector$t ~ "ok" , TRUE ~ "na")
          
          vector$par=paste0(activo2,"_",activo1)
          vector$pary=activo2
          vector$parx=activo1
          vector$fecha=vector_fechas[f]
          
          if (f == 1){
            tab_f <- vector
            
          }else if (f != 1 ) {
            tab_f <- rbind(tab_f,vector)
          }
        }
        # Guardado de la tabla del par analizado
        result <- rbind(result, tab_f) 
      }
    }
  }
  return(result)
}
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


#rm()
