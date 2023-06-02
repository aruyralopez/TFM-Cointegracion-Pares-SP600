library(quantmod)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape)
library(grid)
library(PerformanceAnalytics)
library(car)
library(urca)

path_output= "/Users/Alejandro/OneDrive/Escritorio/TFM-32/datos"

################################################################################################
## Funcion EqCurve en RDs diario

funcion_eqc <- function(df,df3) {
  
  unique_vector <- unique(df3$par)
  
  for(i in unique_vector){
    substraction <- subset(df, par==i)

    Activox=substraction$parx[1]
    Activoy=substraction$pary[1]
    stocks<- c(Activox,Activoy)
    
    date_begin <- as.Date("2005-01-01")
    date_end <- as.Date("2022-10-31")
    
    tickers <- getSymbols(stocks, from=date_begin, to=date_end, auto.assign=TRUE)

    ETFS <- Cl(to.daily(Ad(get(tickers[1]))))
    
    for (j in 2:length(tickers)) { ETFS <- merge(ETFS, Cl(to.daily(Ad(get(tickers[j]))))) }
    colnames(ETFS) <- c(Activox,Activoy)
    ETFS=na.omit(ETFS)
    ETFS <- ETFS[ETFS[,2]>=0,]

    returns.data <- CalculateReturns(ETFS,method="discrete")
    colnames(returns.data)=c(paste0("Rentab_",Activox),paste0("Rentab_",Activoy))
    ETFS=cbind(ETFS,returns.data)
    ETFS=as.data.frame(ETFS)
    
    ETFS$Date=rownames(ETFS)
    ETFS$Date <- ymd(ETFS$Date)
    ETFS$year = year(ETFS$Date)
    ETFS$semester=semester(ETFS$Date)
    ETFS$fecha=paste0(year(ETFS$Date),ETFS$semester)
    
    tab_f=substraction[,c("intercepto","pendiente","fecha")]
    tab_f$fecha=as.numeric(tab_f$fecha)
    
    tab_f$interceptolag= dplyr::lag(tab_f$intercepto,n=1)
    tab_f$pendientelag=  dplyr::lag(tab_f$pendiente,n=1)
    
    ETFS$fecha=as.numeric(ETFS$fecha)
    ETFS = left_join(ETFS, tab_f ,by = c("fecha"))
    row.names(ETFS)=ETFS$Date 
    ETFS$Var_cointeg= ETFS$interceptolag+ETFS$pendientelag*ETFS[,1]
    ETFS$diff_coint_real= ETFS$Var_cointeg-ETFS[,2]

    ETFS$diff_coint_real_lag=dplyr::lag(ETFS$diff_coint_real,n=1)
    ETFS$signal_activoy <- case_when(ETFS$diff_coint_real_lag>0  ~ 1,
                                     ETFS$diff_coint_real_lag<=0~ -1, TRUE ~ 0)
    ETFS$signal_activox <- case_when(ETFS$diff_coint_real_lag<0  ~ 1,
                                     ETFS$diff_coint_real_lag>=0 ~ -1, TRUE ~ 0)
    
    ETFS$rentab_estrat_activoy= ETFS$signal_activoy*ETFS[,4]
    ETFS$rentab_estrat_activox= ETFS$signal_activox*ETFS[,3]
    ETFS$rentab_estrategia=(0.5*ETFS$rentab_estrat_activoy)+(0.5*ETFS$rentab_estrat_activox)
    ETFS$eqcurve_estrategia=100
    
    for (fila in 2:nrow(ETFS)) {
      ETFS$eqcurve_estrategia[fila]=(ETFS$eqcurve_estrategia[(fila-1)]*(1+ETFS$rentab_estrategia[fila]))
    }
    #names(ETFS)[names(ETFS) == "eqcurve_estrategia"] <- paste0(i)
    ETFS <- ETFS[,c("Date","eqcurve_estrategia")]
    saveRDS(ETFS, file.path(ruta_salida, paste0(i,".rds")))
  }
}

################################################################################################
#tt <- readRDS("/Users/Alejandro/OneDrive/Escritorio/TFM-32/output2/Communication_Services/ATNI_CCOI.rds")
################################################################################################


### 11 sectores

## 1 Communication Services
ruta_salida="/Users/Alejandro/OneDrive/Escritorio/TFM-32/output2/Communication_Services"
result_Communication_Services <- read.csv(file.path(path_output,"result_Communication_Services.csv"))
rcs3 <- read.csv(file.path(path_output,"rcs3.csv"))

funcion_eqc(result_Communication_Services,rcs3)

## 2 Consumer Discretionary
ruta_salida="/Users/Alejandro/OneDrive/Escritorio/TFM-32/output2/Consumer_Discretionary"
result_Consumer_Discretionary <- read.csv(file.path(path_output,"result_Consumer_Discretionary.csv"))
rcd3 <- read.csv(file.path(path_output,"rcd3.csv"))

funcion_eqc(result_Consumer_Discretionary,rcd3)

## 3 Consumer Staples (23 index)
ruta_salida="/Users/Alejandro/OneDrive/Escritorio/TFM-32/output2/Consumer_Staples"
result_Consumer_Staples <- read.csv(file.path(path_output,"result_Consumer_Staples.csv"))
rcst3 <- read.csv(file.path(path_output,"rcst3.csv"))

funcion_eqc(result_Consumer_Staples,rcst3)

## 4 Energy 
ruta_salida="/Users/Alejandro/OneDrive/Escritorio/TFM-32/output2/Energy"
result_Energy <- read.csv(file.path(path_output,"result_Energy.csv"))
re3 <- read.csv(file.path(path_output,"re3.csv"))

funcion_eqc(result_Energy,re3)

## 5 Financials
ruta_salida="/Users/Alejandro/OneDrive/Escritorio/TFM-32/output2/Financials"
result_Financials <- read.csv(file.path(path_output,"result_Financials.csv"))
rf3 <- read.csv(file.path(path_output,"rf3.csv"))

funcion_eqc(result_Financials,rf3)

## 6 Health Care
ruta_salida="/Users/Alejandro/OneDrive/Escritorio/TFM-32/output2/Health_Care"
result_Health_Care <- read.csv(file.path(path_output,"result_Health_Care.csv"))
rhc3 <- read.csv(file.path(path_output,"rhc3.csv"))

funcion_eqc(result_Health_Care,rhc3)

## 7 Industrials
ruta_salida="/Users/Alejandro/OneDrive/Escritorio/TFM-32/output2/Industrials"
result_Industrials <- read.csv(file.path(path_output,"result_Industrials.csv"))
ri3 <- read.csv(file.path(path_output,"ri3.csv"))

funcion_eqc(result_Industrials,ri3)

## 8 Information Technology 
ruta_salida="/Users/Alejandro/OneDrive/Escritorio/TFM-32/output2/Information_Technology"
result_Information_Technology <- read.csv(file.path(path_output,"result_Information_Technology.csv"))
rit3 <- read.csv(file.path(path_output,"rit3.csv"))

funcion_eqc(result_Information_Technology,rit3)

## 9 Materials
ruta_salida="/Users/Alejandro/OneDrive/Escritorio/TFM-32/output2/Materials"
result_Materials <- read.csv(file.path(path_output,"result_Materials.csv"))
rm3 <- read.csv(file.path(path_output,"rm3.csv"))

funcion_eqc(result_Materials,rm3)

## 10 Real Estate (19 index)
ruta_salida="/Users/Alejandro/OneDrive/Escritorio/TFM-32/output2/Real_Estate"
result_Real_Estate <- read.csv(file.path(path_output,"result_Real_Estate.csv"))
rre3 <- read.csv(file.path(path_output,"rre3.csv"))

funcion_eqc(result_Real_Estate,rre3)

## 11 Utilities
ruta_salida="/Users/Alejandro/OneDrive/Escritorio/TFM-32/output2/Utilities"
result_Utilities <- read.csv(file.path(path_output,"result_Utilities.csv"))
ru3 <- read.csv(file.path(path_output,"ru3.csv"))

funcion_eqc(result_Utilities,ru3)
