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
ruta_salida="/Users/Alejandro/OneDrive/Escritorio/TFM-32/output"

########################################################################################
## Funcion mensual equity curve
funcion_eqc <- function(df,df3) {
  
  unique_vector <- unique(df3$par)
  result <- data.frame()
  
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
    ETFS$Date_aux <- format(ETFS$Date,"%Y-%m")
    
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
    df_filtered <- ETFS %>% group_by(year(Date), month(Date)) %>% slice_tail(n = 1)
    
    # Guardado de la tabla del par analizado
    if(nrow(result) == nrow(df_filtered)){
      result <- cbind(result, df_filtered["eqcurve_estrategia"])
    } else {
      result <- rbind(result, df_filtered["eqcurve_estrategia"])
    }
    result = rename(result, c(eqcurve_estrategia = i))
  }
  rownames(result) <- df_filtered$Date_aux
  return(result)
}
########################################################################################
## Funcion ultimos equitys

last_eq <- function(df,cluster){
  eq <- df
  ind <- names(eq)
  names(eq) <- NULL
  eq <- t(eq)
  eq <- eq[, ncol(eq)]
  result <- data.frame(ind,eq) %>% arrange(eq)
  result <- result |> mutate(sector = cluster, .before = ind)
  return(result)
}
########################################################################################

### 11 sectores

## 1 Communication Services

result_Communication_Services <- read.csv(file.path(path_output,"result_Communication_Services.csv"))
rcs3 <- read.csv(file.path(path_output,"rcs3.csv"))

cs_eqc <- funcion_eqc(result_Communication_Services,rcs3)
write.csv(cs_eqc,file.path(ruta_salida,"cs_eqc.csv"),row.names = F)
cs_eq <- last_eq(cs_eqc,"Communication_Services")


## 2 Consumer Discretionary
result_Consumer_Discretionary <- read.csv(file.path(path_output,"result_Consumer_Discretionary.csv"))
rcd3 <- read.csv(file.path(path_output,"rcd3.csv"))

cd_eqc <- funcion_eqc(result_Consumer_Discretionary,rcd3)
write.csv(cd_eqc,file.path(ruta_salida,"cd_eqc.csv"),row.names = F)
cd_eq <- last_eq(cd_eqc,"Consumer_Discretionary")


## 3 Consumer Staples
result_Consumer_Staples <- read.csv(file.path(path_output,"result_Consumer_Staples.csv"))
rcst3 <- read.csv(file.path(path_output,"rcst3.csv"))

cst_eqc <- funcion_eqc(result_Consumer_Staples,rcst3)
write.csv(cst_eqc,file.path(ruta_salida,"cst_eqc.csv"),row.names = F)
cst_eq <- last_eq(cst_eqc,"Consumer_Staples")


## 4 Energy 
result_Energy <- read.csv(file.path(path_output,"result_Energy.csv"))
re3 <- read.csv(file.path(path_output,"re3.csv"))

e_eqc <- funcion_eqc(result_Energy,re3)
write.csv(e_eqc,file.path(ruta_salida,"e_eqc.csv"),row.names = F)
e_eq <- last_eq(e_eqc,"Energy")


## 5 Financials
result_Financials <- read.csv(file.path(path_output,"result_Financials.csv"))
rf3 <- read.csv(file.path(path_output,"rf3.csv"))

f_eqc <- funcion_eqc(result_Financials,rf3)
write.csv(f_eqc,file.path(ruta_salida,"f_eqc.csv"),row.names = F)
f_eq <- last_eq(f_eqc,"Financials")


## 6 Health Care
result_Health_Care <- read.csv(file.path(path_output,"result_Health_Care.csv"))
rhc3 <- read.csv(file.path(path_output,"rhc3.csv"))

hc_eqc <- funcion_eqc(result_Health_Care,rhc3)
write.csv(hc_eqc,file.path(ruta_salida,"hc_eqc.csv"),row.names = F)
hc_eqc <- read.csv(file.path(ruta_salida,"hc_eqc.csv"))
hc_eq <- last_eq(hc_eqc,"Health_Care")


## 7 Industrials
result_Industrials <- read.csv(file.path(path_output,"result_Industrials.csv"))
ri3 <- read.csv(file.path(path_output,"ri3.csv"))

i_eqc <- funcion_eqc(result_Industrials,ri3)
write.csv(i_eqc,file.path(ruta_salida,"i_eqc.csv"),row.names = F)
i_eq <- last_eq(i_eqc,"Industrials")


## 8 Information Technology 
result_Information_Technology <- read.csv(file.path(path_output,"result_Information_Technology.csv"))
rit3 <- read.csv(file.path(path_output,"rit3.csv"))

it_eqc <- funcion_eqc(result_Information_Technology,rit3)
write.csv(it_eqc,file.path(ruta_salida,"it_eqc.csv"),row.names = F)
it_eq <- last_eq(it_eqc,"Information_Technology")


## 9 Materials
result_Materials <- read.csv(file.path(path_output,"result_Materials.csv"))
rm3 <- read.csv(file.path(path_output,"rm3.csv"))

m_eqc <- funcion_eqc(result_Materials,rm3)
write.csv(m_eqc,file.path(ruta_salida,"m_eqc.csv"),row.names = F)
m_eq <- last_eq(m_eqc,"Materials")


## 10 Real Estate 
result_Real_Estate <- read.csv(file.path(path_output,"result_Real_Estate.csv"))
rre3 <- read.csv(file.path(path_output,"rre3.csv"))

re_eqc <- funcion_eqc(result_Real_Estate,rre3)
write.csv(re_eqc,file.path(ruta_salida,"re_eqc.csv"),row.names = F)
re_eq <- last_eq(re_eqc,"Real_Estate")


## 11 Utilities
result_Utilities <- read.csv(file.path(path_output,"result_Utilities.csv"))
ru3 <- read.csv(file.path(path_output,"ru3.csv"))

u_eqc <- funcion_eqc(result_Utilities,ru3)
write.csv(u_eqc,file.path(ruta_salida,"u_eqc.csv"),row.names = F)
u_eq <- last_eq(u_eqc,"Utilities")


#####

final_eq <- rbind(cd_eq, cs_eq, cst_eq, e_eq, f_eq, hc_eq, i_eq, it_eq, m_eq, re_eq, u_eq)
write.csv(final_eq,file.path(ruta_salida,"final_eq.csv"),row.names = F)
final_eq <- read.csv(file.path(ruta_salida,"final_eq.csv"))
