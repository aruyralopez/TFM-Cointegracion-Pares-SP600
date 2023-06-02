library(dplyr)
library(quantmod)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape)
library(grid)
library(TTR)
library(xts)
library(xlsx)
library(PerformanceAnalytics)

salida_output= "/Users/Alejandro/OneDrive/Escritorio/TFM-32/output"
salida= "/Users/Alejandro/OneDrive/Escritorio/TFM-32/metricas_anuales"

################################################################################
### Calculo rentabilidad
rent <- function(df) {
  df$lag=lag(df[,2])
  df$rent_estrategia=(df[,2]/df$lag)-1
  df=df[,c(1,2,4)]
  df[is.na(df)] = 0
  return(df)
}
################################################################################
### Metricas
metrics <- function(tabla){
  
  tabla$Date=as.character(tabla$Date)
  tabla_xts <- xts(tabla,order.by = as.Date(tabla$Date,"%Y-%m-%d"))
  
  #3.Profit factor (estrategias rentables==> >1)
  tabla$Rent_eur= tabla$eqcurve_estrategia-dplyr::lag(tabla$eqcurve_estrategia,n=1)
  tabla$Rent_eur[is.na(tabla$Rent_eur)] = 0
  count_perd=tabla[tabla$Rent_eur < 0,]
  count_perd=count_perd[2:nrow(count_perd),c("eqcurve_estrategia","Rent_eur")]
  perd=-sum(count_perd[, c("Rent_eur")])
  count_ganad=tabla[tabla$Rent_eur > 0,]
  count_ganad=count_ganad[2:nrow(count_ganad),c("eqcurve_estrategia","Rent_eur")]
  ganad=sum(count_ganad[, c("Rent_eur")])
  Profit_factor=ganad/perd
  
  #4.Obtencion de Tabla y retornos en xts para poder calcular los ratios
  tabla_xts$Rent_eur=NULL
  tabla_xts$Date=NULL 
  tabla_xts$rent_estrategia=NULL
  tabla_xts$pr=1
  returns.data <- PerformanceAnalytics::CalculateReturns(tabla_xts[,1],method="discrete")
  returns.data <- na.omit(returns.data)
  
  returns_df=as.data.frame(returns.data)
  returns_df=na.omit(returns_df)
  
  # 5. - Cálculo de los drawdowns
  Drawdowns=table.Drawdowns(returns.data[,"eqcurve_estrategia"],top=5)
  Drawdowns <- na.omit(Drawdowns)
  max_dd=Drawdowns[1,]
  avg_dd=as.data.frame(t(apply(Drawdowns[,4:7], 2, mean)))
  
  # 6 . - Obtencion de la tabla con los resultados
  tab=cbind(max_dd[,4:7],avg_dd)
  tab$Tot_Ret=prod(1+returns_df$eqcurve_estrategia)-1 
  tab$An_ret=table.AnnualizedReturns(returns.data[,1],scale=252,digits=4)$eqcurve_estrategia[1]
  tab$Std_dev=table.AnnualizedReturns(returns.data[,1],scale=252,digits=4)$eqcurve_estrategia[2]
  tab$sharpe=table.AnnualizedReturns(returns.data[,1],scale=252,digits=4)$eqcurve_estrategia[3]
  tab$Downsidedev=DownsideDeviation(returns.data,MAR=0)[1]
  tab$Sortino=SortinoRatio(returns.data,MAR=0)[1]
  tab$Calmar=CalmarRatio(returns.data,scale=252)[1]
  tab$Sterling=tab$An_ret/-avg_dd$Depth
  tab$Profit_factor=Profit_factor
  tab$Rfactor=(prod(1+returns_df$eqcurve_estrategia)-1)/-max_dd$Depth
  tab$name="tendencial"
  
  #primera ejecucion
  Metricas <- tab
  return(Metricas)
}
################################################################################

### 11 sectores

## 1 Communication Services
cs2 <- read.csv(file.path(salida_output,"cs.csv"))
cs2 <- rent(cs2)
cs2m <- metrics(cs2)

write.csv(cs2m,file.path(salida,"csm.csv"),row.names = F)

## 2 Consumer Discretionary
cd2 <- read.csv(file.path(salida_output,"cd.csv"))
cd2 <- rent(cd2)
cd2m <- metrics(cd2)

write.csv(cd2m,file.path(salida,"cdm.csv"),row.names = F)


## 3 Consumer Staples
cst2 <- read.csv(file.path(salida_output,"cst.csv"))
cst2 <- rent(cst2)
cst2m <- metrics(cst2)

write.csv(cst2m,file.path(salida,"cstm.csv"),row.names = F)


## 4 Energy 
e2 <- read.csv(file.path(salida_output,"e.csv"))
e2 <- rent(e2)
e2m <- metrics(e2)

write.csv(e2m,file.path(salida,"em.csv"),row.names = F)


## 5 Financials
f2 <- read.csv(file.path(salida_output,"f.csv"))
f2 <- rent(f2)
f2m <- metrics(f2)

write.csv(f2m,file.path(salida,"fm.csv"),row.names = F)


## 6 Health Care
hc2 <- read.csv(file.path(salida_output,"hc.csv"))
hc2 <- rent(hc2)
hc2m <- metrics(hc2)

write.csv(hc2m,file.path(salida,"hcm.csv"),row.names = F)


## 7 Industrials
i2 <- read.csv(file.path(salida_output,"i.csv"))
i2 <- rent(i2)
i2m <- metrics(i2)

write.csv(i2m,file.path(salida,"im.csv"),row.names = F)


## 8 Information Technology 
it2 <- read.csv(file.path(salida_output,"it.csv"))
it2 <- rent(it2)
it2m <- metrics(it2)

write.csv(it2m,file.path(salida,"itm.csv"),row.names = F)


## 9 Materials
m2 <- read.csv(file.path(salida_output,"m.csv"))
m2 <- rent(m2)
m2m <- metrics(m2)

write.csv(m2m,file.path(salida,"mm.csv"),row.names = F)


## 10 Real Estate 
re2 <- read.csv(file.path(salida_output,"re.csv"))
re2 <- rent(re2)
re2m <- metrics(re2)

write.csv(re2m,file.path(salida,"rem.csv"),row.names = F)


## 11 Utilities
u2 <- read.csv(file.path(salida_output,"u.csv"))
u2 <- rent(u2)
u2m <- metrics(u2)

write.csv(u2m,file.path(salida,"um.csv"),row.names = F)

