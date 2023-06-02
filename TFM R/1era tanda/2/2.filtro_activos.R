library(quantmod)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape)
library(grid)
library(TTR)
library(PerformanceAnalytics)
library(stringr)
library(xlsx)

path_output= "/Users/Alejandro/OneDrive/Escritorio/TFM-32/datos"

symbol <- read.csv("/Users/Alejandro/OneDrive/Escritorio/TFM-32/datos/symbol.csv")

stocks<- symbol[,c("x")]

# select ticker symbols and time frame
date_begin <- as.Date("2005-01-01")
date_end <- as.Date("2022-01-28")

# Quitamos RXO ya que el programa no es capaz de download su informacion
stocks <- stocks[!grepl("RXO", stocks)]
# Quitamos EMBC ya que el programa no es capaz de download su informacion
stocks <- stocks[!grepl("EMBC", stocks)]
# Quitamos MBC ya que el programa no es capaz de download su informacion
stocks <- stocks[!grepl("MBC", stocks)]
# Quitamos EHAB ya que el programa no es capaz de download su informacion
stocks <- stocks[!grepl("EHAB", stocks)]
# Quitamos XPER  ya que el programa no es capaz de download su informacion
stocks <- stocks[!grepl("XPER", stocks)]
# Quitamos ZIMV  ya que el programa no es capaz de download su informacion
stocks <- stocks[!grepl("ZIMV", stocks)]

for (stock in stocks) {
  
  df=as.data.frame(get(ticker <- getSymbols(stock, from=date_begin, to=date_end, auto.assign=TRUE)))

  df$Date=row.names(df)
  
  df$Date <- ymd(df$Date)
  df <- na.omit(df)
  fecha_min=as.character(min(df$Date))
  tab=c(stock,fecha_min)
  tab=as.data.frame(t(tab))
  colnames(tab)=c("stock","fecha_min")
  
        if (stock == stocks[1]){
          #i=2
          
          tab_f <- tab
          
        }else if (stock != stocks[1] ) {
          tab_f <- rbind(tab_f,tab)
          
        }
  
rm(tab)
}


# guardado de la salida
write.csv2(tab_f,file.path(path_output,"filtro_activos.csv"),row.names = F)

# fechas para 2005-01-03
tab_n <- subset(tab_f, fecha_min == "2005-01-03")

write.csv2(tab_n[,c("stock")],file.path(path_output,"filtro_activos2.csv"),row.names = F)
filtro_activos2 <- read.csv(file.path(path_output,"filtro_activos2.csv"))
#write.csv2(stok,file.path(path_output,"filtro_activos.csv"),row.names = F)

