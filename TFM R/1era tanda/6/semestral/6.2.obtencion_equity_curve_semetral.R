library(quantmod)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape)
library(grid)
library(TTR)
library(PerformanceAnalytics)
library(stringr)
library(data.table)

path_output= "/Users/Alejandro/OneDrive/Escritorio/TFM-32/datos"
ruta_output= "/Users/Alejandro/OneDrive/Escritorio/TFM-32/output"
salida_output= "/Users/Alejandro/OneDrive/Escritorio/TFM-32/output3"


########################################################################################
## Funcion rents dailys

funcion_equity_curve <- function(input, inputd) {
  
  input$par <- gsub("[.-]", "_", input$par)
  colnames(inputd) <- gsub("[.-]", "_", colnames(inputd))
  
  pares <- tail(names(inputd), -1)
  result <- subset(inputd, select = c("Date"))
  
  for (i in pares){
    df <- subset(inputd, select = c("Date", i))
    df$lag=lag(df[,2])
    df$Rent=(df[,2]/df$lag)-1
    df=df[,c(1,4)]
    names(df)[names(df)=="Rent"]= i
    result <- merge(result, df, by = "Date", all.x = TRUE)
    result= arrange(result,Date)
    result[is.na(result)] = 0
  }
  
  df=result
  
  df$mes=month(df$Date)
  df$semester=semester(df$Date)
  df$year=year(df$Date)
  df$fechaaux=paste0(year(df$Date),df$semester)
  df$rent_estrategia=0
  df$eqcurve_estrategia=100
  
  # Los activos seleccionados para cada año son los del semestre anterior del anterior código
  nombres_elementos <- c()
  for (anio in 2005:2022) {
    for (semestre in 1:2) {
      for (trimestre in 1:3) {
        nombre_elemento <- paste0("Activos_", anio, semestre, "_", trimestre)
        nombres_elementos <- c(nombres_elementos, nombre_elemento)
      }
    }
  }
  nombres_elementos <- head(nombres_elementos, - 3)
  data <- head(input, n = nrow(input) - 3)$par
  df1 <- data.frame(col1 = unlist(nombres_elementos), col2 = unlist(data))
  
  result <- list()
  for (i in 1:nrow(df1)) {
    list_name <- trimws(as.character(df1[i, "col1"]))
    index <- which(names(df) == df1[i, "col2"])
    if (length(index) > 0) {
      result[[list_name]] <- unlist(df[, index])
    } else {
      result[[list_name]] <- NULL
    }
  }
  
  for (fila in 2:c(nrow(df))) {  
    # fila=2
    df$rent_estrategia[fila] <- case_when(
                                          df$year[fila]  == "2006"  ~ (result$Activos_20052_1[fila]+result$Activos_20052_2[fila]+result$Activos_20052_3[fila]+result$Activos_20061_1[fila]+result$Activos_20061_2[fila]+result$Activos_20061_3[fila])/6 ,
                                          df$year[fila]  == "2007"  ~ (result$Activos_20062_1[fila]+result$Activos_20062_2[fila]+result$Activos_20062_3[fila]+result$Activos_20071_1[fila]+result$Activos_20071_2[fila]+result$Activos_20071_3[fila])/6 ,
                                          df$year[fila]  == "2008"  ~ (result$Activos_20072_1[fila]+result$Activos_20072_2[fila]+result$Activos_20072_3[fila]+result$Activos_20081_1[fila]+result$Activos_20081_2[fila]+result$Activos_20081_3[fila])/6 ,
                                          df$year[fila]  == "2009"  ~ (result$Activos_20082_1[fila]+result$Activos_20082_2[fila]+result$Activos_20082_3[fila]+result$Activos_20091_1[fila]+result$Activos_20091_2[fila]+result$Activos_20091_3[fila])/6 ,
                                          df$year[fila]  == "2010"  ~ (result$Activos_20092_1[fila]+result$Activos_20092_2[fila]+result$Activos_20092_3[fila]+result$Activos_20101_1[fila]+result$Activos_20101_2[fila]+result$Activos_20101_3[fila])/6 ,
                                          df$year[fila]  == "2011"  ~ (result$Activos_20102_1[fila]+result$Activos_20102_2[fila]+result$Activos_20102_3[fila]+result$Activos_20111_1[fila]+result$Activos_20111_2[fila]+result$Activos_20111_3[fila])/6 ,
                                          df$year[fila]  == "2012"  ~ (result$Activos_20112_1[fila]+result$Activos_20112_2[fila]+result$Activos_20112_3[fila]+result$Activos_20121_1[fila]+result$Activos_20121_2[fila]+result$Activos_20121_3[fila])/6 ,
                                          df$year[fila]  == "2013"  ~ (result$Activos_20122_1[fila]+result$Activos_20122_2[fila]+result$Activos_20122_3[fila]+result$Activos_20131_1[fila]+result$Activos_20131_2[fila]+result$Activos_20131_3[fila])/6 ,
                                          df$year[fila]  == "2014"  ~ (result$Activos_20132_1[fila]+result$Activos_20132_2[fila]+result$Activos_20132_3[fila]+result$Activos_20141_1[fila]+result$Activos_20141_2[fila]+result$Activos_20141_3[fila])/6 ,
                                          df$year[fila]  == "2015"  ~ (result$Activos_20142_1[fila]+result$Activos_20142_2[fila]+result$Activos_20142_3[fila]+result$Activos_20151_1[fila]+result$Activos_20151_2[fila]+result$Activos_20151_3[fila])/6 ,
                                          df$year[fila]  == "2016"  ~ (result$Activos_20152_1[fila]+result$Activos_20152_2[fila]+result$Activos_20152_3[fila]+result$Activos_20161_1[fila]+result$Activos_20161_2[fila]+result$Activos_20161_3[fila])/6 ,
                                          df$year[fila]  == "2017"  ~ (result$Activos_20162_1[fila]+result$Activos_20162_2[fila]+result$Activos_20162_3[fila]+result$Activos_20171_1[fila]+result$Activos_20171_2[fila]+result$Activos_20171_3[fila])/6 ,
                                          df$year[fila]  == "2018"  ~ (result$Activos_20172_1[fila]+result$Activos_20172_2[fila]+result$Activos_20172_3[fila]+result$Activos_20181_1[fila]+result$Activos_20181_2[fila]+result$Activos_20181_3[fila])/6 ,
                                          df$year[fila]  == "2019"  ~ (result$Activos_20182_1[fila]+result$Activos_20182_2[fila]+result$Activos_20182_3[fila]+result$Activos_20191_1[fila]+result$Activos_20191_2[fila]+result$Activos_20191_3[fila])/6 ,
                                          df$year[fila]  == "2020"  ~ (result$Activos_20192_1[fila]+result$Activos_20192_2[fila]+result$Activos_20192_3[fila]+result$Activos_20201_1[fila]+result$Activos_20201_2[fila]+result$Activos_20201_3[fila])/6 ,
                                          df$year[fila]  == "2021"  ~ (result$Activos_20202_1[fila]+result$Activos_20202_2[fila]+result$Activos_20202_3[fila]+result$Activos_20211_1[fila]+result$Activos_20211_2[fila]+result$Activos_20211_3[fila])/6 ,
                                          df$year[fila]  == "2022"  ~ (result$Activos_20212_1[fila]+result$Activos_20212_2[fila]+result$Activos_20212_3[fila]+result$Activos_20221_1[fila]+result$Activos_20221_2[fila]+result$Activos_20221_3[fila])/6 ,
                                          TRUE ~ 0 
                                   )
    
    df$eqcurve_estrategia[fila]=df$eqcurve_estrategia[(fila-1)]*(1+df$rent_estrategia[fila])
  }
  df <- df[,c("Date","eqcurve_estrategia")]
  return(df)
}
#################################################################################################################################################

### 11 sectores

## 1 Communication Services
rcs3d <- read.csv(file.path(ruta_output,"rcs3d.csv"))
rcs3 <- read.csv(file.path(path_output,"rcs3.csv"))

cs <- funcion_equity_curve(rcs3,rcs3d)

write.csv(cs,file.path(salida_output,"cs2.csv"),row.names = F)


## 2 Consumer Discretionary
rcd3d <- read.csv(file.path(ruta_output,"rcd3d.csv"))
rcd3 <- read.csv(file.path(path_output,"rcd3.csv"))

cd <- funcion_equity_curve(rcd3,rcd3d)

write.csv(cd,file.path(salida_output,"cd2.csv"),row.names = F)


## 3 Consumer Staples
rcst3d <- read.csv(file.path(ruta_output,"rcst3d.csv"))
rcst3 <- read.csv(file.path(path_output,"rcst3.csv"))

cst <- funcion_equity_curve(rcst3,rcst3d)

write.csv(cst,file.path(salida_output,"cst2.csv"),row.names = F)


## 4 Energy 
re3d <- read.csv(file.path(ruta_output,"re3d.csv"))
re3 <- read.csv(file.path(path_output,"re3.csv"))

e <- funcion_equity_curve(re3,re3d)

write.csv(e,file.path(salida_output,"e2.csv"),row.names = F)


## 5 Financials
rf3d <- read.csv(file.path(ruta_output,"rf3d.csv"))
rf3 <- read.csv(file.path(path_output,"rf3.csv"))

f <- funcion_equity_curve(rf3,rf3d)

write.csv(f,file.path(salida_output,"f2.csv"),row.names = F)


## 6 Health Care
rhc3d <- read.csv(file.path(ruta_output,"rhc3d.csv"))
rhc3 <- read.csv(file.path(path_output,"rhc3.csv"))

hc <- funcion_equity_curve(rhc3,rhc3d)

write.csv(hc,file.path(salida_output,"hc2.csv"),row.names = F)


## 7 Industrials
ri3d <- read.csv(file.path(ruta_output,"ri3d.csv"))
ri3 <- read.csv(file.path(path_output,"ri3.csv"))

i <- funcion_equity_curve(ri3,ri3d)

write.csv(i,file.path(salida_output,"i2.csv"),row.names = F)


## 8 Information Technology 
rit3d <- read.csv(file.path(ruta_output,"rit3d.csv"))
rit3 <- read.csv(file.path(path_output,"rit3.csv"))

it <- funcion_equity_curve(rit3,rit3d)

write.csv(it,file.path(salida_output,"it2.csv"),row.names = F)


## 9 Materials
rm3d <- read.csv(file.path(ruta_output,"rm3d.csv"))
rm3 <- read.csv(file.path(path_output,"rm3.csv"))

m <- funcion_equity_curve(rm3,rm3d)

write.csv(m,file.path(salida_output,"m2.csv"),row.names = F)


## 10 Real Estate
rre3d <- read.csv(file.path(ruta_output,"rre3d.csv"))
rre3 <- read.csv(file.path(path_output,"rre3.csv"))

re <- funcion_equity_curve(rre3,rre3d)

write.csv(re,file.path(salida_output,"re2.csv"),row.names = F)


## 11 Utilities
ru3d <- read.csv(file.path(ruta_output,"ru3d.csv"))
ru3 <- read.csv(file.path(path_output,"ru3.csv"))

u <- funcion_equity_curve(ru3,ru3d)

write.csv(u,file.path(salida_output,"u2.csv"),row.names = F)

