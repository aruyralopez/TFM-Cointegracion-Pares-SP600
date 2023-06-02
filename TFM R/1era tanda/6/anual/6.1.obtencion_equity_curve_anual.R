library(quantmod)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape)
library(grid)
library(TTR)
library(PerformanceAnalytics)
library(stringr) # para arreglar la fecha (rellenado de ceros a la izquierda del mes)
library(data.table)

path_output= "/Users/Alejandro/OneDrive/Escritorio/TFM-32/datos"
ruta_output= "/Users/Alejandro/OneDrive/Escritorio/TFM-32/output"


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
  for (anio in 2006:2022) {
    for (trimestre in 1:3) {
      nombre_elemento <- paste0("Activos_", anio, "_", trimestre)
      nombres_elementos <- c(nombres_elementos, nombre_elemento)
    }
  }
  
  data <- subset(input, fecha %% 10 != 1)
  data <- head(data, n = nrow(data) - 3)$par
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
                                          
                                          df$year[fila]  == "2006"  ~ (result$Activos_2006_1[fila]+result$Activos_2006_2[fila]+result$Activos_2006_3[fila])/3 ,
                                          df$year[fila]  == "2007"  ~ (result$Activos_2007_1[fila]+result$Activos_2007_2[fila]+result$Activos_2007_3[fila])/3 ,
                                          df$year[fila]  == "2008"  ~ (result$Activos_2008_1[fila]+result$Activos_2008_2[fila]+result$Activos_2008_3[fila])/3 ,
                                          df$year[fila]  == "2009"  ~ (result$Activos_2009_1[fila]+result$Activos_2009_2[fila]+result$Activos_2009_3[fila])/3 ,
                                          df$year[fila]  == "2010"  ~ (result$Activos_2010_1[fila]+result$Activos_2010_2[fila]+result$Activos_2010_3[fila])/3 ,
                                          df$year[fila]  == "2011"  ~ (result$Activos_2011_1[fila]+result$Activos_2011_2[fila]+result$Activos_2011_3[fila])/3 ,
                                          df$year[fila]  == "2012"  ~ (result$Activos_2012_1[fila]+result$Activos_2012_2[fila]+result$Activos_2012_3[fila])/3 ,
                                          df$year[fila]  == "2013"  ~ (result$Activos_2013_1[fila]+result$Activos_2013_2[fila]+result$Activos_2013_3[fila])/3 ,
                                          df$year[fila]  == "2014"  ~ (result$Activos_2014_1[fila]+result$Activos_2014_2[fila]+result$Activos_2014_3[fila])/3 ,
                                          df$year[fila]  == "2015"  ~ (result$Activos_2015_1[fila]+result$Activos_2015_2[fila]+result$Activos_2015_3[fila])/3 ,
                                          df$year[fila]  == "2016"  ~ (result$Activos_2016_1[fila]+result$Activos_2016_2[fila]+result$Activos_2016_3[fila])/3 ,
                                          df$year[fila]  == "2017"  ~ (result$Activos_2017_1[fila]+result$Activos_2017_2[fila]+result$Activos_2017_3[fila])/3 ,
                                          df$year[fila]  == "2018"  ~ (result$Activos_2018_1[fila]+result$Activos_2018_2[fila]+result$Activos_2018_3[fila])/3 ,
                                          df$year[fila]  == "2019"  ~ (result$Activos_2019_1[fila]+result$Activos_2019_2[fila]+result$Activos_2019_3[fila])/3 ,
                                          df$year[fila]  == "2020"  ~ (result$Activos_2020_1[fila]+result$Activos_2020_2[fila]+result$Activos_2020_3[fila])/3 ,
                                          df$year[fila]  == "2021"  ~ (result$Activos_2021_1[fila]+result$Activos_2021_2[fila]+result$Activos_2021_3[fila])/3 ,
                                          df$year[fila]  == "2022"  ~ (result$Activos_2022_1[fila]+result$Activos_2022_2[fila]+result$Activos_2022_3[fila])/3 ,
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

write.csv(cs,file.path(ruta_output,"cs.csv"),row.names = F)


## 2 Consumer Discretionary
rcd3d <- read.csv(file.path(ruta_output,"rcd3d.csv"))
rcd3 <- read.csv(file.path(path_output,"rcd3.csv"))

cd <- funcion_equity_curve(rcd3,rcd3d)

write.csv(cd,file.path(ruta_output,"cd.csv"),row.names = F)


## 3 Consumer Staples
rcst3d <- read.csv(file.path(ruta_output,"rcst3d.csv"))
rcst3 <- read.csv(file.path(path_output,"rcst3.csv"))

cst <- funcion_equity_curve(rcst3,rcst3d)

write.csv(cst,file.path(ruta_output,"cst.csv"),row.names = F)


## 4 Energy 
re3d <- read.csv(file.path(ruta_output,"re3d.csv"))
re3 <- read.csv(file.path(path_output,"re3.csv"))

e <- funcion_equity_curve(re3,re3d)

write.csv(e,file.path(ruta_output,"e.csv"),row.names = F)


## 5 Financials
rf3d <- read.csv(file.path(ruta_output,"rf3d.csv"))
rf3 <- read.csv(file.path(path_output,"rf3.csv"))

f <- funcion_equity_curve(rf3,rf3d)

write.csv(f,file.path(ruta_output,"f.csv"),row.names = F)


## 6 Health Care
rhc3d <- read.csv(file.path(ruta_output,"rhc3d.csv"))
rhc3 <- read.csv(file.path(path_output,"rhc3.csv"))

hc <- funcion_equity_curve(rhc3,rhc3d)

write.csv(hc,file.path(ruta_output,"hc.csv"),row.names = F)


## 7 Industrials
ri3d <- read.csv(file.path(ruta_output,"ri3d.csv"))
ri3 <- read.csv(file.path(path_output,"ri3.csv"))

i <- funcion_equity_curve(ri3,ri3d)

write.csv(i,file.path(ruta_output,"i.csv"),row.names = F)


## 8 Information Technology 
rit3d <- read.csv(file.path(ruta_output,"rit3d.csv"))
rit3 <- read.csv(file.path(path_output,"rit3.csv"))

it <- funcion_equity_curve(rit3,rit3d)

write.csv(it,file.path(ruta_output,"it.csv"),row.names = F)


## 9 Materials
rm3d <- read.csv(file.path(ruta_output,"rm3d.csv"))
rm3 <- read.csv(file.path(path_output,"rm3.csv"))

m <- funcion_equity_curve(rm3,rm3d)

write.csv(m,file.path(ruta_output,"m.csv"),row.names = F)


## 10 Real Estate
rre3d <- read.csv(file.path(ruta_output,"rre3d.csv"))
rre3 <- read.csv(file.path(path_output,"rre3.csv"))

re <- funcion_equity_curve(rre3,rre3d)

write.csv(re,file.path(ruta_output,"re.csv"),row.names = F)


## 11 Utilities
ru3d <- read.csv(file.path(ruta_output,"ru3d.csv"))
ru3 <- read.csv(file.path(path_output,"ru3.csv"))

u <- funcion_equity_curve(ru3,ru3d)

write.csv(u,file.path(ruta_output,"u.csv"),row.names = F)

