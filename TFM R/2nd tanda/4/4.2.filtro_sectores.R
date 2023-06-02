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

path_output= "/Users/Alejandro/OneDrive/Escritorio/TFM-2/datos"

#############################################################################
## Funcion filtro veces y duracion + t con duplicados

filtrar_datos<- function(df) {
  
  # Filtrar los registros según las condiciones dadas
  df_filtrado <- df %>%
    filter( num_veces > 5, dur_media < 50, t < -2.58 )
  
  # Obtener los tres valores más pequeños por cada año
  df_resultado <- df_filtrado %>%
    arrange(fecha, t) %>%
    group_by(fecha) %>%
    mutate(rank = row_number()) %>%
    filter(rank <= 3) %>%
    ungroup() %>%
    select(-rank)
  
  # Completar con los valores más pequeños en caso de tener menos de tres registros por año
  df_completado <- df_resultado %>%
    group_by(fecha) %>%
    mutate(n = n()) %>%
    ungroup()
  
  fecha_duplicados <- df_completado$fecha[df_completado$n < 3]
  df_duplicado <- df_completado %>% filter(fecha %in% fecha_duplicados)
  
  df_completado <- bind_rows(df_completado, df_duplicado) %>%
    group_by(fecha) %>%
    slice_min(t, n = 3, with_ties = FALSE) %>%
    ungroup() %>%
    select(-n)
  
  return(df_completado)
}
#############################################################################

### 11 sectores

## 1 Communication Services (7 index)
result_Communication_Services <- read.csv(file.path(path_output,"result_Communication_Services.csv"))

rcs3 <- filtrar_datos(result_Communication_Services)
write.csv(rcs3,file.path(path_output,"rcs3.csv"),row.names = F)

## 2 Consumer Discretionary (57 index)
result_Consumer_Discretionary <- read.csv(file.path(path_output,"result_Consumer_Discretionary.csv"))

rcd3 <- filtrar_datos(result_Consumer_Discretionary)
write.csv(rcd3,file.path(path_output,"rcd3.csv"),row.names = F)

## 3 Consumer Staples (23 index)
result_Consumer_Staples <- read.csv(file.path(path_output,"result_Consumer_Staples.csv"))

rcst3 <- filtrar_datos(result_Consumer_Staples)
write.csv(rcst3,file.path(path_output,"rcst3.csv"),row.names = F)

## 4 Energy (15 index)
result_Energy <- read.csv(file.path(path_output,"result_Energy.csv"))

re3 <- filtrar_datos(result_Energy)
write.csv(re3,file.path(path_output,"re3.csv"),row.names = F)

## 5 Financials (66 index)
result_Financials <- read.csv(file.path(path_output,"result_Financials.csv"))

rf3 <- filtrar_datos(result_Financials)
write.csv(rf3,file.path(path_output,"rf3.csv"),row.names = F)

## 6 Health Care (40 index)
result_Health_Care <- read.csv(file.path(path_output,"result_Health_Care.csv"))

rhc3 <- filtrar_datos(result_Health_Care)
write.csv(rhc3,file.path(path_output,"rhc3.csv"),row.names = F)

## 7 Industrials (71 index)
result_Industrials <- read.csv(file.path(path_output,"result_Industrials.csv"))

ri3 <- filtrar_datos(result_Industrials)
write.csv(ri3,file.path(path_output,"ri3.csv"),row.names = F)

## 8 Information Technology (51 index)
result_Information_Technology <- read.csv(file.path(path_output,"result_Information_Technology.csv"))

rit3 <- filtrar_datos(result_Information_Technology)
write.csv(rit3,file.path(path_output,"rit3.csv"),row.names = F)

## 9 Materials (19 index)
result_Materials <- read.csv(file.path(path_output,"result_Materials.csv"))

rm3 <- filtrar_datos(result_Materials)
write.csv(rm3,file.path(path_output,"rm3.csv"),row.names = F)

## 10 Real Estate (19 index)
result_Real_Estate <- read.csv(file.path(path_output,"result_Real_Estate.csv"))

rre3 <- filtrar_datos(result_Real_Estate)
write.csv(rre3,file.path(path_output,"rre3.csv"),row.names = F)

## 11 Utilities (8 index)
result_Utilities <- read.csv(file.path(path_output,"result_Utilities.csv"))

ru3 <- filtrar_datos(result_Utilities)
write.csv(ru3,file.path(path_output,"ru3.csv"),row.names = F)

rm(result_Communication_Services,result_Consumer_Discretionary,result_Consumer_Staples,result_Energy,result_Financials,result_Health_Care,result_Industrials,result_Information_Technology,result_Materials,result_Real_Estate,result_Utilities)
