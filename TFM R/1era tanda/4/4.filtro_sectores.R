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

path_output= "/Users/Alejandro/OneDrive/Escritorio/TFM-32/datos"

#############################################################################
## Funcion Filtro 
filter_function <- function(df) {
  result <- df[df$t <= -2.58, ]
  return(result)
}
#############################################################################
## Funcion distribucion con densidad
graph_function <- function(df) {
  ggplot(df, aes(x=t)) + 
    geom_histogram(aes(y=..density..),binwidth=.2,colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666")
}
#############################################################################
## Funcion tres valores minimos por trimestre
min3 <- function(df) {
  df_top3 <- df %>% 
    group_by(fecha) %>% 
    top_n(-3, t) %>%
    arrange(t) %>%
    arrange(fecha)
  return(df_top3)
}
#############################################################################


### 11 sectores

## 1 Communication Services (7 index)
result_Communication_Services <- read.csv(file.path(path_output,"result_Communication_Services.csv"))
rcs <- filter_function(result_Communication_Services)
nrow(rcs)*100/nrow(result_Communication_Services)
write.csv(rcs,file.path(path_output,"rcs.csv"),row.names = F)

graph_function(rcs)

rcs3 <- min3(result_Communication_Services)
write.csv(rcs3,file.path(path_output,"rcs3.csv"),row.names = F)

## 2 Consumer Discretionary (57 index)
result_Consumer_Discretionary <- read.csv(file.path(path_output,"result_Consumer_Discretionary.csv"))
rcd <- filter_function(result_Consumer_Discretionary)
nrow(rcd)*100/nrow(result_Consumer_Discretionary)
write.csv(rcd,file.path(path_output,"rcd.csv"),row.names = F)

graph_function(rcd)

rcd3 <- min3(result_Consumer_Discretionary)
write.csv(rcd3,file.path(path_output,"rcd3.csv"),row.names = F)

## 3 Consumer Staples (23 index)
result_Consumer_Staples <- read.csv(file.path(path_output,"result_Consumer_Staples.csv"))
rcst <- filter_function(result_Consumer_Staples)
nrow(rcst)*100/nrow(result_Consumer_Staples)
write.csv(rcst,file.path(path_output,"rcst.csv"),row.names = F)

graph_function(rcst)

rcst3 <- min3(result_Consumer_Staples)
write.csv(rcst3,file.path(path_output,"rcst3.csv"),row.names = F)

## 4 Energy (15 index)
result_Energy <- read.csv(file.path(path_output,"result_Energy.csv"))
re <- filter_function(result_Energy)
nrow(re)*100/nrow(result_Energy)
write.csv(re,file.path(path_output,"re.csv"),row.names = F)

graph_function(re)

re3 <- min3(result_Energy)
write.csv(re3,file.path(path_output,"re3.csv"),row.names = F)

## 5 Financials (66 index)
result_Financials <- read.csv(file.path(path_output,"result_Financials.csv"))
rf <- filter_function(result_Financials)
nrow(rf)*100/nrow(result_Financials)
write.csv(rf,file.path(path_output,"rf.csv"),row.names = F)

graph_function(rf)

rf3 <- min3(result_Financials)
write.csv(rf3,file.path(path_output,"rf3.csv"),row.names = F)

## 6 Health Care (40 index)
result_Health_Care <- read.csv(file.path(path_output,"result_Health_Care.csv"))
rhc <- filter_function(result_Health_Care)
nrow(rhc)*100/nrow(result_Health_Care)
write.csv(rhc,file.path(path_output,"rhc.csv"),row.names = F)

graph_function(rhc)

rhc3 <- min3(result_Health_Care)
write.csv(rhc3,file.path(path_output,"rhc3.csv"),row.names = F)

## 7 Industrials (71 index)
result_Industrials <- read.csv(file.path(path_output,"result_Industrials.csv"))
ri <- filter_function(result_Industrials)
nrow(ri)*100/nrow(result_Industrials)
write.csv(ri,file.path(path_output,"ri.csv"),row.names = F)

graph_function(ri)

ri3 <- min3(result_Industrials)
write.csv(ri3,file.path(path_output,"ri3.csv"),row.names = F)

## 8 Information Technology (51 index)
result_Information_Technology <- read.csv(file.path(path_output,"result_Information_Technology.csv"))
rit <- filter_function(result_Information_Technology)
nrow(rit)*100/nrow(result_Information_Technology)
write.csv(rit,file.path(path_output,"rit.csv"),row.names = F)

graph_function(rit)

rit3 <- min3(result_Information_Technology)
write.csv(rit3,file.path(path_output,"rit3.csv"),row.names = F)

## 9 Materials (19 index)
result_Materials <- read.csv(file.path(path_output,"result_Materials.csv"))
rm <- filter_function(result_Materials)
nrow(rm)*100/nrow(result_Materials)
write.csv(rm,file.path(path_output,"rm.csv"),row.names = F)

graph_function(rm)

rm3 <- min3(result_Materials)
write.csv(rm3,file.path(path_output,"rm3.csv"),row.names = F)

## 10 Real Estate (19 index)
result_Real_Estate <- read.csv(file.path(path_output,"result_Real_Estate.csv"))
rre <- filter_function(result_Real_Estate)
nrow(rre)*100/nrow(result_Real_Estate)
write.csv(rre,file.path(path_output,"rre3.csv"),row.names = F)

graph_function(rre)

rre3 <- min3(result_Real_Estate)
write.csv(rre3,file.path(path_output,"rre3.csv"),row.names = F)

## 11 Utilities (8 index)
result_Utilities <- read.csv(file.path(path_output,"result_Utilities.csv"))
ru <- filter_function(result_Utilities)
nrow(ru)*100/nrow(result_Utilities)
write.csv(ru,file.path(path_output,"ru.csv"),row.names = F)

graph_function(ru)

ru3 <- min3(result_Utilities)
write.csv(ru3,file.path(path_output,"ru3.csv"),row.names = F)

#rm()