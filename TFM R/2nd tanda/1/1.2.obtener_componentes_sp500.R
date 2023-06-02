library(tidyquant)
tq_index_options()

path_output= "/Users/Alejandro/OneDrive/Escritorio/TFM-2/datos"


table(df_sp600$sector)
df_sp600$sector

# symbol, company, sector
library(dplyr)

sp600 <- df_sp600 %>% select(symbol, sector)
write.csv2(sp600,file.path(path_output,"sp600.csv"),row.names = F)

symbol <- sp600 %>% pull(symbol)
write.csv2(symbol,file.path(path_output,"symbol.csv"),row.names = F)





