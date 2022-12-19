library(tidyverse)
library(cansim)


businesses <- get_cansim_vector("v1203704157")

businesses <- 
  businesses %>% 
    select(Date,val_norm) %>% 
    rename(Active_Businesses = val_norm)

write.csv(x = businesses, file="businesses.csv")



