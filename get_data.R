library(tidyverse)
library(cansim)


businesses <- get_cansim_vector("v1203704156") %>% 
    select(Date,val_norm) %>% 
    rename(Active_Businesses = val_norm)

write.csv(x = businesses, file="data/businesses.csv")


gdp <- get_cansim_vector("v65201210",
                         start_time = "2000-01-01") %>% 
  select(Date,val_norm) %>% 
  rename(gdp = val_norm)

write.csv(x = gdp, file="data/gdp.csv")


manufacturing <- get_cansim_vector("v800450") %>% 
  select(Date,val_norm) %>% 
  rename(manufacturing = val_norm)

write.csv(x = manufacturing, file="data/manufacturing.csv")


export <- get_cansim_vector("v1001809606",
                            start_time = "2000-01-01") %>% 
  select(Date,val_norm) %>% 
  rename(export = val_norm)

write.csv(x = export, file="data/export.csv")



retail <- get_cansim_vector("52367097") %>% 
  select(Date,val_norm) %>% 
  rename(retail = val_norm)

write.csv(x = retail, file="data/retail.csv")
