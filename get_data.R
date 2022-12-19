library(tidyverse)
library(cansim)


wages_canada <- get_cansim_vector("v2133245")

write.csv(x = wages_canada, file="wages.csv")