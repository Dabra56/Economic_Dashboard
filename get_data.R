library(tidyverse)
library(cansim)


# --------------------- ECONOMY -------------------------  # 

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



retail <- get_cansim_vector("v52367097",
                            start_time = "2000-01-01") %>% 
  select(Date,val_norm) %>% 
  rename(retail = val_norm)

write.csv(x = retail, file="data/retail.csv")

# --------------------- LABOUR -------------------------  # 

employment<- get_cansim_vector("v2062811",
                                      start_time = "2000-01-01") %>% 
                    select(Date,val_norm) %>% 
                    rename(employment = val_norm)

write.csv(x = employment, file="data/employment.csv")

unemployment<- get_cansim_vector("v2062815",
                                start_time = "2000-01-01") %>% 
  select(Date,val_norm) %>% 
  rename(unemployment = val_norm)

write.csv(x = unemployment, file="data/unemployment.csv")


employment_rate<- get_cansim_vector("v2062952",
                                  start_time = "2000-01-01") %>% 
  select(Date,val_norm) %>% 
  rename(employment_rate = val_norm)

write.csv(x = employment_rate, file="data/employment_rate.csv")


job_vacancy_quarter<- get_cansim_vector("v104272652", 
                                        end_time = "2020-10-01") %>% 
  select(Date,val_norm) %>% 
  rename(job_vacancy = val_norm)


job_vacancy_monthly<- get_cansim_vector("v1212389364",
                                        start_time = "2020-11-01") %>% 
  select(Date,val_norm) %>% 
  rename(job_vacancy = val_norm)


job_vacancy <- 
  bind_rows(job_vacancy_quarter,
            job_vacancy_monthly)

write.csv(x = job_vacancy, file="data/job_vacancy.csv")


wages<- get_cansim_vector("v105812668",
                          start_time = "2000-01-01") %>% 
  select(Date,val_norm) %>% 
  rename(wages = val_norm)

write.csv(x = wages, file="data/wages.csv")

# --------------------- FINANCE -------------------------  # 



household_debt<- get_cansim_vector("v1231415625") %>% 
  select(Date,val_norm) %>% 
  rename(household_debt = val_norm)


government_debt<- get_cansim_vector("v52531047") %>% 
  select(Date,val_norm) %>% 
  rename(government_debt = val_norm)



nominal_debt<- get_cansim_vector("v62305783") %>% 
  select(Date,val_norm) %>% 
  rename(nominal_gdp = val_norm)

household_debt_gdp <- inner_join(household_debt,nominal_debt,by="Date")
household_gvm_debt_gdp <- inner_join(household_debt_gdp,government_debt,by="Date")

household_gvm_debt_gdp <- 
  household_gvm_debt_gdp %>% 
      mutate(Household = household_debt / nominal_gdp *100,
             Government = government_debt / nominal_gdp *100) %>% 
      select(Date, Household, Government)

write.csv(x = household_gvm_debt_gdp, file="data/debt.csv")

cpi<- get_cansim_vector("v41690973") %>% 
  select(Date,val_norm) %>% 
  rename(cpi = val_norm)

cpi <- 
  cpi %>%  mutate(yoy = ((cpi / lag(cpi, n=12))-1)*100) %>% 
  select(Date,yoy) %>% 
  filter(Date >= "1990-01-01")

write.csv(x = cpi, file="data/cpi.csv")

