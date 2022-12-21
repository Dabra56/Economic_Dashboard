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

# ----------------------- ECONOMY TABLE ------------------

vector_gdp <- c("v62787277",
                "v62787378",
                "v62787495",
                "v62787612",
                "v62787729",
                "v62787846",
                "v62787963",
                "v62788080",
                "v62788197",
                "v62788314",
                "v62788431")

province_vector <- c("Canada",
                     "Newfoundland and Labrador",
                     "Prince Edward Island",
                     "Nova Scotia",
                     "New Brunswick",
                     "Quebec",
                     "Ontario",
                     "Manitoba",
                     "Saskatchewan",
                     "Alberta",
                     "British Colombia")

dataframe_gdp <- data.frame()
  
for (i in 1:length(vector_gdp)) {
  
  gdp_province <- get_cansim_vector(vector_gdp[i]) 
  tail =  tail(gdp_province$Date,n=1)
  
    gdp_province <- 
      gdp_province  %>% 
        mutate(y_o_y = ((val_norm / lag(val_norm, n=1))-1)*100) %>% 
        filter(Date==tail) %>% 
        select(y_o_y) %>% bind_cols(province_vector[i]) 
    
    colnames(gdp_province)[2] <- "provinces"
    
    gdp_province <- 
      gdp_province %>% 
          select(provinces,y_o_y)
    
    dataframe_gdp <- 
      dataframe_gdp %>% 
        bind_rows(gdp_province)
  
}

dataframe_gdp <- 
  dataframe_gdp %>% 
    rename(GDP = y_o_y)

tail =  tail(manufacturing$Date,n=1)

manufacturing_canada <- 
  manufacturing %>% 
       mutate(m_o_m = ((manufacturing / lag(manufacturing, n=1))-1)*100) %>% 
  filter(Date==tail) %>% 
  select(m_o_m)
  

vector_manuf <- c("v803786",
                "v804246",
                "v804706",
                "v805166",
                "v805626",
                "v806086",
                "v806546",
                "v807006",
                "v807466",
                "v807928")


province_vector <- c("Newfoundland and Labrador",
                     "Prince Edward Island",
                     "Nova Scotia",
                     "New Brunswick",
                     "Quebec",
                     "Ontario",
                     "Manitoba",
                     "Saskatchewan",
                     "Alberta",
                     "British Colombia")


for (i in 1:length(vector_manuf)) {
  
  manuf_province <- get_cansim_vector(vector_manuf[i]) 
  tail =  tail(manuf_province$Date,n=1)
  
  manuf_province <- 
    manuf_province  %>% 
    mutate(m_o_m = ((val_norm / lag(val_norm, n=1))-1)*100) %>% 
    filter(Date==tail) %>% 
    select(m_o_m) %>% bind_cols(province_vector[i]) 
  
   colnames(manuf_province)[2] <- "provinces"
  
 manuf_province <- 
     manuf_province %>% 
     select(provinces,m_o_m)
   
   manufacturing_canada <- 
     manufacturing_canada %>% 
     bind_rows(manuf_province)
  
}

manufacturing_canada[1,2] <- "Canada"

manufacturing_canada <- 
  manufacturing_canada %>% 
  rename(Manufacturing_Sales = m_o_m)



vector_export <- c("v1001819785",
                     "v1001820916",
                     "v1001809606",
                     "v1001817523",
                     "v1001814130",
                     "v1001810737",
                     "v1001812999",
                     "v1001816392",
                     "v1001811868",
                     "v1001815261",
                     "v1001818654")

province_vector <- c("Alberta",
                          "British Colombia",
                          "Canada",
                          "Manitoba",
                          "New Brunswick",
                          "Newfoundland and Labrador",
                          "Nova Scotia",
                          "Ontario",
                          "Prince Edward Island",
                          "Quebec",
                          "Saskatchewan")

dataframe_export <- data.frame()

for (i in 1:length(vector_export)) {
  
  export <- get_cansim_vector(vector_export[i]) 
  tail =  tail(export$Date,n=1)
  
  export <- 
    export  %>% 
    mutate(m_o_m = ((val_norm / lag(val_norm, n=1))-1)*100) %>% 
    filter(Date==tail) %>% 
    select(m_o_m) %>% bind_cols(province_vector[i]) 
  
  colnames(export)[2] <- "provinces"
  
  export <- 
    export %>% 
    select(provinces,m_o_m)
  
  
  dataframe_export <- 
    dataframe_export %>% 
    bind_rows(export)
  
}

dataframe_export <- 
  dataframe_export %>% 
  rename(Export = m_o_m)



vector_retail <- c("v52367097",
                   "v52367394",
                   "v52367424",
                   "v52367454",
                   "v52367484",
                   "v52367514",
                   "v52367573",
                   "v52367155",
                   "v52367185",
                   "v52367215",
                   "v52367245")

province_vector <- c("Canada",
                     "Newfoundland and Labrador",
                     "Prince Edward Island",
                     "Nova Scotia",
                     "New Brunswick",
                     "Quebec",
                     "Ontario",
                     "Manitoba",
                     "Saskatchewan",
                     "Alberta",
                     "British Colombia")

dataframe_retail <- data.frame()

for (i in 1:length(vector_retail)) {
  
  retail <- get_cansim_vector(vector_retail[i]) 
  tail =  tail(retail$Date,n=1)
  
  retail <- 
    retail  %>% 
    mutate(m_o_m = ((val_norm / lag(val_norm, n=1))-1)*100) %>% 
    filter(Date==tail) %>% 
    select(m_o_m) %>% bind_cols(province_vector[i]) 
  
  colnames(retail)[2] <- "provinces"
  
  retail <- 
    retail %>% 
    select(provinces,m_o_m)
  
  
  dataframe_retail <- 
    dataframe_retail %>% 
    bind_rows(retail)
  
}

dataframe_retail <- 
  dataframe_retail %>% 
  rename(Retail = m_o_m)




vector_business <- c("v1203704156",
                   "v1203704232",
                   "v1203704384",
                   "v1203704460",
                   "v1203704612",
                   "v1203704840",
                   "v1203705296",
                   "v1203706588",
                   "v1203706740",
                   "v1203706968",
                   "v1203707272")

province_vector <- c("Canada",
                     "Newfoundland and Labrador",
                     "Prince Edward Island",
                     "Nova Scotia",
                     "New Brunswick",
                     "Quebec",
                     "Ontario",
                     "Manitoba",
                     "Saskatchewan",
                     "Alberta",
                     "British Colombia")

dataframe_business <- data.frame()

for (i in 1:length(vector_business)) {
  
 business <- get_cansim_vector(vector_business[i]) 
  tail =  tail(business$Date,n=1)
  
  business <- 
    business  %>% 
    mutate(m_o_m = ((val_norm / lag(val_norm, n=1))-1)*100) %>% 
    filter(Date==tail) %>% 
    select(m_o_m) %>% bind_cols(province_vector[i]) 
  
  colnames(business)[2] <- "provinces"
  
  business <- 
    business %>% 
    select(provinces,m_o_m)
  
  
  dataframe_business <- 
    dataframe_business %>% 
    bind_rows(business)
  
}

dataframe_business <- 
  dataframe_business %>% 
  rename(Active_Businesses = m_o_m)


dataframe_economy <- merge(x=dataframe_gdp,y=manufacturing_canada, by = "provinces")
dataframe_economy <- merge(x=dataframe_economy,y=dataframe_export, by = "provinces")
dataframe_economy <- merge(x=dataframe_economy,y=dataframe_retail, by = "provinces")
dataframe_economy <- merge(x=dataframe_economy,y=dataframe_business, by = "provinces")

dataframe_economy <- 
  dataframe_economy %>%
  arrange(factor(provinces,levels=c("Canada",
                                    "Alberta",
                                    "British Colombia",
                                    "Manitoba",
                                    "New Brunswick",
                                    "Newfoundland and Labrador",
                                    "Nova Scotia",
                                    "Ontario",
                                    "Prince Edward Island",
                                    "Quebec",
                                    "Saskatchewan")))



dataframe_economy <- 
  dataframe_economy %>% 
    mutate(ColorGDP = case_when(
                      round(GDP,digits = 2) < 0  ~ "RED", 
                      round(GDP,digits = 2) == 0  ~ "YELLOW",
                      round(GDP,digits = 2) > 0  ~ "GREEN"),
           ColorManuf = case_when(
             round(Manufacturing_Sales,digits = 2) < 0  ~ "RED", 
             round(Manufacturing_Sales,digits = 2) == 0  ~ "YELLOW",
             round(Manufacturing_Sales,digits = 2) > 0  ~ "GREEN"), 
           ColorExport= case_when(
             round(Export,digits = 2) < 0  ~ "RED", 
             round(Export,digits = 2) == 0  ~ "YELLOW",
             round(Export,digits = 2) > 0  ~ "GREEN"), 
           ColorRetail= case_when(
             round(Retail,digits = 2) < 0  ~ "RED", 
             round(Retail,digits = 2) == 0  ~ "YELLOW",
             round(Retail,digits = 2) > 0  ~ "GREEN"),
           ColorActiveBusiness= case_when(
             round(Active_Businesses,digits = 2) < 0  ~ "RED", 
             round(Active_Businesses,digits = 2) == 0  ~ "YELLOW",
             round(Active_Businesses,digits = 2) > 0  ~ "GREEN")) %>% 
  select(provinces, GDP, ColorGDP, Manufacturing_Sales,ColorManuf,Export,ColorExport,Retail,ColorRetail,Active_Businesses,ColorActiveBusiness)


write.csv(x = dataframe_economy, file="data/table_economy.csv")


# ---------------------------- INFLATION TABLE -----------------------------

cpi_all_items  <- get_cansim_vector("v41690973")

tail =  tail(cpi_all_items$Date,n=1)

cpi_all_items <- 
  cpi_all_items %>% 
      mutate(y_o_y = ((val_norm / lag(val_norm, n=12))-1)*100,
             m_o_m = ((val_norm / lag(val_norm, n=1))-1)*100,
             component = "Consumer price index (CPI)") %>% 
      filter(Date==tail) %>% 
      select(component, y_o_y,m_o_m)

cpi_no_energy <- get_cansim_vector("v41691238")%>% 
  mutate(y_o_y = ((val_norm / lag(val_norm, n=12))-1)*100,
         m_o_m = ((val_norm / lag(val_norm, n=1))-1)*100,
         component = "CPI without energy") %>% 
  filter(Date==tail) %>% 
  select(component, y_o_y,m_o_m)

cpi_food <- get_cansim_vector("v41690974")%>% 
  mutate(y_o_y = ((val_norm / lag(val_norm, n=12))-1)*100,
         m_o_m = ((val_norm / lag(val_norm, n=1))-1)*100,
         component = "Food") %>% 
  filter(Date==tail) %>% 
  select(component, y_o_y,m_o_m)

cpi_shelter<- get_cansim_vector("v41691050")%>% 
  mutate(y_o_y = ((val_norm / lag(val_norm, n=12))-1)*100,
         m_o_m = ((val_norm / lag(val_norm, n=1))-1)*100,
         component = "Shelter") %>% 
  filter(Date==tail) %>% 
  select(component, y_o_y,m_o_m)

cpi_transport<- get_cansim_vector("v41691128")%>% 
  mutate(y_o_y = ((val_norm / lag(val_norm, n=12))-1)*100,
         m_o_m = ((val_norm / lag(val_norm, n=1))-1)*100,
         component = "Transportation") %>% 
  filter(Date==tail) %>% 
  select(component, y_o_y,m_o_m)

cpi_good<- get_cansim_vector("v41691222")%>% 
  mutate(y_o_y = ((val_norm / lag(val_norm, n=12))-1)*100,
         m_o_m = ((val_norm / lag(val_norm, n=1))-1)*100,
         component = "Good") %>% 
  filter(Date==tail) %>% 
  select(component, y_o_y,m_o_m)

cpi_services<- get_cansim_vector("v41691230")%>% 
  mutate(y_o_y = ((val_norm / lag(val_norm, n=12))-1)*100,
         m_o_m = ((val_norm / lag(val_norm, n=1))-1)*100,
         component = "Services") %>% 
  filter(Date==tail) %>% 
  select(component, y_o_y,m_o_m)

inflation_table <- bind_rows(cpi_all_items,cpi_no_energy,cpi_food, cpi_shelter,cpi_transport, cpi_good,cpi_services)

colnames(inflation_table) <- c("","Year/Year","Month/Month")

write.csv(x = inflation_table, file="data/table_inflation.csv")


# ---------------------------- EMPLOYMENT TABLE -----------------------------

# Total jobs

vector_jobs <- c("v2062811",
                     "v2063000",
                     "v2063189",
                     "v2063378",
                     "v2063567",
                     "v2063756",
                     "v2063945",
                     "v2064134",
                     "v2064323",
                     "v2064512",
                     "v2064701")

province_vector <- c("Canada",
                     "Newfoundland and Labrador",
                     "Prince Edward Island",
                     "Nova Scotia",
                     "New Brunswick",
                     "Quebec",
                     "Ontario",
                     "Manitoba",
                     "Saskatchewan",
                     "Alberta",
                     "British Colombia")

dataframe_jobs <- data.frame()

for (i in 1:length(vector_jobs)) {
  
 jobs <- get_cansim_vector(vector_jobs[i]) 
  tail =  tail(jobs$Date,n=1)
  
  jobs <- 
    jobs  %>% 
    mutate(m_o_m = ((val_norm / lag(val_norm, n=1))-1)*100,
           y_o_y = ((val_norm / lag(val_norm, n=12))-1)*100) %>% 
    filter(Date==tail) %>% 
    select(m_o_m,y_o_y) %>% bind_cols(province_vector[i]) 

  
  dataframe_jobs <- 
    dataframe_jobs %>% 
    bind_rows(jobs)
  
}


dataframe_jobs <- 
  dataframe_jobs %>% 
  select(...3,m_o_m,y_o_y) 

colnames(dataframe_jobs)<- c("provinces","Job variations ^M/M^","Job variations ^Y/Y^")
  

# Unemployment

vector_unemployment <- c("v2062815",
                 "v2063004",
                 "v2063193",
                 "v2063382",
                 "v2063571",
                 "v2063760",
                 "v2063949",
                 "v2064138",
                 "v2064327",
                 "v2064516",
                 "v2064705")

province_vector <- c("Canada",
                     "Newfoundland and Labrador",
                     "Prince Edward Island",
                     "Nova Scotia",
                     "New Brunswick",
                     "Quebec",
                     "Ontario",
                     "Manitoba",
                     "Saskatchewan",
                     "Alberta",
                     "British Colombia")

dataframe_unemployment <- data.frame()

for (i in 1:length(vector_unemployment)) {
  
  unemployment <- get_cansim_vector(vector_unemployment[i]) 
  tail =  tail(unemployment$Date,n=1)
  
  unemployment <- 
    unemployment   %>% 
    filter(Date==tail) %>% 
    select(val_norm) %>% bind_cols(province_vector[i]) 
  
  
  dataframe_unemployment <- 
    dataframe_unemployment %>% 
    bind_rows(unemployment)
  
}

colnames(dataframe_unemployment)[2] <- "provinces"
dataframe_unemployment <- 
  dataframe_unemployment %>% 
  select(provinces,val_norm) 

colnames(dataframe_unemployment)[2] <- "Unemployement rate"


# Employment rate 25 - 54 


vector_employment <- c("v2062952",
                         "v2063141",
                         "v2063330",
                         "v2063519",
                         "v2063708",
                         "v2063897",
                         "v2064086",
                         "v2064275",
                         "v2064464",
                         "v2064653",
                         "v2064842")

province_vector <- c("Canada",
                     "Newfoundland and Labrador",
                     "Prince Edward Island",
                     "Nova Scotia",
                     "New Brunswick",
                     "Quebec",
                     "Ontario",
                     "Manitoba",
                     "Saskatchewan",
                     "Alberta",
                     "British Colombia")

dataframe_employment <- data.frame()

for (i in 1:length(vector_employment)) {
  
  employment <- get_cansim_vector(vector_employment[i]) 
  tail =  tail(employment$Date,n=1)
  
  employment <- 
    employment   %>% 
    filter(Date==tail) %>% 
    select(val_norm) %>% bind_cols(province_vector[i]) 
  
  
  dataframe_employment <- 
    dataframe_employment %>% 
    bind_rows(employment)
  
}

colnames(dataframe_employment)[2] <- "provinces"
dataframe_employment <- 
  dataframe_employment %>% 
  select(provinces,val_norm) 

colnames(dataframe_employment)[2] <- "Employement rate ^25-54 years old^"


# Job vacancy rate

vector_vacancy <- c("v1212389365",
                       "v1212389368",
                       "v1212389371",
                       "v1212389374",
                       "v1212389377",
                       "v1212389380",
                       "v1212389383",
                       "v1212389386",
                       "v1212389389",
                       "v1212389392",
                       "v1212389395")

province_vector <- c("Canada",
                     "Newfoundland and Labrador",
                     "Prince Edward Island",
                     "Nova Scotia",
                     "New Brunswick",
                     "Quebec",
                     "Ontario",
                     "Manitoba",
                     "Saskatchewan",
                     "Alberta",
                     "British Colombia")

dataframe_vacancy <- data.frame()

for (i in 1:length(vector_vacancy)) {
  
  vacancy <- get_cansim_vector(vector_vacancy[i]) 
  tail =  tail(vacancy$Date,n=1)
  
  vacancy <- 
    vacancy   %>% 
    filter(Date==tail) %>% 
    select(val_norm) %>% bind_cols(province_vector[i]) 
  
  
  dataframe_vacancy <- 
    dataframe_vacancy %>% 
    bind_rows(vacancy)
  
}

colnames(dataframe_vacancy)[2] <- "provinces"
dataframe_vacancy <- 
  dataframe_vacancy %>% 
  select(provinces,val_norm) 

colnames(dataframe_vacancy)[2] <- "Job vacancy rate"


# MERGE

dataframe_labor <- merge(x=dataframe_jobs,y=dataframe_unemployment, by = "provinces")
dataframe_labor <- merge(x=dataframe_labor,y=dataframe_employment, by = "provinces")
dataframe_labor <- merge(x=dataframe_labor,y=dataframe_vacancy, by = "provinces")

dataframe_labor <- 
  dataframe_labor %>%
    arrange(factor(provinces,levels=c("Canada",
                                      "Alberta",
                                      "British Colombia",
                                      "Manitoba",
                                      "New Brunswick",
                                      "Newfoundland and Labrador",
                                      "Nova Scotia",
                                      "Ontario",
                                      "Prince Edward Island",
                                      "Quebec",
                                      "Saskatchewan")))


write.csv(x = dataframe_labor, file="data/table_labor.csv")

