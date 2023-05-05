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

# Extracting the last date to use in column names 

tail_manufacturing <-
  tail(manufacturing$Date,n=1) %>% 
    format("%Y-%m")

retail_date <- get_cansim_vector("v52367097") 

tail_retail <-
  tail(retail_date$Date,n=1) %>% 
  format("%Y-%m")

export_date <- get_cansim_vector("v1001819785") 

tail_export <-
  tail(export_date$Date,n=1) %>% 
  format("%Y-%m")

tail_business <-
  tail(businesses$Date,n=1) %>% 
  format("%Y-%m")


colnames(dataframe_economy) <- c("Provinces", 
                                "GDP ^Y/Y^", 
                                "ColorGDP",
                                paste0("Manufacturing sales","^",tail_manufacturing,"^"),
                                "ColorManuf",
                                paste0("Export","^",tail_export,"^"),
                                "ColorExport",
                                paste0("Retail sales","^",tail_retail,"^"),
                                "ColorRetail",
                                paste0("Active businesses","^",tail_business,"^"),
                                "ColorBusinesses")

fr_dataframe_economy <- 
  dataframe_economy 
    
  fr_dataframe_economy[3,1] <- "Colombie-Britannique"
  fr_dataframe_economy[5,1] <- "Nouveau-Brunswick"
  fr_dataframe_economy[6,1] <- "Terre-Neuve-et-Labrador"
  fr_dataframe_economy[7,1] <- "Nouvelle-Écosse"
  fr_dataframe_economy[9,1] <- "Île-du-Prince-Édouard"
  fr_dataframe_economy[10,1] <- "Québec"
  

colnames(fr_dataframe_economy) <- c("Provinces", 
                                 "PIB ^A/A^", 
                                 "ColorGDP",
                                 paste0("Ventes manufacturières","^",tail_manufacturing,"^"),
                                 "ColorManuf",
                                 paste0("Exportations","^",tail_export,"^"),
                                 "ColorExport",
                                 paste0("Ventes au détail","^",tail_retail,"^"),
                                 "ColorRetail",
                                 paste0("Entreprises actives","^",tail_business,"^"),
                                 "ColorBusinesses")



write.csv(x = dataframe_economy, file="data/table_economy.csv")
write.csv(x = fr_dataframe_economy, file="data/fr_table_economy.csv")

# ---------------------------- INFLATION TABLE -----------------------------

cpi_all_items  <- get_cansim_vector("v41690973")

tail =  tail(cpi_all_items$Date,n=1)

last_date <- 
  cpi_all_items %>% 
  filter(Date==tail) %>% 
  select(Date) %>% 
  mutate(Date=format(format(Date, "%Y-%m")))


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

inflation_table <- 
  inflation_table %>% 
    bind_cols(last_date) %>% 
      select(component,Date,y_o_y,m_o_m)

colnames(inflation_table) <- c("","Date","Year/Year","Month/Month")



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

colnames(dataframe_unemployment)[2] <- "Unemployment rate"


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

colnames(dataframe_employment)[2] <- "Employment rate ^25-54 years old^"


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


employment_date <- get_cansim_vector("v2062952") 

tail_employment <-
  tail(employment_date$Date,n=1) %>% 
  format("%Y-%m") 

vacancy_date <- get_cansim_vector("v1212389365") 

tail_vacancy <-
  tail(vacancy_date$Date,n=1) %>% 
  format("%Y-%m")

colnames(dataframe_labor)[4] <- paste0("Unemployment rate","^",tail_employment,"^")
colnames(dataframe_labor)[6] <- paste0("Job vacancy rate","^",tail_vacancy,"^")

write.csv(x = dataframe_labor, file="data/table_labor.csv")



# ----------------------------- PROVINCE SUMMARY TABLE ------------------------------------- # 



#GDP 

cansim_gdp_vector <- c("v62787277",
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

cansim_gdp_province <-  c("Canada",
                          "Newfoundland_and_Labrador",
                          "Prince_Edward_Island",
                          "Nova_Scotia",
                          "New_Brunswick",
                          "Quebec",
                          "Ontario",
                          "Manitoba",
                          "Saskatchewan",
                          "Alberta",
                          "British_Colombia")


vector_manuf <- c("v800450",
                  "v803786",
                  "v804246",
                  "v804706",
                  "v805166",
                  "v805626",
                  "v806086",
                  "v806546",
                  "v807006",
                  "v807466",
                  "v807928")  


vector_export <- c("v1001809606",
                   "v1001810737",
                   "v1001811868",
                   "v1001812999",
                   "v1001814130",
                   "v1001815261",
                   "v1001816392",
                   "v1001817523",
                   "v1001818654",
                   "v1001819785",
                   "v1001820916")



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

vector_vacancy <- c("v1212389364",
                    "v1212389367",
                    "v1212389370",
                    "v1212389373",
                    "v1212389376",
                    "v1212389379",
                    "v1212389382",
                    "v1212389385",
                    "v1212389388",
                    "v1212389391",
                    "v1212389394")



vector_vacancy_rate <- c("v1212389365",
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


vector_wages <- c("v2133245",
                     "v2136665",
                     "v2140085",
                     "v2143505",
                     "v2146925",
                     "v2150345",
                     "v2153765",
                     "v2157185",
                     "v2160605",
                     "v2164025",
                     "v2167445")


vector_inflation <- c("v41690973",
                  "v41691244",
                  "v41691379",
                  "v41691513",
                  "v41691648",
                  "v41691783",
                  "v41691919",
                  "v41692055",
                  "v41692191",
                  "v41692327",
                  "v41692462")

vector_immigration <- c("v29850342",
                      "v29850347",
                      "v29850352",
                      "v29850357",
                      "v29850362",
                      "v29850367",
                      "v29850372",
                      "v29850377",
                      "v29850382",
                      "v29850387",
                      "v29850392")



# ----------------- ISOLATING THE CANADA VALUES ------------------------------

#--------------------------------------
gdp_canada <- get_cansim_vector(cansim_gdp_vector[1]) 
  tail =  tail(gdp_canada$Date,n=1)
  
gdp_canada_last_value <- 
  gdp_canada %>% 
      filter(Date==tail) %>% 
  select(val_norm)

#--------------------------------------
manuf_canada <- get_cansim_vector(vector_manuf[1]) 
tail =  tail(manuf_canada$Date,n=1)

manuf_canada_last_value <- 
  manuf_canada %>% 
  filter(Date==tail) %>% 
  select(val_norm)


#--------------------------------------
export_canada <- get_cansim_vector(vector_export[1]) 
tail =  tail(export_canada$Date,n=1)

export_canada_last_value <- 
  export_canada %>% 
  filter(Date==tail) %>% 
  select(val_norm)

#--------------------------------------
retail_canada <- get_cansim_vector(vector_retail[1]) 
tail =  tail(retail_canada$Date,n=1)

retail_canada_last_value <- 
  retail_canada %>% 
  filter(Date==tail) %>% 
  select(val_norm)

#--------------------------------------
business_canada <- get_cansim_vector(vector_business[1]) 
tail =  tail(business_canada$Date,n=1)

business_canada_last_value <- 
  business_canada %>% 
  filter(Date==tail) %>% 
  select(val_norm)

#--------------------------------------
jobs_canada <- get_cansim_vector(vector_jobs[1]) 
tail =  tail(jobs_canada$Date,n=1)

jobs_canada_last_value <- 
  jobs_canada %>% 
  filter(Date==tail) %>% 
  select(val_norm)

#--------------------------------------
unemployment_canada <- get_cansim_vector(vector_unemployment[1]) 
tail =  tail(unemployment_canada$Date,n=1)

unemployment_canada_last_value <- 
  unemployment_canada %>% 
  filter(Date==tail) %>% 
  select(val_norm)


#--------------------------------------
employment_canada <- get_cansim_vector(vector_employment[1]) 
tail =  tail(employment_canada$Date,n=1)

employment_canada_last_value <- 
  employment_canada %>% 
  filter(Date==tail) %>% 
  select(val_norm)



#--------------------------------------
vacancy_canada <- get_cansim_vector(vector_vacancy[1]) 
tail =  tail(vacancy_canada$Date,n=1)

vacancy_canada_last_value <- 
  vacancy_canada %>% 
  filter(Date==tail) %>% 
  select(val_norm)

#--------------------------------------
wages_canada <- get_cansim_vector(vector_wages[1]) 
tail =  tail(wages_canada$Date,n=1)

wages_canada_last_value <- 
  wages_canada %>% 
  filter(Date==tail) %>% 
  select(val_norm)

#--------------------------------------
inflation_canada <- get_cansim_vector(vector_inflation[1]) 
tail =  tail(inflation_canada$Date,n=1)

inflation_canada_last_value <- 
  inflation_canada %>% 
  mutate(y_o_y = ((val_norm/lag(val_norm,n=12))-1)*100) %>% 
  filter(Date==tail) %>% 
  select(y_o_y)

#--------------------------------------
immigration_canada <- get_cansim_vector(vector_immigration[1]) 
tail =  tail(immigration_canada$Date,n=1)

immigration_canada_last_value <- 
  immigration_canada  %>% 
  filter(Date==tail) %>% 
  select(val_norm)


for (i in 2:length(vector_vacancy)) {
 
  # GDP 
  
  gdp_province <- get_cansim_vector(cansim_gdp_vector[i]) 
  tail =  tail(gdp_province$Date,n=1)
  
  
  gdp_province <- 
    gdp_province  %>% 
      mutate(Date_mod=format(Date, "%Y"),
              value = paste0("$",round(val_norm/1000000000,digits=1),"^Billion^"),
             m_o_m = NA,
             color_mom = NA,
             y_o_y = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^Y/Y^"),
             color_yoy = case_when(
               round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
               round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
               round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
            indicators = "Real gross domestic product (GDP)
^Annual adjusted for inflation^")%>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(round(val_norm/gdp_canada_last_value*100,digits=1),"%")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)

  
# Manufacturing 
  
 manuf_province <- get_cansim_vector(vector_manuf[i]) 
  tail =  tail(manuf_province$Date,n=1)
  
  
  manuf_province <- 
    manuf_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0("$",round(val_norm/1000000000,digits=1),"^Billion^"),
           m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^M/M^"),
           y_o_y = paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),"%","^Y/Y^"),
           indicators = "Manufacturing sales ^Monthly^",
            color_mom = case_when(
              round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
              round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
              round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
            color_yoy = case_when(
              round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
              round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
              round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(round(val_norm/manuf_canada_last_value*100,digits=1),"%")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)

  
  # Export 
  
  export_province <- get_cansim_vector(vector_export[i]) 
  tail =  tail(export_province$Date,n=1)
  
  
  export_province <- 
    export_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0("$",round(val_norm/1000000000,digits=1),"^Billion^"),
           m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^M/M^"),
           y_o_y = paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),"%","^Y/Y^"),
           indicators = "Exports ^Monthly^",
           color_mom = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(round(val_norm/export_canada_last_value*100,digits=1),"%")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  # Retail trade 
  
  
retail_province <- get_cansim_vector(vector_retail[i]) 
  tail =  tail(retail_province$Date,n=1)
  
  
  retail_province <- 
    retail_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0("$",round(val_norm/1000000000,digits=1),"^Billion^"),
           m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^M/M^"),
           y_o_y =  paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),"%","^Y/Y^"),
           indicators = "Retail sales ^monthly^",
           color_mom = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(round(val_norm/retail_canada_last_value*100,digits=1),"%")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  
  #Business 
  
  business_province <- get_cansim_vector(vector_business[i]) 
  tail =  tail(business_province$Date,n=1)
  
  
  business_province <- 
    business_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0(round(val_norm/1000,digits=0)," K"),
           m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^M/M^"),
           y_o_y =  paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),"%","^Y/Y^"),
           indicators = "Active businesses^Total number^",
           color_mom = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN"))  %>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(round(val_norm/business_canada_last_value*100,digits=1),"%")) %>%  
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  # Total jobs 

  job_province <- get_cansim_vector(vector_jobs[i]) 
  tail =  tail(job_province$Date,n=1)
  
  
  job_province <- 
    job_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0(round(val_norm/1000000,digits=1),"^Million jobs^"),
           m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^M/M^"),
           y_o_y =  paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),"%","^Y/Y^"),
           indicators = "Total employed ^Population with a job^",
           color_mom = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(round(val_norm/jobs_canada_last_value*100,digits=1),"%")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
 
   
  # Unemployment rate 
  
  unemployment_province <- get_cansim_vector(vector_unemployment[i]) 
  tail =  tail(unemployment_province$Date,n=1)
  
  
  unemployment_province <- 
    unemployment_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0(round(val_norm,digits=1),"%"),
           m_o_m = paste0(round((((val_norm - lag(val_norm, n=1)))),digits=1)," p.p.","^M/M^"),
           y_o_y =  paste0(round((((val_norm - lag(val_norm, n=12)))),digits=1)," p.p.","^Y/Y^"),
           indicators = "Unemployment rate ^% of active workforce unemployed^",
           color_mom = case_when(
             round((((val_norm - lag(val_norm, n=1)))),digits=1) > 0  ~ "RED", 
             round((((val_norm - lag(val_norm, n=1)))),digits=1) == 0  ~ "YELLOW",
             round((((val_norm - lag(val_norm, n=1)))),digits=1) < 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm - lag(val_norm, n=12)))),digits=1) > 0  ~ "RED", 
             round((((val_norm - lag(val_norm, n=12)))),digits=1) == 0  ~ "YELLOW",
             round((((val_norm - lag(val_norm, n=12)))),digits=1) < 0  ~ "GREEN")) %>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(round(unemployment_canada_last_value,digits=1),"%","^in Canada^")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
 
  
  # Employment rate 
  
  employment_province <- get_cansim_vector(vector_employment[i]) 
  tail =  tail(employment_province$Date,n=1)
  
  
  employment_province <- 
    employment_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0(round(val_norm,digits=1),"%"),
           m_o_m = paste0(round((((val_norm - lag(val_norm, n=1)))),digits=1)," p.p.","^M/M^"),
           y_o_y =  paste0(round((((val_norm - lag(val_norm, n=12)))),digits=1)," p.p.","^Y/Y^"),
           indicators = "Employment rate ^% of total population holding a job (25 - 54 years old)^",
           color_mom = case_when(
             round((((val_norm - lag(val_norm, n=1)))),digits=1) < 0  ~ "RED", 
             round((((val_norm - lag(val_norm, n=1)))),digits=1) == 0  ~ "YELLOW",
             round((((val_norm - lag(val_norm, n=1)))),digits=1) > 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm - lag(val_norm, n=12)))),digits=1) < 0  ~ "RED", 
             round((((val_norm - lag(val_norm, n=12)))),digits=1) == 0  ~ "YELLOW",
             round((((val_norm - lag(val_norm, n=12)))),digits=1) > 0  ~ "GREEN")) %>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(round(employment_canada_last_value,digits=1),"%","^in Canada^")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  
 
  # Job vacancies 
  
  vacancy_rate_province <- get_cansim_vector(vector_vacancy_rate[i]) 
  tail =  tail(vacancy_rate_province$Date,n=1)
  
  vacancy_rate_province <- 
    vacancy_rate_province %>% 
    filter(Date==tail) %>% 
    select(val_norm)
  
  
  vacancy_province <- get_cansim_vector(vector_vacancy[i]) 
  tail =  tail(vacancy_province$Date,n=1)
  
  
  vacancy_province <- 
    vacancy_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0(round(val_norm/1000,digits=0)," K","^",vacancy_rate_province," % of payroll employees^"),
           m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^M/M^"),
           y_o_y =  paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),"%","^Y/Y^"),
           indicators = "Job vacancies ^Number of unfilled positions^",
           color_mom = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "GREEN"))%>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(round(val_norm/vacancy_canada_last_value*100,digits=1),"%")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  # Weekly earnings 
  
  wages_province <- get_cansim_vector(vector_wages[i]) 
  tail =  tail(wages_province$Date,n=1)
  
  
  wages_province <- 
    wages_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0("$",round(val_norm,digits=0)),
           m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^M/M^"),
           y_o_y =  paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),"%","^Y/Y^"),
           indicators = "Average weekly earnings ^Full-time workers (25 - 54 years old)^",
           color_mom = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN"))%>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0("$",round(wages_canada_last_value,digits=0),"^in Canada^")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  
  
  # Inflation
  
  inflation_province <- get_cansim_vector(vector_inflation[i]) 
  tail =  tail(inflation_province$Date,n=1)
  
  
  inflation_province <- 
    inflation_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),"%","^Y/Y^"),
           m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^M/M^"),
           y_o_y = NA ,
           indicators = "Inflation ^Consumer price index (CPI) variations^",
           color_mom = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "GREEN"),
           color_yoy = NA )%>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(round(inflation_canada_last_value,digits=1),"%","^in Canada^")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
    
  
  
  # New immigrants
  
  immigration_province <- get_cansim_vector(vector_immigration[i]) 
  tail =  tail(immigration_province$Date,n=1)
  
  
  immigration_province <- 
    immigration_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0(round(val_norm/1000,digits=1)," K"),
           m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^Q/Q^"),
           y_o_y = paste0(round((((val_norm / lag(val_norm, n=4))-1)*100),digits=1),"%","^Y/Y^"),
           indicators = "New immigrants ^Quarterly^",
           color_mom = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm / lag(val_norm, n=4))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=4))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=4))-1)*100),digits=1) > 0  ~ "GREEN"))%>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(round(val_norm/immigration_canada_last_value*100,digits=1),"%")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  
  
  
  
  economy_line <- data.frame("Economy",NA,NA,NA,NA,NA,NA,NA)   
  colnames(economy_line) <- c("indicators","value","Date_mod","cont_canada","m_o_m","color_mom","y_o_y","color_yoy")
  
labormarket_line <- data.frame("Labour market",NA,NA,NA,NA,NA,NA,NA) 
colnames(labormarket_line) <- c("indicators","value","Date_mod","cont_canada","m_o_m","color_mom","y_o_y","color_yoy")


  province_table <- bind_rows(economy_line,
                              gdp_province,
                              manuf_province,
                              export_province,
                              retail_province,
                              business_province,
                              labormarket_line,
                              job_province,
                              unemployment_province,
                              employment_province,
                              vacancy_province,
                              wages_province,
                              inflation_province,
                              immigration_province
                              )

  colnames(province_table) <- c("Indicators","Value","Date","Contribution to Canada","Recent variations","ColorMoM","","ColorYoY") 
  
  
    file_name = paste0("data/",cansim_gdp_province[i],"_table.csv")
     write.csv(x = province_table, file=file_name)
    
    
   #assign(x=paste0(cansim_gdp_province[i],"_table"),value=province_table,envir = .GlobalEnv)
  
}


# French table  for provinces 

cansim_gdp_province <-  c("fr_Canada_",
                          "fr_Terre_Neuve_Labrador",
                          "fr_Ile_Prince_Edouard",
                          "fr_Nouvelle_Ecosse",
                          "fr_Nouveau_Brunswick",
                          "fr_Quebec",
                          "fr_Ontario",
                          "fr_Manitoba",
                          "fr_Saskatchewan",
                          "fr_Alberta",
                          "fr_Colombie_Britannique")




for (i in 2:length(vector_vacancy)) {
  
  # GDP 
  
  gdp_province <- get_cansim_vector(cansim_gdp_vector[i]) 
  tail =  tail(gdp_province$Date,n=1)
  
  
  gdp_province <- 
    gdp_province  %>% 
    mutate(Date_mod=format(Date, "%Y"),
           value = paste0(format(round(val_norm/1000000000,digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE),"^Milliards de $^"),
           m_o_m = NA,
           color_mom = NA,
           y_o_y = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^A/A^"),
           color_yoy = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
           indicators = "Produit intérieur brut (PIB) réel
^Sur une base annuelle et ajusté en fonction de l'inflation^")%>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(format(round(val_norm/gdp_canada_last_value*100,digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  
  # Manufacturing 
  
  manuf_province <- get_cansim_vector(vector_manuf[i]) 
  tail =  tail(manuf_province$Date,n=1)
  
  
  manuf_province <- 
    manuf_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0(format(round(val_norm/1000000000,digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE),"^Milliards de $^"),
           m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^M/M^"),
           y_o_y = paste0(format(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^A/A^"),
           indicators = "Ventes manufacturières ^Données mensuelles^",
           color_mom = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(format(round(val_norm/manuf_canada_last_value*100,digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  
  # Export 
  
  export_province <- get_cansim_vector(vector_export[i]) 
  tail =  tail(export_province$Date,n=1)
  
  
  export_province <- 
    export_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0(format(round(val_norm/1000000000,digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE),"^Milliards de $^"),
           m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^M/M^"),
           y_o_y = paste0(format(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^A/A^"),
           indicators = "Exportations ^Données mensuelles^",
           color_mom = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(format(round(val_norm/export_canada_last_value*100,digits=1),decimal.mark=",")," %")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  # Retail trade 
  
  
  retail_province <- get_cansim_vector(vector_retail[i]) 
  tail =  tail(retail_province$Date,n=1)
  
  
  retail_province <- 
    retail_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0(format(round(val_norm/1000000000,digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE),"^Milliards de $^"),
           m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^M/M^"),
           y_o_y =  paste0(format(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^A/A^"),
           indicators = "Ventes au détail ^Données mensuelles^",
           color_mom = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(format(round(val_norm/retail_canada_last_value*100,digits=1),decimal.mark=",")," %")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  
  #Business 
  
  business_province <- get_cansim_vector(vector_business[i]) 
  tail =  tail(business_province$Date,n=1)
  
  
  business_province <- 
    business_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0(format(round(val_norm/1000,digits=0),big.mark=" ")," K"),
           m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^M/M^"),
           y_o_y =  paste0(format(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^A/A^"),
           indicators = "Entreprises actives",
           color_mom = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN"))  %>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(format(round(val_norm/business_canada_last_value*100,digits=1),decimal.mark=",")," %")) %>%  
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  # Total jobs 
  
  job_province <- get_cansim_vector(vector_jobs[i]) 
  tail =  tail(job_province$Date,n=1)
  
  
  job_province <- 
    job_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0(format(round(val_norm/1000000,digits=1),decimal.mark=","),"^Millions d'emplois^"),
           m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^M/M^"),
           y_o_y =  paste0(format(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^A/A^"),
           indicators = "Emplois totaux ^Population avec un emploi^",
           color_mom = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(format(round(val_norm/jobs_canada_last_value*100,digits=1),decimal.mark=",")," %")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  
  # Unemployment rate 
  
  unemployment_province <- get_cansim_vector(vector_unemployment[i]) 
  tail =  tail(unemployment_province$Date,n=1)
  
  
  unemployment_province <- 
    unemployment_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0(format(round(val_norm,digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %"),
           m_o_m = paste0(format(round((((val_norm - lag(val_norm, n=1)))),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," p.p.","^M/M^"),
           y_o_y =  paste0(format(round((((val_norm - lag(val_norm, n=12)))),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," p.p.","^A/A^"),
           indicators = "Taux de chômage ^% de la population active sans emploi^",
           color_mom = case_when(
             round((((val_norm - lag(val_norm, n=1)))),digits=1) > 0  ~ "RED", 
             round((((val_norm - lag(val_norm, n=1)))),digits=1) == 0  ~ "YELLOW",
             round((((val_norm - lag(val_norm, n=1)))),digits=1) < 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm - lag(val_norm, n=12)))),digits=1) > 0  ~ "RED", 
             round((((val_norm - lag(val_norm, n=12)))),digits=1) == 0  ~ "YELLOW",
             round((((val_norm - lag(val_norm, n=12)))),digits=1) < 0  ~ "GREEN")) %>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(format(round(unemployment_canada_last_value[[1]],digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^au Canada^")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  
  # Employment rate 
  
  employment_province <- get_cansim_vector(vector_employment[i]) 
  tail =  tail(employment_province$Date,n=1)
  
  
  employment_province <- 
    employment_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0(format(round(val_norm,digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %"),
           m_o_m = paste0(format(round((((val_norm - lag(val_norm, n=1)))),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," p.p.","^M/M^"),
           y_o_y =  paste0(format(round((((val_norm - lag(val_norm, n=12)))),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," p.p.","^A/A^"),
           indicators = "Taux d'emploi ^% de la population avec un emploi (25 - 54 ans)^",
           color_mom = case_when(
             round((((val_norm - lag(val_norm, n=1)))),digits=1) < 0  ~ "RED", 
             round((((val_norm - lag(val_norm, n=1)))),digits=1) == 0  ~ "YELLOW",
             round((((val_norm - lag(val_norm, n=1)))),digits=1) > 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm - lag(val_norm, n=12)))),digits=1) < 0  ~ "RED", 
             round((((val_norm - lag(val_norm, n=12)))),digits=1) == 0  ~ "YELLOW",
             round((((val_norm - lag(val_norm, n=12)))),digits=1) > 0  ~ "GREEN")) %>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(format(round(employment_canada_last_value[[1]],digits=1),decimal.mark=",")," %","^au Canada^")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  
  
  # Job vacancies 
  
  vacancy_rate_province <- get_cansim_vector(vector_vacancy_rate[i]) 
  tail =  tail(vacancy_rate_province$Date,n=1)
  
  vacancy_rate_province <- 
    vacancy_rate_province %>% 
    filter(Date==tail) %>% 
    select(val_norm)
  
  
  vacancy_province <- get_cansim_vector(vector_vacancy[i]) 
  tail =  tail(vacancy_province$Date,n=1)
  
  
  vacancy_province <- 
    vacancy_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0(round(val_norm/1000,digits=0)," K","^",format(vacancy_rate_province[[1]],decimal.mark=",")," % des employés^"),
           m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^M/M^"),
           y_o_y =  paste0(format(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^A/A^"),
           indicators = "Postes vacants ^Nombre de postes non-comblés^",
           color_mom = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "GREEN"))%>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(format(round(val_norm/vacancy_canada_last_value*100,digits=1),decimal.mark=",")," %")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  # Weekly earnings 
  
  wages_province <- get_cansim_vector(vector_wages[i]) 
  tail =  tail(wages_province$Date,n=1)
  
  
  wages_province <- 
    wages_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0(format(round(val_norm,digits=0),big.mark=" "),"$"),
           m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^M/M^"),
           y_o_y =  paste0(format(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^A/A^"),
           indicators = "Salaire hebdomadaire moyen ^Travailleurs à temps plein (25 - 54 ans)^",
           color_mom = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN"))%>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(format(round(wages_canada_last_value[[1]],digits=0),big.mark=" ")," $","^au Canada^")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  
  
  # Inflation
  
  inflation_province <- get_cansim_vector(vector_inflation[i]) 
  tail =  tail(inflation_province$Date,n=1)
  
  
  inflation_province <- 
    inflation_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0(format(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),decimal.mark=",")," %","^A/A^"),
           m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^M/M^"),
           y_o_y = NA ,
           indicators = "Inflation ^Variations de l'Indice des prix à la consommation (IPC)^",
           color_mom = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "GREEN"),
           color_yoy = NA )%>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(format(round(inflation_canada_last_value[[1]],digits=1),decimal.mark=",")," %","^au Canada^")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  
  
  # New immigrants
  
  immigration_province <- get_cansim_vector(vector_immigration[i]) 
  tail =  tail(immigration_province$Date,n=1)
  
  
  immigration_province <- 
    immigration_province  %>% 
    mutate(Date_mod=format(Date, "%Y-%m"),
           value = paste0(format(round(val_norm/1000,digits=1),decimal.mark=",")," K"),
           m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^T/T^"),
           y_o_y = paste0(format(round((((val_norm / lag(val_norm, n=4))-1)*100),digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE)," %","^A/A^"),
           indicators = "Nouveaux immigrants ^Données trimestrielles^",
           color_mom = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
           color_yoy = case_when(
             round((((val_norm / lag(val_norm, n=4))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=4))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=4))-1)*100),digits=1) > 0  ~ "GREEN"))%>% 
    filter(Date==tail) %>% 
    mutate(cont_canada = paste0(format(round(val_norm/immigration_canada_last_value*100,digits=1),decimal.mark=",")," %")) %>% 
    select(indicators,value, Date_mod,cont_canada,m_o_m,color_mom,y_o_y,color_yoy)
  
  
  
  
  
  economy_line <- data.frame("Economie",NA,NA,NA,NA,NA,NA,NA)   
  colnames(economy_line) <- c("indicators","value","Date_mod","cont_canada","m_o_m","color_mom","y_o_y","color_yoy")
  
  labormarket_line <- data.frame("Marché de l'emploi",NA,NA,NA,NA,NA,NA,NA) 
  colnames(labormarket_line) <- c("indicators","value","Date_mod","cont_canada","m_o_m","color_mom","y_o_y","color_yoy")
  
  
  province_table <- bind_rows(economy_line,
                              gdp_province,
                              manuf_province,
                              export_province,
                              retail_province,
                              business_province,
                              labormarket_line,
                              job_province,
                              unemployment_province,
                              employment_province,
                              vacancy_province,
                              wages_province,
                              inflation_province,
                              immigration_province
  )
  
  colnames(province_table) <- c("Indicateurs","Valeur","Date","Contribution au Canada","Variations récentes","ColorMoM","","ColorYoY") 
  
  
  # province_table[6,2] <- as.numeric(province_table[6,2])
  # 
  
  file_name = paste0("data/",cansim_gdp_province[i],"_table.csv")
  write.csv(x = province_table, file=file_name)
  
  
  
}








# ----------------------- CANADA TABLE ---------------------


# --------- ECONOMY  ----------------

# GDP

gdp_canada <- get_cansim_vector("v65201210") 
tail =  tail(gdp_canada$Date,n=1)

gdp_canada_final <- 
  gdp_canada %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0("$",round(val_norm/1000000000,digits=1),"^Billion^"),
         m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^M/M^"),
         y_o_y = paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),"%","^Y/Y^") ,
         indicators = "Real gross domestic product (GDP) ^Annual adjusted for inflation^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
         color_yoy =  case_when(
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

fr_gdp_canada <- 
  gdp_canada %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(format(round(val_norm/1000000000,digits=1),big.mark=" ",decimal.mark=",", scientific=FALSE),"^Milliards de dollars^"),
         m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),decimal.mark=",")," %","^M/M^"),
         y_o_y = paste0(format(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),decimal.mark=",")," %","^A/A^") ,
         indicators = "Produit intérieur brut (PIB) réel ^Sur une base annuelle et ajustée pour l'inflation^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
         color_yoy =  case_when(
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)



# Manufacturing 


tail =  tail(manuf_canada$Date,n=1)

manuf_canada_final <- 
  manuf_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0("$",round(val_norm/1000000000,digits=1),"^Billion^"),
         m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^M/M^"),
         y_o_y = paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),"%","^Y/Y^"),
         indicators = "Manufacturing sales ^Monthly^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

fr_manuf_canada <- 
  manuf_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(format(round(val_norm/1000000000,digits=1),decimal.mark=","),"^Milliards de dollars^"),
         m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),decimal.mark=",")," %","^M/M^"),
         y_o_y = paste0(format(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),decimal.mark=",")," %","^A/A^"),
         indicators = "Ventes manufacturières ^Données mensuelles^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)


# Export 

tail =  tail(export_canada$Date,n=1)

export_canada_final <- 
  export_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0("$",round(val_norm/1000000000,digits=1),"^Billion^"),
         m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^M/M^"),
         y_o_y = paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),"%","^Y/Y^"),
         indicators = "Exports ^Monthly^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

fr_export_canada <- 
  export_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(format(round(val_norm/1000000000,digits=1),decimal.mark=","),"^Milliards de dollars^"),
         m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),decimal.mark=",")," %","^M/M^"),
         y_o_y = paste0(format(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),decimal.mark=",")," %","^A/A^"),
         indicators = "Exportations^Données mensuelles^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

# Retail trade 

tail =  tail(retail_canada$Date,n=1)


retail_canada_final <- 
  retail_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0("$",round(val_norm/1000000000,digits=1),"^Billion^"),
         m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^M/M^"),
         y_o_y =  paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),"%","^Y/Y^"),
         indicators = "Retail sales ^monthly^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

fr_retail_canada <- 
  retail_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(format(round(val_norm/1000000000,digits=1),decimal.mark=","),"^Milliards de dollars^"),
         m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),decimal.mark=",")," %","^M/M^"),
         y_o_y =  paste0(format(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),decimal.mark=",")," %","^A/A^"),
         indicators = "Ventes au détail ^Données mensuelles^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)


#Business 

tail =  tail(business_canada$Date,n=1)


business_canada_final <- 
  business_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(round(val_norm/1000,digits=0)," K"),
         m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^M/M^"),
         y_o_y =  paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),"%","^Y/Y^"),
         indicators = "Active businesses^Total number^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN"))  %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

fr_business_canada <- 
  business_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(format(round(val_norm/1000,digits=0),big.mark=" ", decimal.mark=",", scientific=FALSE)," K"),
         m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),decimal.mark=",")," %","^M/M^"),
         y_o_y =  paste0(format(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),decimal.mark=",")," %","^A/A^"),
         indicators = "Entreprises actives",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN"))  %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)


# --------- LABOR MARKET ----------------

# Total jobs 
 
tail =  tail(jobs_canada$Date,n=1)


jobs_canada_final <- 
  jobs_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(round(val_norm/1000000,digits=1),"^Million jobs^"),
         m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^M/M^"),
         y_o_y =  paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),"%","^Y/Y^"),
         indicators = "Total employed ^Population with a job^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

fr_jobs_canada <- 
  jobs_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(format(round(val_norm/1000000,digits=1),decimal.mark=","),"^Millions d'emplois^"),
         m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),decimal.mark=",")," %","^M/M^"),
         y_o_y =  paste0(format(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),decimal.mark=",")," %","^A/A^"),
         indicators = "Emplois totaux ^Population avec un emploi^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)


# Unemployment rate 
 
tail =  tail(unemployment_canada$Date,n=1)

unemployment_canada_final <- 
  unemployment_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(round(val_norm,digits=1),"%"),
         m_o_m = paste0(round((((val_norm - lag(val_norm, n=1)))),digits=1)," p.p.","^M/M^"),
         y_o_y =  paste0(round((((val_norm - lag(val_norm, n=12)))),digits=1)," p.p.","^Y/Y^"),
         indicators = "Unemployment rate ^% of active workforce unemployed^",
         color_mom = case_when(
           round((((val_norm - lag(val_norm, n=1)))),digits=1) > 0  ~ "RED", 
           round((((val_norm - lag(val_norm, n=1)))),digits=1) == 0  ~ "YELLOW",
           round((((val_norm - lag(val_norm, n=1)))),digits=1) < 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm - lag(val_norm, n=12)))),digits=1) > 0  ~ "RED", 
           round((((val_norm - lag(val_norm, n=12)))),digits=1) == 0  ~ "YELLOW",
           round((((val_norm - lag(val_norm, n=12)))),digits=1) < 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)


fr_unemployment_canada <- 
  unemployment_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(format(round(val_norm,digits=1),decimal.mark=",")," %"),
         m_o_m = paste0(format(round((((val_norm - lag(val_norm, n=1)))),digits=1),decimal.mark=",")," p.p.","^M/M^"),
         y_o_y =  paste0(format(round((((val_norm - lag(val_norm, n=12)))),digits=1),decimal.mark=",")," p.p.","^A/A^"),
         indicators = "Taux de chômage ^% de la population active sans emploi^",
         color_mom = case_when(
           round((((val_norm - lag(val_norm, n=1)))),digits=1) > 0  ~ "RED", 
           round((((val_norm - lag(val_norm, n=1)))),digits=1) == 0  ~ "YELLOW",
           round((((val_norm - lag(val_norm, n=1)))),digits=1) < 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm - lag(val_norm, n=12)))),digits=1) > 0  ~ "RED", 
           round((((val_norm - lag(val_norm, n=12)))),digits=1) == 0  ~ "YELLOW",
           round((((val_norm - lag(val_norm, n=12)))),digits=1) < 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)


# Employment rate 

tail =  tail(employment_canada$Date,n=1)

employment_canada_final <- 
  employment_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(round(val_norm,digits=1),"%"),
         m_o_m = paste0(round((((val_norm - lag(val_norm, n=1)))),digits=1)," p.p.","^M/M^"),
         y_o_y =  paste0(round((((val_norm - lag(val_norm, n=12)))),digits=1)," p.p.","^Y/Y^"),
         indicators = "Employment rate ^% of total population holding a job (25 - 54 years old)^",
         color_mom = case_when(
           round((((val_norm - lag(val_norm, n=1)))),digits=1) < 0  ~ "RED", 
           round((((val_norm - lag(val_norm, n=1)))),digits=1) == 0  ~ "YELLOW",
           round((((val_norm - lag(val_norm, n=1)))),digits=1) > 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm - lag(val_norm, n=12)))),digits=1) < 0  ~ "RED", 
           round((((val_norm - lag(val_norm, n=12)))),digits=1) == 0  ~ "YELLOW",
           round((((val_norm - lag(val_norm, n=12)))),digits=1) > 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

fr_employment_canada <- 
  employment_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(format(round(val_norm,digits=1),decimal.mark=",")," %"),
         m_o_m = paste0(format(round((((val_norm - lag(val_norm, n=1)))),digits=1),decimal.mark=",")," p.p.","^M/M^"),
         y_o_y =  paste0(format(round((((val_norm - lag(val_norm, n=12)))),digits=1),decimal.mark=",")," p.p.","^A/A^"),
         indicators = "Taux d'emploi ^% de la population avec un emploi (25 - 54 ans)^",
         color_mom = case_when(
           round((((val_norm - lag(val_norm, n=1)))),digits=1) < 0  ~ "RED", 
           round((((val_norm - lag(val_norm, n=1)))),digits=1) == 0  ~ "YELLOW",
           round((((val_norm - lag(val_norm, n=1)))),digits=1) > 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm - lag(val_norm, n=12)))),digits=1) < 0  ~ "RED", 
           round((((val_norm - lag(val_norm, n=12)))),digits=1) == 0  ~ "YELLOW",
           round((((val_norm - lag(val_norm, n=12)))),digits=1) > 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)


# Job vacancies 

vacancy_rate_canada <- get_cansim_vector(vector_vacancy_rate[1]) 
tail =  tail(vacancy_rate_canada$Date,n=1)

vacancy_rate_canada <- 
  vacancy_rate_canada %>% 
  filter(Date==tail) %>% 
  select(val_norm)

tail =  tail(vacancy_canada$Date,n=1)


vacancy_canada_final <- 
  vacancy_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(round(val_norm/1000,digits=0)," K","^",vacancy_rate_canada,"% of payroll employees^"),
         m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1)," %","^M/M^"),
         y_o_y =  paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),"%","^Y/Y^"),
         indicators = "Job vacancies ^Number of unfilled positions^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "GREEN"))%>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

fr_vacancy_canada <- 
  vacancy_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(format(round(val_norm/1000,digits=0),decimal.mark=",")," K","^",format(vacancy_rate_canada[[1]],decimal.mark=",")," % des employés^"),
         m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),decimal.mark=",")," %","^M/M^"),
         y_o_y =  paste0(format(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),decimal.mark=",")," %","^A/A^"),
         indicators = "Postes vacants ^Nombre de postes non-comblés^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "GREEN"))%>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)
# Weekly earnings 

tail =  tail(wages_canada$Date,n=1)

wages_canada_final <- 
  wages_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0("$",round(val_norm,digits=0)),
         m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^M/M^"),
         y_o_y =  paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),"%","^Y/Y^"),
         indicators = "Average weekly earnings ^Full-time workers (25 - 54 years old)^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN"))%>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

fr_wages_canada <- 
  wages_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(format(round(val_norm,digits=0),big.mark=" ", decimal.mark=",",scientific=FALSE),"$"),
         m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),decimal.mark=",")," %","^M/M^"),
         y_o_y =  paste0(format(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),decimal.mark=",")," %","^A/A^"),
         indicators = "Salaire hebdomadaire moyen ^Travailleurs à temps plein (25 à 54 ans)^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1) > 0  ~ "GREEN"))%>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)


# --------- FINANCE  ----------------


# Household debt

tail =  tail(household_gvm_debt_gdp$Date,n=1)


household_debt <- 
  household_gvm_debt_gdp  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(round(Household,digits=1),"%"),
         m_o_m = paste0(round((((Household - lag(Household, n=1)))),digits=1)," p.p.","^Q/Q^"),
         y_o_y =  paste0(round((((Household - lag(Household, n=4)))),digits=1)," p.p.","^Y/Y^"),
         indicators = "Household debt ^Weight on the economy^",
         color_mom = case_when(
           round((((Household - lag(Household, n=1)))),digits=1) > 0  ~ "RED", 
           round((((Household - lag(Household, n=1)))),digits=1) == 0  ~ "YELLOW",
           round((((Household - lag(Household, n=1)))),digits=1) < 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((Household - lag(Household, n=4)))),digits=1) > 0  ~ "RED", 
           round((((Household - lag(Household, n=4)))),digits=1) == 0  ~ "YELLOW",
           round((((Household - lag(Household, n=4)))),digits=1) < 0  ~ "GREEN"))%>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)


fr_household_debt <- 
  household_gvm_debt_gdp  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(format(round(Household,digits=1),decimal.mark=",")," %"),
         m_o_m = paste0(format(round((((Household - lag(Household, n=1)))),digits=1),decimal.mark=",")," p.p.","^T/T^"),
         y_o_y =  paste0(format(round((((Household - lag(Household, n=4)))),digits=1),decimal.mark=",")," p.p.","^A/A^"),
         indicators = "Dette des ménages ^Poids sur l'économie^",
         color_mom = case_when(
           round((((Household - lag(Household, n=1)))),digits=1) > 0  ~ "RED", 
           round((((Household - lag(Household, n=1)))),digits=1) == 0  ~ "YELLOW",
           round((((Household - lag(Household, n=1)))),digits=1) < 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((Household - lag(Household, n=4)))),digits=1) > 0  ~ "RED", 
           round((((Household - lag(Household, n=4)))),digits=1) == 0  ~ "YELLOW",
           round((((Household - lag(Household, n=4)))),digits=1) < 0  ~ "GREEN"))%>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)


# Government debt 

government_debt <- 
  household_gvm_debt_gdp  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(round(Government,digits=1),"%"),
         m_o_m = paste0(round((((Government - lag(Government, n=1)))),digits=1)," p.p.","^Q/Q^"),
         y_o_y =  paste0(round((((Government - lag(Government, n=4)))),digits=1)," p.p.","^Y/Y^"),
         indicators = "Government debt ^Weight on the economy^",
         color_mom = case_when(
           round((((Government - lag(Government, n=1)))),digits=1) > 0  ~ "RED", 
           round((((Government - lag(Government, n=1)))),digits=1) == 0  ~ "YELLOW",
           round((((Government - lag(Government, n=1)))),digits=1) < 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((Government - lag(Government, n=4)))),digits=1) > 0  ~ "RED", 
           round((((Government - lag(Government, n=4)))),digits=1) == 0  ~ "YELLOW",
           round((((Government - lag(Government, n=4)))),digits=1) < 0  ~ "GREEN"))%>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

fr_government_debt <- 
  household_gvm_debt_gdp  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(format(round(Government,digits=1),decimal.mark=",")," %"),
         m_o_m = paste0(format(round((((Government - lag(Government, n=1)))),digits=1),decimal.mark=",")," p.p.","^T/T^"),
         y_o_y =  paste0(format(round((((Government - lag(Government, n=4)))),digits=1),decimal.mark=",")," p.p.","^A/A^"),
         indicators = "Dette des gouvernements ^Poids sur l'économie^",
         color_mom = case_when(
           round((((Government - lag(Government, n=1)))),digits=1) > 0  ~ "RED", 
           round((((Government - lag(Government, n=1)))),digits=1) == 0  ~ "YELLOW",
           round((((Government - lag(Government, n=1)))),digits=1) < 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((Government - lag(Government, n=4)))),digits=1) > 0  ~ "RED", 
           round((((Government - lag(Government, n=4)))),digits=1) == 0  ~ "YELLOW",
           round((((Government - lag(Government, n=4)))),digits=1) < 0  ~ "GREEN"))%>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)


# INFLATION

tail =  tail(inflation_canada$Date,n=1)

inflation_canada_final <- 
  inflation_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),"%","^Y/Y^"),
         m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^M/M^"),
         y_o_y = NA ,
         indicators = "Inflation ^Consumer price index (CPI) variations^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "GREEN"),
         color_yoy = NA )%>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)


fr_inflation_canada <- 
  inflation_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         value = paste0(format(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1),decimal.mark=",")," %","^A/A^"),
         m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),decimal.mark=",")," %","^M/M^"),
         y_o_y = NA ,
         indicators = "Inflation ^Variations de l'Indice des prix à la consommation (IPC)^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "GREEN"),
         color_yoy = NA )%>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)



# -------------------- Demography and social --------------------


# Immigration


population_canada <- get_cansim_vector("v1") 
tail =  tail(population_canada$Date,n=1)

population_canada_last_value <- 
  population_canada  %>% 
  filter(Date==tail) %>% 
  select(val_norm)


tail =  tail(immigration_canada$Date,n=1)

immigration_canada_final <- 
  immigration_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),"%","^Q/Q^"),
         y_o_y = paste0(round((((val_norm / lag(val_norm, n=4))-1)*100),digits=1),"%","^Y/Y^"),
         indicators = "New immigrants ^Quarterly^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm / lag(val_norm, n=4))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=4))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=4))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  mutate( value = paste0(round(val_norm/1000,digits=1)," K","^Equivalent to ",round((val_norm*4)/population_canada_last_value*100,digits=1),"% of population annually^")) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

fr_immigration_canada <- 
  immigration_canada  %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         m_o_m = paste0(format(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1),decimal.mark=",")," %","^T/T^"),
         y_o_y = paste0(format(round((((val_norm / lag(val_norm, n=4))-1)*100),digits=1),decimal.mark=",")," %","^A/A^"),
         indicators = "Nouveaux immigrants ^Donnéees trimestrielles^",
         color_mom = case_when(
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((val_norm / lag(val_norm, n=4))-1)*100),digits=1) < 0  ~ "RED", 
           round((((val_norm / lag(val_norm, n=4))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((val_norm / lag(val_norm, n=4))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  mutate( value = paste0(format(round(val_norm/1000,digits=1),decimal.mark=",")," K","^Equivalent à ",format(round((val_norm*4)/population_canada_last_value*100,digits=1),decimal.mark=",")," % de la population annuellement^")) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)


# Immigration - employment gap

employment_rate_immigrant_female <- get_cansim_vector("v53039574") %>% 
  select(Date,val_norm) %>% 
  rename(employment_rate_immigrant_female = val_norm)

employment_rate_born_canada_female <- get_cansim_vector("v53040054") %>% 
  select(Date,val_norm) %>% 
  rename(employment_rate_born_canada_female = val_norm)

employment_gap_immigration <- inner_join(x=employment_rate_born_canada_female,y=employment_rate_immigrant_female, by = "Date")

tail =  tail(employment_gap_immigration$Date,n=1)


employment_gap_immigration_final <- 
  employment_gap_immigration %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         difference = employment_rate_born_canada_female - employment_rate_immigrant_female, 
         value = paste0(round(difference,digits=1)," p.p."),
         m_o_m = paste0(round((((difference - lag(difference, n=1)))),digits=1)," p.p.","^M/M^"),
         y_o_y = paste0(round((((difference - lag(difference, n=12)))),digits=1)," p.p.","^Y/Y^"),
         indicators = "Immigrant employment gap ^Difference in employment rate between immigrant women and Canadian-Born (25-54)^",
         color_mom = case_when(
           round((((difference - lag(difference, n=1)))),digits=1) > 0  ~ "RED", 
           round((((difference - lag(difference, n=1)))),digits=1) == 0  ~ "YELLOW",
           round((((difference - lag(difference, n=1)))),digits=1) < 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((difference - lag(difference, n=12)))),digits=1) > 0  ~ "RED", 
           round((((difference - lag(difference, n=12)))),digits=1) == 0  ~ "YELLOW",
           round((((difference - lag(difference, n=12)))),digits=1) < 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

fr_employment_gap_immigration <- 
  employment_gap_immigration %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         difference = employment_rate_born_canada_female - employment_rate_immigrant_female, 
         value = paste0(format(round(difference,digits=1),digital.mark=",")," p.p."),
         m_o_m = paste0(format(round((((difference - lag(difference, n=1)))),digits=1),digital.mark=",")," p.p.","^M/M^"),
         y_o_y = paste0(format(round((((difference - lag(difference, n=12)))),digits=1),digital.mark=",")," p.p.","^A/A^"),
         indicators = "Écart de l'emploi pour les immigrantes ^Differences entre le taux d'emploi des femmes immigrantes et de celles nées au Canada (25-54)^",
         color_mom = case_when(
           round((((difference - lag(difference, n=1)))),digits=1) > 0  ~ "RED", 
           round((((difference - lag(difference, n=1)))),digits=1) == 0  ~ "YELLOW",
           round((((difference - lag(difference, n=1)))),digits=1) < 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((difference - lag(difference, n=12)))),digits=1) > 0  ~ "RED", 
           round((((difference - lag(difference, n=12)))),digits=1) == 0  ~ "YELLOW",
           round((((difference - lag(difference, n=12)))),digits=1) < 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)
    

# Indigenous employment gap 


employment_rate_indigenous <- get_cansim_vector("v1411943325") %>% 
  select(Date,val_norm) %>% 
  rename(employment_rate_indigenous = val_norm)

employment_rate_non_indigenous <- get_cansim_vector("v1411946205") %>% 
  select(Date,val_norm) %>% 
  rename(employment_rate_non_indigenous = val_norm)

employment_gap_indigenous <- inner_join(x=employment_rate_non_indigenous,y=employment_rate_indigenous, by = "Date")

tail =  tail(employment_gap_indigenous$Date,n=1)

employment_gap_indigenous_final <- 
  employment_gap_indigenous %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         difference = employment_rate_non_indigenous - employment_rate_indigenous, 
         value = paste0(round(difference,digits=1)," p.p."),
         m_o_m = paste0(round((((difference - lag(difference, n=1)))),digits=1)," p.p.","^M/M^"),
         y_o_y = paste0(round((((difference - lag(difference, n=12)))),digits=1)," p.p.","^Y/Y^"),
         indicators = "Indigenous employment gap ^Difference in employment rate between indigenous and non-indigenous (25-54)^",
         color_mom = case_when(
           round((((difference - lag(difference, n=1)))),digits=1) > 0  ~ "RED", 
           round((((difference - lag(difference, n=1)))),digits=1) == 0  ~ "YELLOW",
           round((((difference - lag(difference, n=1)))),digits=1) < 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((difference - lag(difference, n=12)))),digits=1) > 0  ~ "RED", 
           round((((difference - lag(difference, n=12)))),digits=1) == 0  ~ "YELLOW",
           round((((difference - lag(difference, n=12)))),digits=1) < 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

fr_employment_gap_indigenous <- 
  employment_gap_indigenous %>% 
  mutate(Date_mod=format(Date, "%Y-%m"),
         difference = employment_rate_non_indigenous - employment_rate_indigenous, 
         value = paste0(format(round(difference,digits=1),decimal.mark=",")," p.p."),
         m_o_m = paste0(format(round((((difference - lag(difference, n=1)))),digits=1),decimal.mark=",")," p.p.","^M/M^"),
         y_o_y = paste0(format(round((((difference - lag(difference, n=12)))),digits=1),decimal.mark=",")," p.p.","^A/A^"),
         indicators = "Écart de l'emploi pour la population autochtone ^Differences entre le taux d'emploi de la population autochtone et non autochtone (25-54)^",
         color_mom = case_when(
           round((((difference - lag(difference, n=1)))),digits=1) > 0  ~ "RED", 
           round((((difference - lag(difference, n=1)))),digits=1) == 0  ~ "YELLOW",
           round((((difference - lag(difference, n=1)))),digits=1) < 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((difference - lag(difference, n=12)))),digits=1) > 0  ~ "RED", 
           round((((difference - lag(difference, n=12)))),digits=1) == 0  ~ "YELLOW",
           round((((difference - lag(difference, n=12)))),digits=1) < 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)


# Wealth disparity 

vector_wealth <- c("v1277968976",
                   "v1277969009",
                   "v1277969042",
                   "v1277969075",
                   "v1277969108")

wealth_df <-  wealth_quintile <- 
  get_cansim_vector("v1277968976") %>% 
  select(Date,val_norm) %>% 
  drop_na() %>% 
  rename(wealth_1_quintile=val_norm)

for (i in 2:length(vector_wealth)) {
  
  new_names = c("Date", paste0("wealth_",i,"_quintile"))
  
  wealth_quintile <- 
    get_cansim_vector(vector_wealth[i]) %>% 
    select(Date,val_norm) %>% 
    drop_na()

colnames(wealth_quintile) <- new_names
  
 wealth_df <- merge(wealth_df,wealth_quintile,by="Date")

}


wealth_df <- 
  wealth_df %>% 
    mutate(wealth_bottom_60 = wealth_1_quintile+wealth_2_quintile+wealth_3_quintile)

tail =  tail(wealth_df$Date,n=1)

wealth_df_final <- 
  wealth_df %>% 
  mutate(Date_mod=format(Date, "%Y-%m"), 
         value = paste0(round(wealth_bottom_60,digits=1),"%","^of total wealth^"),
         m_o_m = paste0(round((((wealth_bottom_60 / lag(wealth_bottom_60, n=1))-1)*100),digits=1),"%","^Q/Q^"),
         y_o_y = paste0(round((((wealth_bottom_60 / lag(wealth_bottom_60, n=4))-1)*100),digits=1),"%","^Y/Y^"),
         indicators = "Wealth distribution ^Wealth of bottom 60%^",
         color_mom = case_when(
           round((((wealth_bottom_60 / lag(wealth_bottom_60, n=1))-1)*100),digits=1) < 0  ~ "RED", 
           round((((wealth_bottom_60 / lag(wealth_bottom_60, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((wealth_bottom_60 / lag(wealth_bottom_60, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((wealth_bottom_60 / lag(wealth_bottom_60, n=4))-1)*100),digits=1) < 0  ~ "RED", 
           round((((wealth_bottom_60 / lag(wealth_bottom_60, n=4))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((wealth_bottom_60 / lag(wealth_bottom_60, n=4))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

fr_wealth_df <- 
  wealth_df %>% 
  mutate(Date_mod=format(Date, "%Y-%m"), 
         value = paste0(format(round(wealth_bottom_60,digits=1),decimal.mark=",")," %","^de la richesse totale^"),
         m_o_m = paste0(format(round((((wealth_bottom_60 / lag(wealth_bottom_60, n=1))-1)*100),digits=1),decimal.mark=",")," %","^T/T^"),
         y_o_y = paste0(format(round((((wealth_bottom_60 / lag(wealth_bottom_60, n=4))-1)*100),digits=1),decimal.mark=",")," %","^A/A^"),
         indicators = "Distribution de la richesse ^Richesse des 60 % les moins riches^",
         color_mom = case_when(
           round((((wealth_bottom_60 / lag(wealth_bottom_60, n=1))-1)*100),digits=1) < 0  ~ "RED", 
           round((((wealth_bottom_60 / lag(wealth_bottom_60, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((wealth_bottom_60 / lag(wealth_bottom_60, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((wealth_bottom_60 / lag(wealth_bottom_60, n=4))-1)*100),digits=1) < 0  ~ "RED", 
           round((((wealth_bottom_60 / lag(wealth_bottom_60, n=4))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((wealth_bottom_60 / lag(wealth_bottom_60, n=4))-1)*100),digits=1) > 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)



files <- c("lfs/pub0423.csv","lfs/pub0323.csv","lfs/pub0422.csv") # Change for last month, last year

NSNE_Young_People <- data.frame()

for(i in files) {  
  
  Labor_Force <- read.csv(i) # CHANGE DATE
  
  Young_People <- Labor_Force %>% 
    drop_na(AGE_12) %>% 
    filter(AGE_12 == 1 | 
             AGE_12 == 2 | 
             AGE_12 == 3)
  
  Young_People_Weight <- Young_People %>% 
    summarise(Young_People_Weight=sum(FINALWT))
  
  
  Non_Student_Weight <- Young_People %>% 
    drop_na(SCHOOLN) %>% 
    filter(SCHOOLN=="1") %>% 
    summarise(Young_Non_Student_Weight=sum(FINALWT))
  
  Non_Student_Non_Employed_Weight <-  Young_People %>% 
    drop_na(SCHOOLN) %>% 
    filter(SCHOOLN=="1") %>%
    filter(LFSSTAT=="3" |
             LFSSTAT=="4") %>% 
    summarise(Young_Non_Student_Non_Employed_Weight=sum(FINALWT))   
  
  
  Young_Not_student_Not_employed <- cbind(unique(Labor_Force$SURVYEAR),unique(Labor_Force$SURVMNTH),Young_People_Weight,Non_Student_Weight,Non_Student_Non_Employed_Weight)
  
  NSNE_Young_People <- rbind(NSNE_Young_People,Young_Not_student_Not_employed)
  
} 

colnames(NSNE_Young_People)[1] <- "year"
colnames(NSNE_Young_People)[2] <- "month"

NSNE_Young_People <- 
  NSNE_Young_People %>% 
  mutate(nsne = (Young_Non_Student_Non_Employed_Weight/Young_People_Weight)*100, 
         Date=paste(year,"-",month) ) %>% 
  arrange(Date)

tail =  tail(NSNE_Young_People$Date,n=1)

NSNE_Young_People_final <- 
  NSNE_Young_People %>% 
  mutate(Date_mod=Date,
         value = paste0(round(nsne,digits=1),"%","^",format(round(Young_Non_Student_Non_Employed_Weight,digits = -3),big.mark = " "),"^"),
         m_o_m = paste0(round((((nsne / lag(nsne, n=1))-1)*100),digits=1),"%","^M/M^"),
         y_o_y = paste0(round((((nsne / lag(nsne, n=2))-1)*100),digits=1),"%","^Y/Y^"),
         indicators = "Unactive youth ^Not employed, not studying or training (15-29)^",
         color_mom = case_when(
           round((((nsne / lag(nsne, n=1))-1)*100),digits=1) >  0  ~ "RED", 
           round((((nsne / lag(nsne, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((nsne / lag(nsne, n=1))-1)*100),digits=1) < 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((nsne / lag(nsne, n=2))-1)*100),digits=1) >  0  ~ "RED", 
           round((((nsne / lag(nsne, n=2))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((nsne / lag(nsne, n=2))-1)*100),digits=1) < 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)


fr_NSNE_Young_People <- 
  NSNE_Young_People %>% 
  mutate(Date_mod=Date,
         value = paste0(format(round(nsne,digits=1),decimal.mark=","),"%","^",format(round(Young_Non_Student_Non_Employed_Weight,digits = -3),big.mark = " "),"^"),
         m_o_m = paste0(format(round((((nsne / lag(nsne, n=1))-1)*100),digits=1),decimal.mark=",")," %","^M/M^"),
         y_o_y = paste0(format(round((((nsne / lag(nsne, n=2))-1)*100),digits=1),decimal.mark=",")," %","^A/A^"),
         indicators = "Jeunes inactifs ^Ni en emploi, ni aux études, ni en formation (15-29)^",
         color_mom = case_when(
           round((((nsne / lag(nsne, n=1))-1)*100),digits=1) >  0  ~ "RED", 
           round((((nsne / lag(nsne, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((nsne / lag(nsne, n=1))-1)*100),digits=1) < 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((nsne / lag(nsne, n=2))-1)*100),digits=1) >  0  ~ "RED", 
           round((((nsne / lag(nsne, n=2))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((nsne / lag(nsne, n=2))-1)*100),digits=1) < 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

# WAGE GAP - WOMEN VS MEN


Wage_Difference_Loop <- data.frame()

for(i in files) {  
  
  Labor_Force <- read.csv(i) # CHANGE DATE
  
  Wages_People_25_54 <- Labor_Force %>% 
    drop_na(AGE_12,LFSSTAT,HRLYEARN) %>% 
    filter(AGE_12 == 3 | 
             AGE_12 == 4 | 
             AGE_12 == 5 | 
             AGE_12 == 6 |
             AGE_12 == 7 |
             AGE_12 == 8 ) %>% 
    filter(LFSSTAT == 1 | 
             LFSSTAT==2)
  
  Wage_Men_Industry <- Wages_People_25_54 %>% 
    drop_na(NAICS_21) %>% 
    filter(SEX==1) %>% 
    group_by(NAICS_21) %>% 
    summarise(Average_Wage = weighted.mean(HRLYEARN,FINALWT) )
  
  Weight_Men_Industry <- Wages_People_25_54 %>% 
    drop_na(NAICS_21) %>% 
    filter(SEX==1) %>% 
    group_by(NAICS_21) %>% 
    summarise(Weight = sum(FINALWT) )
  
  Men_Wage_Average <- merge(Wage_Men_Industry,Weight_Men_Industry, by="NAICS_21") 
  
  
  Wage_Women_Industry <- Wages_People_25_54 %>% 
    drop_na(NAICS_21) %>% 
    filter(SEX==2) %>% 
    group_by(NAICS_21) %>% 
    summarise(Average_Wage_Women = weighted.mean(HRLYEARN,FINALWT) )
  
  Weight_Women_Industry <- Wages_People_25_54 %>% 
    drop_na(NAICS_21) %>% 
    filter(SEX==2) %>% 
    group_by(NAICS_21) %>% 
    summarise(Weight_Women = sum(FINALWT) )
  
  Women_Wage_Average <- merge(Wage_Women_Industry,Weight_Women_Industry, by="NAICS_21") 
  
  Wage_Difference <- merge(Men_Wage_Average, Women_Wage_Average, by="NAICS_21") %>% 
    mutate(Wage_Difference_Industry=Average_Wage_Women - Average_Wage) %>% 
    summarise(Wage_Difference = weighted.mean(Wage_Difference_Industry,Weight_Women))
  
  Average_Wage_Women <-  Women_Wage_Average %>% 
    summarise(Average_Wage_Women = weighted.mean(Average_Wage_Women,Weight_Women))
  
  Final_Wage_Difference <-   cbind(unique(Labor_Force$SURVYEAR),unique(Labor_Force$SURVMNTH),Average_Wage_Women,Wage_Difference)
  
  
  Wage_Difference_Loop <- rbind( Wage_Difference_Loop , Final_Wage_Difference)
  
} 

colnames(Wage_Difference_Loop)[1] <- "year"
colnames(Wage_Difference_Loop)[2] <- "month"

wage_difference_women <- 
  Wage_Difference_Loop %>% 
    mutate(wage_difference_percent = -Wage_Difference / Average_Wage_Women * 100, 
           Date=paste(year,"-",month)) %>% 
  arrange(Date)


tail =  tail(wage_difference_women$Date,n=1)

wage_difference_women <- 
  wage_difference_women %>% 
  mutate(Date_mod=Date,
         value = paste0(round(wage_difference_percent,digits=1),"%"),
         m_o_m = paste0(round((((wage_difference_percent - lag(wage_difference_percent, n=1)))),digits=1)," p.p.","^M/M^"),
         y_o_y = paste0(round((((wage_difference_percent - lag(wage_difference_percent, n=2)))),digits=1)," p.p.","^Y/Y^"),
         indicators = "Women wage gap ^% of difference with men (adjusted by industry)^",
         color_mom = case_when(
           round((((wage_difference_percent - lag(wage_difference_percent, n=1)))),digits=1) > 0  ~ "RED", 
           round((((wage_difference_percent - lag(wage_difference_percent, n=1)))),digits=1) == 0  ~ "YELLOW",
           round((((wage_difference_percent - lag(wage_difference_percent, n=1)))),digits=1) < 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((wage_difference_percent - lag(wage_difference_percent, n=2)))),digits=1) > 0  ~ "RED", 
           round((((wage_difference_percent - lag(wage_difference_percent, n=2)))),digits=1) == 0  ~ "YELLOW",
           round((((wage_difference_percent - lag(wage_difference_percent, n=2)))),digits=1) < 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)


fr_wage_difference_women <- 
  Wage_Difference_Loop %>% 
  mutate(wage_difference_percent = -Wage_Difference / Average_Wage_Women * 100, 
         Date=paste(year,"-",month))%>% 
  mutate(Date_mod=Date,
         value = paste0(format(round(wage_difference_percent,digits=1),decimal.mark=",")," %"),
         m_o_m = paste0(format(round((((wage_difference_percent - lag(wage_difference_percent, n=1)))),digits=1),decimal.mark=",")," p.p.","^M/M^"),
         y_o_y = paste0(format(round((((wage_difference_percent - lag(wage_difference_percent, n=2)))),digits=1),decimal.mark=",")," p.p.","^A/A^"),
         indicators = "Écart salarial des femmes ^% de différence avec les hommes (ajusté par industrie)^",
         color_mom = case_when(
           round((((wage_difference_percent - lag(wage_difference_percent, n=1)))),digits=1) > 0  ~ "RED", 
           round((((wage_difference_percent - lag(wage_difference_percent, n=1)))),digits=1) == 0  ~ "YELLOW",
           round((((wage_difference_percent - lag(wage_difference_percent, n=1)))),digits=1) < 0  ~ "GREEN"),
         color_yoy = case_when(
           round((((wage_difference_percent - lag(wage_difference_percent, n=2)))),digits=1) > 0  ~ "RED", 
           round((((wage_difference_percent - lag(wage_difference_percent, n=2)))),digits=1) == 0  ~ "YELLOW",
           round((((wage_difference_percent - lag(wage_difference_percent, n=2)))),digits=1) < 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

# Impute lines by hand for environmental components

ghg_population <- data.frame("Greenhouse gas ^Intensity of emissions^","17.7 ^Tons of CO2 per capita^","2020",NA,NA,"-9.9 % ^Y/Y^","GREEN")
colnames(ghg_population) <- c("indicators","value","Date_mod","m_o_m","color_mom","y_o_y","color_yoy")


non_ghg_electricity <- data.frame("Non-GHG Electricity production","82.6% ^of total electricity^","2019",NA,NA,"2 p.p. ^Since 2016^","GREEN")
colnames(non_ghg_electricity) <- c("indicators","value","Date_mod","m_o_m","color_mom","y_o_y","color_yoy")


fr_ghg_population <- data.frame("Gas à effet de serre ^Intensité des émissions^","17,7 ^Tonnes de CO2 par capita^","2020",NA,NA,"-9,9 % ^A/A^","GREEN")
colnames(fr_ghg_population) <- c("indicators","value","Date_mod","m_o_m","color_mom","y_o_y","color_yoy")


fr_non_ghg_electricity <- data.frame("Électricité produite sans GES","82,6% ^de l'électricité totale^","2019",NA,NA,"2 p.p. ^Depuis 2016^","GREEN")
colnames(fr_non_ghg_electricity) <- c("indicators","value","Date_mod","m_o_m","color_mom","y_o_y","color_yoy")

# Energy demand


energy_use <- get_cansim_vector("v54272321") %>% 
  select(Date,val_norm) %>% 
  rename(energy_demand_tj = val_norm)


gdp_annual<- get_cansim_vector("v62787277") %>% 
  select(Date,val_norm) %>% 
  rename(gdp = val_norm)

energy_use_gdp <- merge(energy_use,gdp_annual,by="Date")

energy_use_gdp <- 
  energy_use_gdp %>% 
    mutate(energy_use_gdp=energy_demand_tj/(gdp/1000000))

tail =  tail(energy_use_gdp$Date,n=1)

energy_use_gdp_final <- 
  energy_use_gdp %>% 
  mutate(Date_mod=format(Date, "%Y"),
         value = paste0(round(energy_use_gdp,digits=1),"^Terajoules per million $ of real GDP^"),
         m_o_m = NA,
         y_o_y = paste0(round((((energy_use_gdp / lag(energy_use_gdp, n=1))-1)*100),digits=1),"%","^Y/Y^") ,
         indicators = "Energy use ^Intensity in the economy^",
         color_mom = NA,
         color_yoy = case_when(
           round((((energy_use_gdp / lag(energy_use_gdp, n=1))-1)*100),digits=1) >  0  ~ "RED", 
           round((((energy_use_gdp / lag(energy_use_gdp, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((energy_use_gdp / lag(energy_use_gdp, n=1))-1)*100),digits=1) < 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

fr_energy_use_gdp <- 
  energy_use_gdp %>% 
  mutate(Date_mod=format(Date, "%Y"),
         value = paste0(format(round(energy_use_gdp,digits=1),decimal.mark=","),"^Terajoules par million de $ de PIB réel^"),
         m_o_m = NA,
         y_o_y = paste0(format(round((((energy_use_gdp / lag(energy_use_gdp, n=1))-1)*100),digits=1),decimal_mark=",")," %","^A/A^") ,
         indicators = "Utilisation d'énergie ^Intensité dans l'économie^",
         color_mom = NA,
         color_yoy = case_when(
           round((((energy_use_gdp / lag(energy_use_gdp, n=1))-1)*100),digits=1) >  0  ~ "RED", 
           round((((energy_use_gdp / lag(energy_use_gdp, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
           round((((energy_use_gdp / lag(energy_use_gdp, n=1))-1)*100),digits=1) < 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)



# Hybrid and electric cars


vector_cars<- c("v1079014835",
                   "v1079014836",
                   "v1079014837") # Vectors of the battery, hybrid and plug-in hybrid vehicles respectively

cars_fuel <- get_cansim_vector("v1079014832") %>% 
  select(Date,val_norm) %>% 
  rename(all_fuel_type = val_norm)


for (i in 1:length(vector_cars)) {
  
  new_names = c("Date", paste0("fueltype_",i))
  
  cars_fuel_type <- 
    get_cansim_vector(vector_cars[i]) %>% 
    select(Date,val_norm) %>% 
    drop_na()
  
  colnames(cars_fuel_type) <- new_names
  
  cars_fuel <- merge(cars_fuel,cars_fuel_type,all=TRUE)
  
}


tail =  tail(cars_fuel$Date,n=1)

cars_fuel_final <- 
  cars_fuel %>% 
  mutate(hybrid_electric = (fueltype_1+fueltype_2+fueltype_3)/all_fuel_type*100) %>% 
  mutate(Date_mod=format(Date, "%Y"),
         value = paste0(round(hybrid_electric,digits=1),"%","^of new registrations^"),
         m_o_m = NA,
         y_o_y = paste0(round(hybrid_electric - lag(hybrid_electric, n=1),digits=1)," p.p.","^Y/Y^") ,
         indicators = "Hybrid and electric vehicles",
         color_mom = NA,
         color_yoy =  case_when(
           round(hybrid_electric - lag(hybrid_electric, n=1),digits=1) < 0  ~ "RED", 
           round(hybrid_electric - lag(hybrid_electric, n=1),digits=1) == 0  ~ "YELLOW",
           round(hybrid_electric - lag(hybrid_electric, n=1),digits=1) > 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)

fr_cars_fuel <- 
  cars_fuel %>% 
  mutate(hybrid_electric = (fueltype_1+fueltype_2+fueltype_3)/all_fuel_type*100) %>% 
  mutate(Date_mod=format(Date, "%Y"),
         value = paste0(format(round(hybrid_electric,digits=1),decimal.mark=","),"%","^des nouvelles immatriculations^"),
         m_o_m = NA,
         y_o_y = paste0(format(round(hybrid_electric - lag(hybrid_electric, n=1),digits=1),decimal.mark=",")," p.p.","^A/A^") ,
         indicators = "Véhicules hybrides et électriques",
         color_mom = NA,
         color_yoy =  case_when(
           round(hybrid_electric - lag(hybrid_electric, n=1),digits=1) < 0  ~ "RED", 
           round(hybrid_electric - lag(hybrid_electric, n=1),digits=1) == 0  ~ "YELLOW",
           round(hybrid_electric - lag(hybrid_electric, n=1),digits=1) > 0  ~ "GREEN")) %>% 
  filter(Date==tail) %>% 
  select(indicators,value, Date_mod,m_o_m,color_mom,y_o_y,color_yoy)




economy_line <- data.frame("Economy",NA,NA,NA,NA,NA,NA)   
colnames(economy_line) <- c("indicators","value","Date_mod","m_o_m","color_mom","y_o_y","color_yoy")

labor_line <- data.frame("Labour market",NA,NA,NA,NA,NA,NA)   
colnames(labor_line) <- c("indicators","value","Date_mod","m_o_m","color_mom","y_o_y","color_yoy")


finance_line <- data.frame("Finance",NA,NA,NA,NA,NA,NA)   
colnames(finance_line) <- c("indicators","value","Date_mod","m_o_m","color_mom","y_o_y","color_yoy")

demography_social_line <- data.frame("Demography and social",NA,NA,NA,NA,NA,NA)   
colnames(demography_social_line) <- c("indicators","value","Date_mod","m_o_m","color_mom","y_o_y","color_yoy")


sustainability_line <- data.frame("Sustainability",NA,NA,NA,NA,NA,NA)   
colnames(sustainability_line) <- c("indicators","value","Date_mod","m_o_m","color_mom","y_o_y","color_yoy")



fr_economy_line <- data.frame("Economie",NA,NA,NA,NA,NA,NA)   
colnames(fr_economy_line) <- c("indicators","value","Date_mod","m_o_m","color_mom","y_o_y","color_yoy")

fr_labor_line <- data.frame("Marché de l'emploi",NA,NA,NA,NA,NA,NA)   
colnames(fr_labor_line) <- c("indicators","value","Date_mod","m_o_m","color_mom","y_o_y","color_yoy")


fr_finance_line <- data.frame("Finance",NA,NA,NA,NA,NA,NA)   
colnames(fr_finance_line) <- c("indicators","value","Date_mod","m_o_m","color_mom","y_o_y","color_yoy")

fr_demography_social_line <- data.frame("Demographie et société",NA,NA,NA,NA,NA,NA)   
colnames(fr_demography_social_line) <- c("indicators","value","Date_mod","m_o_m","color_mom","y_o_y","color_yoy")


fr_sustainability_line <- data.frame("Environnement",NA,NA,NA,NA,NA,NA)   
colnames(fr_sustainability_line) <- c("indicators","value","Date_mod","m_o_m","color_mom","y_o_y","color_yoy")


canada_table <- bind_rows(economy_line,
                          gdp_canada_final,
                          manuf_canada_final,
                          export_canada_final,
                          retail_canada_final,
                          business_canada_final,
                          labor_line,
                          jobs_canada_final,
                          unemployment_canada_final,
                          employment_canada_final,
                          vacancy_canada_final,
                          wages_canada_final,
                          finance_line,
                          household_debt,
                          government_debt,
                          inflation_canada_final,
                          demography_social_line,
                          immigration_canada_final,
                          employment_gap_immigration_final,
                          employment_gap_indigenous_final,
                          wealth_df_final,
                          NSNE_Young_People_final,
                          wage_difference_women,
                          sustainability_line,
                          ghg_population,
                          non_ghg_electricity,
                          energy_use_gdp_final,
                          cars_fuel_final
)

colnames(canada_table) <- c("Indicators","Value","Date","Recent variations","ColorMoM","","ColorYoY") 

write.csv(x = canada_table, file="data/canada_table.csv")




fr_canada_table <- bind_rows(fr_economy_line,
                          fr_gdp_canada,
                          fr_manuf_canada,
                          fr_export_canada,
                          fr_retail_canada,
                          fr_business_canada,
                          fr_labor_line,
                          fr_jobs_canada,
                          fr_unemployment_canada,
                          fr_employment_canada,
                          fr_vacancy_canada,
                          fr_wages_canada,
                          fr_finance_line,
                          fr_household_debt,
                          fr_government_debt,
                          fr_inflation_canada,
                          fr_demography_social_line,
                          fr_immigration_canada,
                          fr_employment_gap_immigration,
                          fr_employment_gap_indigenous,
                          fr_wealth_df,
                          fr_NSNE_Young_People,
                          fr_wage_difference_women,
                          fr_sustainability_line,
                          fr_ghg_population,
                          fr_non_ghg_electricity,
                          fr_energy_use_gdp,
                          fr_cars_fuel)

colnames(fr_canada_table) <- c("Indicateurs","Valeur","Dernières données","Variations récentes","ColorMoM","","ColorYoY") 

write.csv(x = fr_canada_table, file="data/fr_canada_table.csv")




