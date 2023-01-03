library(tidyverse)
library(cansim)


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
           value = paste0(format(round(val_norm/1000000000,digits=1),big.mark=" ", decimal.mark=",",scientific=FALSE),"^Milliards de $^"),
           m_o_m = NA,
           color_mom = NA,
           y_o_y = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1)," %","^A/A^"),
           color_yoy = case_when(
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) < 0  ~ "RED", 
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) == 0  ~ "YELLOW",
             round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1) > 0  ~ "GREEN"),
           indicators = "Produit intérieur brut (PIB) réel
^Sur une base annuelle ajustée pour l'inflation^")%>% 
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
           m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1)," %","^M/M^"),
           y_o_y = paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1)," %","^A/A^"),
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
           m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1)," %","^M/M^"),
           y_o_y = paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1)," %","^A/A^"),
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
           m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1)," %","^M/M^"),
           y_o_y =  paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1)," %","^A/A^"),
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
           m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1)," %","^M/M^"),
           y_o_y =  paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1)," %","^A/A^"),
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
           m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1)," %","^M/M^"),
           y_o_y =  paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1)," %","^A/A^"),
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
           m_o_m = paste0(round((((val_norm - lag(val_norm, n=1)))),digits=1)," p.p.","^M/M^"),
           y_o_y =  paste0(round((((val_norm - lag(val_norm, n=12)))),digits=1)," p.p.","^A/A^"),
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
           m_o_m = paste0(round((((val_norm - lag(val_norm, n=1)))),digits=1)," p.p.","^M/M^"),
           y_o_y =  paste0(round((((val_norm - lag(val_norm, n=12)))),digits=1)," p.p.","^A/A^"),
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
           m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1)," %","^M/M^"),
           y_o_y =  paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1)," %","^A/A^"),
           indicators = "Postes vacants ^Nombre de postes ouverts non-comblés^",
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
           m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1)," %","^M/M^"),
           y_o_y =  paste0(round((((val_norm / lag(val_norm, n=12))-1)*100),digits=1)," %","^A/A^"),
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
           m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1)," %","^M/M^"),
           y_o_y = NA ,
           indicators = "Inflation ^Variations de l'Ince des prix à la consommation (IPC)^",
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
           m_o_m = paste0(round((((val_norm / lag(val_norm, n=1))-1)*100),digits=1)," %","^T/T^"),
           y_o_y = paste0(round((((val_norm / lag(val_norm, n=4))-1)*100),digits=1)," %","^A/A^"),
           indicators = "Nouveaux immigrants ^Quarterly^",
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





