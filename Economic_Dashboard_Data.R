library(tidyverse)
library(cansim)
#library(xlsx)
library(writexl)
library(lubridate)
library(zoo)
library(fredr)
library(readxl)

library(openxlsx)
library(httr)
library(jsonlite)
library(data.table)
library(seasonal)
library(seasonalview)

library(lubridate)

#setwd("C:/Users/dabrassard/Desktop/Working folder/Ecnomic_Dashboard/Data")

setwd("C:/Users/LNB/Desktop/Dossier_DAB/Projets R/Economic_Dashboard")

fredr_set_key("529a8fe75d1703e7b03fbbb9898730b2")



#||||||||||||------------------ ECONOMY SECTION----------------------||||||

#Testing 1-2 

#its a new day 

GDP_Monthly_Industry <-get_cansim("36-10-0434-01")

names(GDP_Monthly_Industry)<-str_replace_all(names(GDP_Monthly_Industry),
                                                     c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

GDP_Monthly_History <- GDP_Monthly_Industry %>% 
                                filter(Seasonal_adjustment=="Seasonally adjusted at annual rates",
                                       North_American_Industry_Classification_System__NAICS_=="All industries [T001]",
                                       Prices=="Chained (2012) dollars") %>% 
                                select(Date,val_norm) %>% 
                            rename(Real_GDP=val_norm) 

GDP_Monthly_Last_Date <- tail(GDP_Monthly_History$Date,n=1)
GDP_Monthly_Last_Month <- GDP_Monthly_Last_Date-months(1) 
GDP_Monthly_Last_Year <-  GDP_Monthly_Last_Date-months(12)
Pandemic <- GDP_Monthly_Last_Date-months(28)


GDP_Monthly_Variations <- GDP_Monthly_History%>% 
                          filter(Date==GDP_Monthly_Last_Date | 
                                   Date==GDP_Monthly_Last_Month | 
                                   Date==GDP_Monthly_Last_Year)


# GDP Additional Page - Industry


GDP_Monthly_NAIC2 <- GDP_Monthly_Industry %>% 
                      filter(North_American_Industry_Classification_System__NAICS_ == "All industries [T001]" | North_American_Industry_Classification_System__NAICS_ == "Agriculture, forestry, fishing and hunting [11]"|
    North_American_Industry_Classification_System__NAICS_ == "Mining, quarrying, and oil and gas extraction [21]"|
    North_American_Industry_Classification_System__NAICS_ == "Utilities [22]"|
    North_American_Industry_Classification_System__NAICS_ == "Construction [23]"|
    North_American_Industry_Classification_System__NAICS_ == "Manufacturing [31-33]"|
    North_American_Industry_Classification_System__NAICS_ == "Wholesale trade [41]"|
    North_American_Industry_Classification_System__NAICS_ == "Retail trade [44-45]"|
    North_American_Industry_Classification_System__NAICS_ == "Transportation and warehousing [48-49]"|
    North_American_Industry_Classification_System__NAICS_ == "Information and cultural industries [51]"|
    North_American_Industry_Classification_System__NAICS_ == "Finance and insurance [52]"|
    North_American_Industry_Classification_System__NAICS_ == "Real estate and rental and leasing [53]"|
    North_American_Industry_Classification_System__NAICS_ == "Professional, scientific and technical services [54]" |
    North_American_Industry_Classification_System__NAICS_ == "Administrative and support, waste management and remediation services [56]"|
    North_American_Industry_Classification_System__NAICS_ == "Educational services [61]"|
    North_American_Industry_Classification_System__NAICS_ == "Health care and social assistance [62]"|
    North_American_Industry_Classification_System__NAICS_ == "Arts, entertainment and recreation [71]"|
    North_American_Industry_Classification_System__NAICS_ == "Accommodation and food services [72]"|
    North_American_Industry_Classification_System__NAICS_ == "Other services (except public administration) [81]"|
    North_American_Industry_Classification_System__NAICS_ == "Public administration [91]") %>% 
            filter(Seasonal_adjustment=="Seasonally adjusted at annual rates",
                   Prices=="Chained (2012) dollars") %>%  
        select(Date,North_American_Industry_Classification_System__NAICS_,val_norm) %>% 
         rename(Industry=North_American_Industry_Classification_System__NAICS_) %>% 
  arrange(Industry)


GDP_Monthly_NAIC2_Last_Date  <- GDP_Monthly_NAIC2 %>%  
                filter(Date==GDP_Monthly_Last_Date) %>% 
                select(Industry,val_norm) %>% 
                rename(Last_Date=val_norm)

GDP_Monthly_NAIC2_Last_Month <- GDP_Monthly_NAIC2 %>%  
  filter(Date==GDP_Monthly_Last_Month)  %>%  
  select(Industry,val_norm) %>% 
  rename(Last_Month=val_norm, Industry2=Industry)

GDP_Monthly_NAIC2_Last_Year <- GDP_Monthly_NAIC2 %>%  
  filter(Date==GDP_Monthly_Last_Year) %>%  
  select(Industry,val_norm) %>% 
  rename(Last_Year=val_norm,Industry3=Industry)

# Change date for pandemic

GDP_Monthly_NAIC2_Pandemic <- GDP_Monthly_NAIC2 %>%  
  filter(Date==Pandemic) %>%  
  select(Industry,val_norm) %>% 
  rename(Pandemic=val_norm,Industry4=Industry)

GDP_Monthly_NAIC2_Variations <- cbind(GDP_Monthly_NAIC2_Last_Date,
                                      GDP_Monthly_NAIC2_Last_Month,
                                      GDP_Monthly_NAIC2_Last_Year,
                                      GDP_Monthly_NAIC2_Pandemic)

GDP_Monthly_NAIC2_Variations <- GDP_Monthly_NAIC2_Variations %>% 
            select(Industry,Last_Date,Last_Month,Last_Year, Pandemic)



# Manufacturing sales

Manufacturing <-get_cansim("16-10-0047-01")

names(Manufacturing)<-str_replace_all(names(Manufacturing),
                                      c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))


Manufacturing_History <- Manufacturing %>% 
                          filter(North_American_Industry_Classification_System__NAICS_=="Manufacturing [31-33]",
                                 Seasonal_adjustment=="Seasonally adjusted",
                                 Principal_statistics=="Sales of goods manufactured (shipments)") %>% 
                          select(Date,val_norm) %>% 
                          rename(Manufacturing_sales=val_norm)

Manufacturing_Last_Date <- tail(Manufacturing$Date,n=1)
Manufacturing_Last_Month <- Manufacturing_Last_Date-months(1) 
Manufacturing_Last_Year <-  Manufacturing_Last_Date-months(12)


Manufacturing_Variations <- Manufacturing_History %>% 
                            filter(Date==Manufacturing_Last_Year | 
                                     Date==Manufacturing_Last_Month | 
                                     Date==Manufacturing_Last_Date )



# International_Trade

International_Trade <-get_cansim("12-10-0122-01")

names(International_Trade)<-str_replace_all(names(International_Trade ),
                                            c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

International_Trade_History <- International_Trade %>% 
                                filter(North_American_Product_Classification_System__NAPCS_=="Total of all merchandise",
                                       Seasonal_adjustment=="Seasonally adjusted",
                                       Basis=="Balance of payments",
                                       Trade=="Export") %>% 
                                select(Date,val_norm) %>% 
                                rename(Export=val_norm)


International_Trade_Last_Date <- tail(International_Trade$Date,n=1)
International_Trade_Last_Quarter <- International_Trade_Last_Date-months(3) 
International_Trade_Last_Year <-  International_Trade_Last_Date-months(12)


International_Trade_Variations <- International_Trade_History  %>% 
  filter(Date==International_Trade_Last_Year | 
           Date==International_Trade_Last_Quarter | 
           Date==International_Trade_Last_Date )




# Retail_Trade

Retail_Trade <-get_cansim("20-10-0008-01")

names(Retail_Trade)<-str_replace_all(names(Retail_Trade),
                                     c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))


Retail_Trade_History <- Retail_Trade %>% 
                        filter(GEO=="Canada",
                               Adjustments=="Seasonally adjusted",
                               North_American_Industry_Classification_System__NAICS_=="Retail trade [44-45]") %>% 
                        select(Date,val_norm) %>% 
                        rename(Retail_trade = val_norm)


Retail_Trade_Last_Date <- tail(Retail_Trade$Date,n=1)
Retail_Trade_Last_Month <- Retail_Trade_Last_Date-months(1) 
Retail_Trade_Last_Year <-  Retail_Trade_Last_Date-months(12)

Retail_Trade_Variations <- Retail_Trade_History %>% 
  filter(Date==Retail_Trade_Last_Year | 
           Date==Retail_Trade_Last_Month | 
           Date==Retail_Trade_Last_Date )


# Active_businesses


Active_Business <-get_cansim("33-10-0270-01")

names(Active_Business)<-str_replace_all(names(Active_Business),
                                     c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

Active_Business_History <- Active_Business %>%  
  filter(GEO=="Canada",
         Industry=="Business sector industries [T004]",
  Business_dynamics_measure=="Active businesses") %>% 
  select(Date,val_norm) %>% 
  rename(Active_Business = val_norm)



Active_Business_Last_Date <- tail(Active_Business$Date,n=1)
Active_Business_Last_Month <- Active_Business_Last_Date-months(1) 
Active_Business_Last_Year <-  Active_Business_Last_Date-months(12)

Active_Business_Variations <- Active_Business_History %>% 
  filter(Date==Active_Business_Last_Year | 
           Date==Active_Business_Last_Month | 
           Date==Active_Business_Last_Date )


# Nominal GDP 

Nominal_GDP <-get_cansim("36-10-0104-01")

names(Nominal_GDP)<-str_replace_all(names(Nominal_GDP),
                                        c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

Nominal_GDP_History <- Nominal_GDP %>%  
  filter(GEO=="Canada",
         Estimates=="Gross domestic product at market prices",
         Prices=="Current prices",
         Date>"2019-12-01",
         Seasonal_adjustment=="Seasonally adjusted at annual rates") %>% 
  select(Date,val_norm) %>% 
  rename(Nominal_GDP = val_norm)



# Investments ----------------- FUN TO HAVE

# Investments <-get_cansim("36-10-0108-01")
# 
# names(Investments)<-str_replace_all(names(Investments),
#                                         c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))
# 
# 
# Investments_History <- Investments %>%  
#   filter(GEO=="Canada",
#          Industry=="Business sector industries [T004]",
#          Business_dynamics_measure=="Active businesses") %>% 
#   select(Date,val_norm) %>% 
#   rename(Investments = val_norm)
# 
# 
# 
# Investments_Last_Date <- tail(Investments$Date,n=1)
# Investments_Last_Month <- Investments_Last_Date-months(1) 
# Investments_Last_Year <-  Investments_Last_Date-months(12)
# 
# Investments_Variations <- Investments_History %>% 
#   filter(Date==Investments_Last_Year | 
#            Date==Investments_Last_Month | 
#            Date==Investments_Last_Date )
# 
# 
# 



Economy_data <- createWorkbook()


addWorksheet(Economy_data,"GDP_Monthly_History")
addWorksheet(Economy_data,"GDP_Monthly_Variations")
addWorksheet(Economy_data,"GDP_Monthly_NAIC2_Variations")
addWorksheet(Economy_data,"Nominal_GDP_History")

addWorksheet(Economy_data,"Manufacturing_History")
addWorksheet(Economy_data,"Manufacturing_Variations")

addWorksheet(Economy_data,"International_Trade_History")
addWorksheet(Economy_data,"International_Trade_Variations")

addWorksheet(Economy_data,"Retail_Trade_History")
addWorksheet(Economy_data,"Retail_Trade_Variations")

addWorksheet(Economy_data,"Active_Business_History")
addWorksheet(Economy_data,"Active_Business_Variations")


writeData(Economy_data,sheet = "GDP_Monthly_History",x=GDP_Monthly_History)
writeData(Economy_data,sheet = "GDP_Monthly_Variations",x=GDP_Monthly_Variations)
writeData(Economy_data,sheet = "GDP_Monthly_NAIC2_Variations",x=GDP_Monthly_NAIC2_Variations)
writeData(Economy_data,sheet = "Nominal_GDP_History",x=Nominal_GDP_History)

writeData(Economy_data,sheet = "Manufacturing_History",x=Manufacturing_History)
writeData(Economy_data,sheet = "Manufacturing_Variations",x=Manufacturing_Variations)

writeData(Economy_data,sheet = "International_Trade_History",x=International_Trade_History)
writeData(Economy_data,sheet = "International_Trade_Variations",x=International_Trade_Variations)

writeData(Economy_data,sheet = "Retail_Trade_History",x=Retail_Trade_History)
writeData(Economy_data,sheet = "Retail_Trade_Variations",x=Retail_Trade_Variations)

writeData(Economy_data,sheet = "Active_Business_History",x=Active_Business_History)
writeData(Economy_data,sheet = "Active_Business_Variations",x=Active_Business_Variations)


saveWorkbook(Economy_data,"Economy_Dashboard_Data.xlsx",overwrite = TRUE)





# GDP Additional Page - Province ----> Rien d'intéressant

#GDP_Provinces_Annual <-get_cansim("36-10-0402-01")

#names(GDP_Provinces_Annual)<-str_replace_all(names(GDP_Provinces_Annual),
#                                             c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

#GDP_Provinces_Annual_Last_Date <- tail(GDP_Provinces_Annual$Date,n=1)
#GDP_Provinces_Annual_Last_Year <- GDP_Provinces_Annual_Last_Date-months(12)

#GDP_Provinces_Annual_Real <-GDP_Provinces_Annual  %>% 
#                      filter(Value=="Chained (2012) dollars", 
 #                            North_American_Industry_Classification_System__NAICS_=="All industries [T001]")

#GDP_Provinces_Annual_Real_Last_Date <- GDP_Provinces_Annual_Real %>% 
#                                          filter(Date==GDP_Provinces_Annual_Last_Date) %>% 
 #                                           select(GEO,val_norm)




# Province GDP annual table : 36-10-0402-01 


#||||||||||||------------------ LABOR MARKET ----------------------||||||



Labor_Market_Canada <-get_cansim("14-10-0287-03")

names(Labor_Market_Canada)<-str_replace_all(names(Labor_Market_Canada),
                                             c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

Labor_Market_Canada_Last_Date <- tail(Labor_Market_Canada$Date,n=1)
Labor_Market_Canada_Last_Month <- Labor_Market_Canada_Last_Date-months(1) 
Labor_Market_Canada_Last_Year <-  Labor_Market_Canada_Last_Date-months(12)
Labor_Market_Canada_Pandemic <- Labor_Market_Canada_Last_Date-months(30)


Labor_Market_Canada_Seasonnaly <-  Labor_Market_Canada %>% filter(Sex=="Both sexes",
                                                                  Age_group=="15 years and over",
                                                                  Data_type=="Seasonally adjusted",
                                                                  Statistics =="Estimate",
                                                                  Date>"1999-12-01",
                                                                  GEO=="Canada")
                
Total_Jobs_Canada <- Labor_Market_Canada_Seasonnaly  %>% 
                        filter(Labour_force_characteristics=="Employment") %>% 
                          select(Date,
                                 val_norm) %>% 
                          rename(Total_Jobs=val_norm)

Unemployment_Rate_Canada <- Labor_Market_Canada_Seasonnaly %>% 
                      filter(Labour_force_characteristics=="Unemployment rate") %>% 
                        select(Date,
                               val_norm) %>% 
                        rename(Unemployment_Rate=val_norm)

Employment_Rate_Canada_25_54 <-  Labor_Market_Canada %>% filter(Sex=="Both sexes",
                                                                Age_group=="25 to 54 years",
                                                                Data_type=="Seasonally adjusted",
                                                                Statistics =="Estimate",
                                                                Date>"1999-12-01",
                                                                GEO=="Canada",
                                                                Labour_force_characteristics=="Employment rate") %>% 
                                                select(Date,
                                                       val_norm) %>% 
                                                rename(Employment_Rate=val_norm)

  

Total_Jobs_Canada_Last_Date  <- Total_Jobs_Canada %>%  
  filter(Date==Labor_Market_Canada_Last_Date) %>% 
  rename(Last_Date=Total_Jobs) %>% 
        select(Last_Date)

Total_Jobs_Canada_Last_Month <- Total_Jobs_Canada %>%  
  filter(Date==Labor_Market_Canada_Last_Month) %>% 
  rename(Last_Month=Total_Jobs) %>% 
  select(Last_Month)


Total_Jobs_Canada_Last_Year  <- Total_Jobs_Canada %>%  
  filter(Date==Labor_Market_Canada_Last_Year) %>% 
  rename(Last_Year=Total_Jobs) %>% 
  select(Last_Year)


Total_Jobs_Canada_Pandemic <- Total_Jobs_Canada %>%  
  filter(Date==Labor_Market_Canada_Pandemic) %>% 
  rename(Pandemic =Total_Jobs) %>% 
  select(Pandemic )


Total_Jobs_Canada_Variations <- cbind(Total_Jobs_Canada_Last_Date,
                                      Total_Jobs_Canada_Last_Month,
                                      Total_Jobs_Canada_Last_Year ,
                                      Total_Jobs_Canada_Pandemic)

row.names(Total_Jobs_Canada_Variations)[1] <-  "Total_Jobs"


Unemployment_Rate_Canada_Last_Date  <- Unemployment_Rate_Canada %>%  
  filter(Date==Labor_Market_Canada_Last_Date) %>% 
  rename(Last_Date=Unemployment_Rate) %>% 
  select(Last_Date)

Unemployment_Rate_Canada_Last_Month <- Unemployment_Rate_Canada %>%  
  filter(Date==Labor_Market_Canada_Last_Month) %>% 
  rename(Last_Month=Unemployment_Rate) %>% 
  select(Last_Month)


Unemployment_Rate_Canada_Last_Year  <- Unemployment_Rate_Canada %>%  
  filter(Date==Labor_Market_Canada_Last_Year) %>% 
  rename(Last_Year=Unemployment_Rate) %>% 
  select(Last_Year)


Unemployment_Rate_Canada_Pandemic <- Unemployment_Rate_Canada %>%  
  filter(Date==Labor_Market_Canada_Pandemic) %>% 
  rename(Pandemic =Unemployment_Rate) %>% 
  select(Pandemic )


Unemployment_Rate_Canada_Variations <- cbind(Unemployment_Rate_Canada_Last_Date,
                                      Unemployment_Rate_Canada_Last_Month,
                                      Unemployment_Rate_Canada_Last_Year ,
                                      Unemployment_Rate_Canada_Pandemic)

row.names(Unemployment_Rate_Canada_Variations)[1] <-  "Unemployment_Rate"



Employment_Rate_Canada_25_54_Last_Date  <- Employment_Rate_Canada_25_54 %>%  
  filter(Date==Labor_Market_Canada_Last_Date) %>% 
  rename(Last_Date=Employment_Rate) %>% 
  select(Last_Date)

Employment_Rate_Canada_25_54_Last_Month <- Employment_Rate_Canada_25_54 %>%  
  filter(Date==Labor_Market_Canada_Last_Month) %>% 
  rename(Last_Month=Employment_Rate) %>% 
  select(Last_Month)


Employment_Rate_Canada_25_54_Last_Year  <- Employment_Rate_Canada_25_54 %>%  
  filter(Date==Labor_Market_Canada_Last_Year) %>% 
  rename(Last_Year=Employment_Rate) %>% 
  select(Last_Year)


Employment_Rate_Canada_25_54_Pandemic <- Employment_Rate_Canada_25_54 %>%  
  filter(Date==Labor_Market_Canada_Pandemic) %>% 
  rename(Pandemic =Employment_Rate) %>% 
  select(Pandemic )


Employment_Rate_Canada_25_54_Variations <- cbind(Employment_Rate_Canada_25_54_Last_Date,
                                             Employment_Rate_Canada_25_54_Last_Month,
                                             Employment_Rate_Canada_25_54_Last_Year ,
                                             Employment_Rate_Canada_25_54_Pandemic)

row.names(Employment_Rate_Canada_25_54_Variations)[1] <-  "Employment_Rate_Canada_25_54"

Name_Labor_Market_Indicators <- c("Total_Jobs","Unemployment_rate","Employment_rate")
Labor_Market_Indicators <- rbind(Total_Jobs_Canada_Variations,Unemployment_Rate_Canada_Variations,Employment_Rate_Canada_25_54_Variations)

Labor_Market_Indicators <- cbind(Name_Labor_Market_Indicators,Labor_Market_Indicators )


# Province data


 
Total_Jobs_Provinces <- Labor_Market_Canada %>% filter(Sex=="Both sexes",
                               Age_group=="15 years and over",
                               Data_type=="Seasonally adjusted",
                               Statistics =="Estimate",
                               Labour_force_characteristics=="Employment") %>% 
                        filter(Date==Labor_Market_Canada_Last_Date   |
                                 Date==Labor_Market_Canada_Last_Month |
                                 Date==Labor_Market_Canada_Last_Year) %>% 
                        select(GEO,Date,val_norm) %>% 
                        rename(Total_jobs = val_norm)


Total_Jobs_Provinces_Last_Date <- Total_Jobs_Provinces %>% 
                                    filter(Date==Labor_Market_Canada_Last_Date) %>% 
                                  select(GEO,Total_jobs) %>% 
                                  rename(Last_Date = Total_jobs)

Total_Jobs_Provinces_Last_Month <- Total_Jobs_Provinces %>% 
  filter(Date==Labor_Market_Canada_Last_Month) %>% 
  select(GEO,Total_jobs) %>% 
  rename(Last_Month = Total_jobs)

Total_Jobs_Provinces_Last_Year <- Total_Jobs_Provinces %>% 
  filter(Date==Labor_Market_Canada_Last_Year) %>% 
  select(GEO,Total_jobs) %>% 
  rename(Last_Year = Total_jobs)



Total_Jobs_Provinces_Merge <- merge(Total_Jobs_Provinces_Last_Date,Total_Jobs_Provinces_Last_Month,by="GEO")
Total_Jobs_Provinces_Variations <- merge(Total_Jobs_Provinces_Merge,Total_Jobs_Provinces_Last_Year,by="GEO")


Total_Jobs_Provinces_Variations <- Total_Jobs_Provinces_Variations %>% 
                                    mutate(Job_Variations_Month = Last_Date/Last_Month -1,
                                           Job_Variations_Year = Last_Date/Last_Year -1)

Total_Jobs_Provinces_Variations_Month_Year <- Total_Jobs_Provinces_Variations %>% 
                                                  select(GEO, Job_Variations_Month,Job_Variations_Year)


Unemployment_Rate_Provinces <- Labor_Market_Canada %>% 
                                      filter(Sex=="Both sexes",
                                             Age_group=="15 years and over",
                                            Data_type=="Seasonally adjusted",
                                              Statistics =="Estimate",
                                            Labour_force_characteristics=="Unemployment rate") %>% 
                                  filter(Date==Labor_Market_Canada_Last_Date   |
                                           Date==Labor_Market_Canada_Last_Month |
                                           Date==Labor_Market_Canada_Last_Year) %>% 
                                  select(GEO,Date,val_norm) %>% 
                                  rename(Unemployment_Rate = val_norm)


Unemployment_Rate_Provinces_Last_Date <- Unemployment_Rate_Provinces  %>% 
  filter(Date==Labor_Market_Canada_Last_Date) %>% 
  select(GEO,Unemployment_Rate)


# 
# Unemployment_Rate_Provinces_Last_Month <- Unemployment_Rate_Provinces  %>% 
#   filter(Date==Labor_Market_Canada_Last_Month) %>% 
#   select(GEO,Unemployment_Rate) %>% 
#   rename(Last_Month = Unemployment_Rate)
# 
# Unemployment_Rate_Provinces_Last_Year <- Unemployment_Rate_Provinces  %>% 
#   filter(Date==Labor_Market_Canada_Last_Year) %>% 
#   select(GEO, Unemployment_Rate) %>% 
#   rename(Last_Year = Unemployment_Rate)
# 
# 
# Unemployment_Rate_Provinces_Merge <- merge(Unemployment_Rate_Provinces_Last_Date,Unemployment_Rate_Provinces_Last_Month,by="GEO")
# Unemployment_Rate_Provinces_Variations <- merge(Unemployment_Rate_Provinces_Merge,Unemployment_Rate_Provinces_Last_Year,by="GEO")



Employment_Rate_Provinces <- Labor_Market_Canada %>% 
  filter(Sex=="Both sexes",
         Age_group=="25 to 54 years",
         Data_type=="Seasonally adjusted",
         Statistics =="Estimate",
         Labour_force_characteristics=="Employment rate") %>% 
  filter(Date==Labor_Market_Canada_Last_Date   |
           Date==Labor_Market_Canada_Last_Month |
           Date==Labor_Market_Canada_Last_Year) %>% 
  select(GEO,Date,val_norm) %>% 
  rename(Employment_Rate = val_norm)


Employment_Rate_Provinces_Last_Date <- Employment_Rate_Provinces  %>% 
  filter(Date==Labor_Market_Canada_Last_Date) %>% 
  select(GEO,Employment_Rate)  


Provinces_Labor_Indicators_Merge <- merge(Total_Jobs_Provinces_Variations_Month_Year,Unemployment_Rate_Provinces_Last_Date,by="GEO")
Provinces_Labor_Indicators <- merge(Provinces_Labor_Indicators_Merge,Employment_Rate_Provinces_Last_Date,by="GEO")

Provinces_Labor_Indicators <- Provinces_Labor_Indicators[c(3,1,2,4,5,6,7,8,9,10,11),]


# Employment_Rate_Provinces_Last_Month <- Employment_Rate_Provinces  %>% 
#   filter(Date==Labor_Market_Canada_Last_Month) %>% 
#   select(GEO,Employment_Rate) %>% 
#   rename(Last_Month = Employment_Rate)
# 
# Employment_Rate_Provinces_Last_Year <- Employment_Rate_Provinces  %>% 
#   filter(Date==Labor_Market_Canada_Last_Year) %>% 
#   select(GEO, Employment_Rate) %>% 
#   rename(Last_Year = Employment_Rate)
# 
# 
# Employment_Rate_Provinces_Merge <- merge(Employment_Rate_Provinces_Last_Date,Employment_Rate_Provinces_Last_Month,by="GEO")
# Employment_Rate_Provinces_Variations <- merge(Employment_Rate_Provinces_Merge,Employment_Rate_Provinces_Last_Year,by="GEO")
# 
# 


# Job Vacancies - we'll work with quartely data


Job_Vacancy_Quartely <-get_cansim("14-10-0325-01")

names(Job_Vacancy_Quartely )<-str_replace_all(names(Job_Vacancy_Quartely ),
                                            c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

Job_Vacancy_Quartely_Canada <- Job_Vacancy_Quartely %>% filter(GEO=="Canada", 
                                             Statistics == "Job vacancies") %>% 
                                        select(Date,val_norm) %>% 
                                        rename(Job_vacancy = val_norm)

Job_Vacancy_Monthly <-get_cansim("14-10-0371-01")

names(Job_Vacancy_Monthly )<-str_replace_all(names(Job_Vacancy_Monthly  ),
                                              c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

Job_Vacancy_Last_Date <- tail(Job_Vacancy_Monthly$Date,n=1)
Job_Vacancy_Last_Month <- Job_Vacancy_Last_Date-months(1) 



Job_Vacancy_Rate <- Job_Vacancy_Monthly %>% filter(Statistics == "Job vacancy rate") %>% 
                                select(GEO,Date, val_norm) %>% 
                                rename(Job_vacancy_rate = val_norm)

Job_Vacancy_Rate_Last_Date <- Job_Vacancy_Rate %>% 
                              filter(Date==Job_Vacancy_Last_Date) %>% 
                              rename(Last_Date = Job_vacancy_rate)

Job_Vacancy_Rate_Last_Month <- Job_Vacancy_Rate %>% 
  filter(Date==Job_Vacancy_Last_Month) %>% 
  rename(Last_Month = Job_vacancy_rate)


Job_Vacancy_Rate_Provinces_merge <- merge(Job_Vacancy_Rate_Last_Date,Job_Vacancy_Rate_Last_Month, by="GEO")


Job_Vacancy_Province<- Job_Vacancy_Monthly %>% filter(Statistics=="Job vacancies",
                                                  Date==Job_Vacancy_Last_Date) %>% 
                                            select(GEO,val_norm) %>% 
                                            rename(Job_vacancy = val_norm)

Job_Vacancy_Rate_Provinces <- merge(Job_Vacancy_Rate_Provinces_merge,Job_Vacancy_Province, by="GEO")



Job_Vacancy_Rate_Provinces  <- Job_Vacancy_Rate_Provinces[c(3,1,2,4,5,6,7,8,9,10,11,12,13,14),]

Job_Vacancy_Rate_Canada <- Job_Vacancy_Monthly %>% 
         filter( Statistics == "Job vacancies" | 
                   Statistics == "Job vacancy rate") %>%                           
            filter(GEO=="Canada") %>% 
                            filter(Date==Job_Vacancy_Last_Date | 
                                     Date == Job_Vacancy_Last_Month) %>% 
            select(Date,Statistics,val_norm)


Unemployed_Provinces <-  Labor_Market_Canada %>% 
                        filter(Sex=="Both sexes",
                               Age_group=="15 years and over",
                               Data_type=="Seasonally adjusted",
                               Statistics =="Estimate",
                               Labour_force_characteristics=="Unemployment",
                               Date==Labor_Market_Canada_Last_Date) %>% 
                        select(GEO,val_norm) %>% 
                        rename(Unemployed = val_norm)


# Wages

Wages <-get_cansim("14-10-0320-02")

names(Wages)<-str_replace_all(names(Wages),
                                     c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))



Wages_History <- Wages %>%  
  filter(GEO=="Canada",
         Characteristics=="25 years and over",
         Hours_and_wages=="Full-time employees, average weekly wages") %>% 
  select(Date,val_norm) %>% 
  rename(Wages = val_norm)

Wages_Last_Date <- tail(Wages$Date,n=1)
Wages_Last_Month <- Wages_Last_Date-months(1) 
Wages_Last_Year <-  Wages_Last_Date-months(12)

Wages_Variations <- Wages_History %>% 
  filter(Date==Wages_Last_Year | 
           Date==Wages_Last_Month | 
           Date==Wages_Last_Date )





Labor_Data <- createWorkbook()


addWorksheet(Labor_Data,"Labor_Market_Indicators")
addWorksheet(Labor_Data,"Total_Jobs_Canada")
addWorksheet(Labor_Data,"Unemployment_Rate_Canada")
addWorksheet(Labor_Data,"Employment_Rate_Canada_25_54")
addWorksheet(Labor_Data,"Job_Vacancy_Quartely_Canada")
addWorksheet(Labor_Data,"Job_Vacancy_Rate_Provinces")
addWorksheet(Labor_Data,"Job_Vacancy_Rate_Canada")
addWorksheet(Labor_Data,"Provinces_Labor_Indicators")
addWorksheet(Labor_Data,"Unemployed_Provinces")

addWorksheet(Labor_Data,"Wages_History")
addWorksheet(Labor_Data,"Wages_Variations")



writeData(Labor_Data,sheet = "Labor_Market_Indicators",x=Labor_Market_Indicators)
writeData(Labor_Data,sheet = "Total_Jobs_Canada",x=Total_Jobs_Canada)
writeData(Labor_Data,sheet = "Unemployment_Rate_Canada",x=Unemployment_Rate_Canada)
writeData(Labor_Data,sheet = "Employment_Rate_Canada_25_54",x=Employment_Rate_Canada_25_54)
writeData(Labor_Data,sheet = "Job_Vacancy_Quartely_Canada",x=Job_Vacancy_Quartely_Canada)
writeData(Labor_Data,sheet = "Job_Vacancy_Rate_Provinces",x=Job_Vacancy_Rate_Provinces)
writeData(Labor_Data,sheet = "Job_Vacancy_Rate_Canada",x=Job_Vacancy_Rate_Canada)
writeData(Labor_Data,sheet = "Provinces_Labor_Indicators",x=Provinces_Labor_Indicators)
writeData(Labor_Data,sheet = "Unemployed_Provinces",x=Unemployed_Provinces)

writeData(Labor_Data,sheet = "Wages_History",x=Wages_History)
writeData(Labor_Data,sheet = "Wages_Variations",x=Wages_Variations)


saveWorkbook(Labor_Data,"Labor_Data.xlsx",overwrite = TRUE)



#||||||||||||------------------ FINANCE  ----------------------||||||


# Household_Debt

Household_Debt <-get_cansim("36-10-0639-01")

names(Household_Debt)<-str_replace_all(names(Household_Debt),
                              c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

Total_Liability <- Household_Debt %>% 
                    filter(Credit_liabilities_of_households=="Total credit liabilities of households",
                           Seasonality=="Seasonally adjusted data") %>% 
                    select(Date,val_norm) %>% 
                    rename(Total_Liability=val_norm,
                           Date2=Date)


Non_Mortgage_Liability <- Household_Debt %>% 
                  filter(Credit_liabilities_of_households=="Non-mortgage loans",
                         Seasonality=="Seasonally adjusted data") %>% 
                  select(Date,val_norm) %>% 
                  rename(Non_Mortgage_Liability=val_norm)

Household_Liability_History <- cbind(Non_Mortgage_Liability,Total_Liability)

Household_Liability_History <- Household_Liability_History  %>% 
                        mutate(Non_Mortgage_Liability_Percentage = Non_Mortgage_Liability/Total_Liability) %>% 
                        select(Date,Total_Liability,Non_Mortgage_Liability_Percentage)


Household_Liability_Last_Date <- tail(Household_Liability_History$Date,n=1)
Household_Liability_Last_Month <- Household_Liability_Last_Date-months(1) 
Household_Liability_Last_Year <-  Household_Liability_Last_Date-months(12)

Household_Liability_Variations <- Household_Liability_History %>% 
  filter(Date==Household_Liability_Last_Year | 
           Date==Household_Liability_Last_Month | 
           Date==Household_Liability_Last_Date ) 
           


# Government_Debt


Government_Debt <-get_cansim("10-10-0015-01")

names(Government_Debt)<-str_replace_all(names(Government_Debt),
                                       c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

Government_Debt <- Government_Debt %>% 
                    filter(Statement_of_government_operations_and_balance_sheet=="Debt securities, liabilities",
                           Government_sectors=="Consolidated government") %>% 
                    select(Date,val_norm) %>% 
                    rename(Government_Liability = val_norm)

Government_Debt_Last_Date <- tail(Government_Debt$Date,n=1)
Government_Debt_Last_Quarter <- Government_Debt_Last_Date-months(3) 
Government_Debt_Last_Year <-  Government_Debt_Last_Date-months(12)

Government_Debt_Variations <- Government_Debt %>% 
  filter(Date==Government_Debt_Last_Year | 
           Date==Government_Debt_Last_Quarter | 
           Date==Government_Debt_Last_Date ) 
 
# Inflation

Inflation<-get_cansim("18-10-0004-01")

names(Inflation)<-str_replace_all(names(Inflation),
                                        c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

Inflation_History <- Inflation %>% 
  filter(GEO=="Canada",Date>"1989-12-01",
        Products_and_product_groups=="All-items") %>% 
  select(Date,val_norm) %>% 
  rename(CPI = val_norm)


Inflation_Last_Date <- tail(Inflation$Date,n=1)
Inflation_Last_Month <- Inflation_Last_Date-months(1) 
Inflation_Last_Year <-  Inflation_Last_Date-months(12)

Inflation_Variations <- Inflation_History %>% 
  filter(Date==Inflation_Last_Year | 
           Date==Inflation_Last_Month | 
           Date==Inflation_Last_Date ) 


# Housing 


# CREA_Data <- read_excel("Not Seasonally Adjusted.xlsx", sheet = "Aggregate")
# 
# CREA_Data <- my_data %>% 
#             mutate(Date2=as.Date(as.numeric(Date),origin="1899-12-30")) %>% 
#             select(Date2,Composite_Benchmark) %>% 
#             rename(Date=Date2) %>% 
#             drop_na(Date)




Finance_Data <- createWorkbook()


addWorksheet(Finance_Data,"Household_Liability_History")
addWorksheet(Finance_Data,"Household_Liability_Variations")

addWorksheet(Finance_Data,"Government_Debt")
addWorksheet(Finance_Data,"Government_Debt_Variations")

addWorksheet(Finance_Data,"Inflation_History")
addWorksheet(Finance_Data,"Inflation_Variations")

#addWorksheet(Finance_Data,"CREA_Data")


writeData(Finance_Data,sheet = "Household_Liability_History",x=Household_Liability_History)
writeData(Finance_Data,sheet = "Household_Liability_Variations",x=Household_Liability_Variations)

writeData(Finance_Data,sheet = "Government_Debt",x=Government_Debt)
writeData(Finance_Data,sheet = "Government_Debt_Variations",x=Government_Debt_Variations)

writeData(Finance_Data,sheet = "Inflation_History",x=Inflation_History)
writeData(Finance_Data,sheet = "Inflation_Variations",x=Inflation_Variations)

#writeData(Finance_Data,sheet = "CREA_Data",x=CREA_Data)

saveWorkbook(Finance_Data,"Finance_Data.xlsx",overwrite = TRUE)



#||||||||||||------------------ DEMOGRAPHY AND SOCIAL  ----------------------||||||

Population <- get_cansim("17-10-0009-01")

names(Population)<-str_replace_all(names(Population),
                                    c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

Population <- Population %>% 
                filter(GEO=="Canada") %>% 
                  select(Date,val_norm) %>% 
                  rename(Population=val_norm)



Immigration <-get_cansim("17-10-0040-01")

names(Immigration)<-str_replace_all(names(Immigration),
                                  c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

Immigration_History <- Immigration%>% 
  filter(GEO=="Canada",Date>"1989-12-01",
         Components_of_population_growth=="Immigrants") %>% 
  select(Date,val_norm) %>% 
  rename(Immigrant = val_norm)


# Wage gap does not really work

# Wage_Gap_Date = Wages_Last_Year - months(1)
# 
# Wage_Gap <- Wages %>% 
#          filter(GEO=="Canada",
#                 Date>Wage_Gap_Date) %>% 
#         filter(Characteristics=="Males" |
#                  Characteristics=="Females") %>% 
#       filter(Hours_and_wages=="Full-time employees, average usual weekly hours" |
#                Hours_and_wages=="Full-time employees, average weekly wages") %>% 
#           select(Date,Characteristics,Hours_and_wages,val_norm)


# Gap in employment rate - immigrant vs Canadian born women

Employment_Immigration <-get_cansim("14-10-0084-01")

names(Employment_Immigration)<-str_replace_all(names(Employment_Immigration),
                                    c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

Immigration_Gap <- Employment_Immigration %>% 
                  filter(Immigrant_status == "Born in Canada" |
                           Immigrant_status=="Landed immigrants") %>% 
                  filter(Labour_force_characteristics=="Employment rate",
                         Age_group=="25 to 54 years",
                         Sex=="Females") %>% 
                  select(Date,Immigrant_status,val_norm) %>% 
                  rename(Employment_Rate= val_norm)

Employment_Rate_Immigration_Women <- Immigration_Gap %>% 
                                      filter(Immigrant_status=="Landed immigrants") %>% 
                                      select(Date, Employment_Rate) %>% 
                                      rename(Employment_Rate_Immigrant = Employment_Rate)
                                      
Employment_Rate_Born_Canada_Women <- Immigration_Gap %>% 
                                filter(Immigrant_status=="Born in Canada") %>% 
                                select(Date, Employment_Rate) %>% 
                                rename(Date2 = Date, 
                                       Employment_Rate_Born_Canada = Employment_Rate) 
                               

Immigration_Gap_Women <- cbind(Employment_Rate_Immigration_Women,Employment_Rate_Born_Canada_Women) %>% 
                          select(-Date2) %>% 
                           mutate(Employment_Rate_Gap = Employment_Rate_Immigrant - Employment_Rate_Born_Canada)


# Indigenous Employement Gap 

Indigenous_Employment  <-get_cansim("14-10-0401-01")

names(Indigenous_Employment)<-str_replace_all(names(Indigenous_Employment),
                                               c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))                              


Indigenous_Employment_Gap <- Indigenous_Employment %>% 
                            filter(GEO=="Canada", 
                                   Labour_force_characteristics=="Employment rate",
                                   Age=="25 to 54 years",
                                   Sex=="Both sexes") %>% 
                            filter(Indigenous_group=="Non-Indigenous population" |
                                     Indigenous_group=="Indigenous population" ) %>% 
                            select(Date, Indigenous_group,val_norm) %>% 
                            rename(Employment_Rate = val_norm)



Indigenous_Employment_Rate <- Indigenous_Employment_Gap %>% 
  filter(Indigenous_group=="Indigenous population") %>% 
  select(Date, Employment_Rate) %>% 
  rename(Employment_Rate_Indigenous = Employment_Rate)

Employment_Rate_Non_Indigenous <- Indigenous_Employment_Gap  %>% 
  filter(Indigenous_group=="Non-Indigenous population") %>% 
  select(Date, Employment_Rate) %>% 
  rename(Date2 = Date, 
         Employment_Rate_Non_Indigenous = Employment_Rate) 


Indigenous_Employment_Gap_Final <- cbind(Indigenous_Employment_Rate,Employment_Rate_Non_Indigenous) %>% 
                                    select(-Date2) %>% 
                        mutate(Employment_Rate_Gap = Employment_Rate_Indigenous - Employment_Rate_Non_Indigenous)

# Wealth disparity 

Wealth_Disparity  <-get_cansim("36-10-0660-01")

names(Wealth_Disparity)<-str_replace_all(names(Wealth_Disparity),
                                              c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))                              

Wealth_Disparity <- Wealth_Disparity %>% 
                    filter(Statistics=="Distribution of value",
                           Date>"2019-07-01",
                           Wealth=="Net worth (wealth)") %>% 
                    filter(Characteristics=="Lowest wealth quintile" |
                             Characteristics=="Second wealth quintile" |
                             Characteristics=="Third wealth quintile" |
                             Characteristics=="Fourth wealth quintile" |
                             Characteristics=="Highest wealth quintile" ) %>% 
                    select(Date,Characteristics,val_norm) %>% 
                    rename(Percent_Wealth = val_norm)


First_Quintile <- Wealth_Disparity %>% 
                    filter(Characteristics=="Lowest wealth quintile") %>% 
                    rename(Date2=Date, Percent_Wealth_First_Quintile=Percent_Wealth )


Second_Quintile <- Wealth_Disparity %>% 
  filter(Characteristics=="Second wealth quintile") %>% 
  rename(Date3=Date, Percent_Wealth_Second_Quintile=Percent_Wealth )

Third_Quintile <- Wealth_Disparity %>% 
  filter(Characteristics=="Third wealth quintile") %>% 
  rename(Date4=Date, Percent_Wealth_Third_Quintile=Percent_Wealth )

Fourth_Quintile <- Wealth_Disparity %>% 
  filter(Characteristics=="Fourth wealth quintile") %>% 
  rename( Date5= Date, Percent_Wealth_Fourth_Quintile=Percent_Wealth )

Fifth_Quintile <- Wealth_Disparity %>% 
  filter(Characteristics=="Highest wealth quintile") %>% 
  rename( Percent_Wealth_Fifth_Quintile=Percent_Wealth )


Quintiles_Wealth <- cbind(First_Quintile,Second_Quintile,Third_Quintile,Fourth_Quintile,Fifth_Quintile) %>% 
                    select(Date,
                           Percent_Wealth_First_Quintile,
                           Percent_Wealth_Second_Quintile,
                           Percent_Wealth_Third_Quintile,
                           Percent_Wealth_Fourth_Quintile,
                           Percent_Wealth_Fifth_Quintile)

# Working with LFS microdata 

#setwd("C:/Users/dabrassard/Desktop/Working folder/Ecnomic_Dashboard/Data/LFS")

setwd("C:/Users/LNB/Desktop/Dossier_DAB/Projets R/Economic_Dashboard/LFS")


files <- c("pub0921.csv","pub0822.csv","pub0922.csv") # Change for last month, last year

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
        
        
        Young_Not_student_Not_employed <- cbind(unique(Labor_Force$SURVYEAR),unique(Labor_Force$SURVMNTH),Young_People_Weight,Non_Student_Weight,Non_Student_Non_Employed_Weight,Non_Student_Non_Employed_Weight)
          
          NSNE_Young_People <- rbind(NSNE_Young_People,Young_Not_student_Not_employed)
        
        } 

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



 




#setwd("C:/Users/dabrassard/Desktop/Working folder/Ecnomic_Dashboard/Data")

setwd("C:/Users/LNB/Desktop/Dossier_DAB/Projets R/Economic_Dashboard")

Demography_Social_Data <- createWorkbook()

addWorksheet(Demography_Social_Data,"Population")
addWorksheet(Demography_Social_Data,"Immigration_History")
addWorksheet(Demography_Social_Data,"Immigration_Gap_Women")
addWorksheet(Demography_Social_Data,"Indigenous_Employment_Gap_Final")
addWorksheet(Demography_Social_Data,"Quintiles_Wealth")
addWorksheet(Demography_Social_Data,"NSNE_Young_People")
addWorksheet(Demography_Social_Data,"Wage_Difference_Men_Women")

writeData(Demography_Social_Data,sheet = "Population",x=Population)
writeData(Demography_Social_Data,sheet = "Immigration_History",x=Immigration_History)
writeData(Demography_Social_Data,sheet = "Immigration_Gap_Women",x=Immigration_Gap_Women)
writeData(Demography_Social_Data,sheet = "Indigenous_Employment_Gap_Final",x=Indigenous_Employment_Gap_Final)
writeData(Demography_Social_Data,sheet = "Quintiles_Wealth",x=Quintiles_Wealth)
writeData(Demography_Social_Data,sheet = "NSNE_Young_People",x=NSNE_Young_People)
writeData(Demography_Social_Data,sheet = "Wage_Difference_Men_Women",x=Wage_Difference_Loop)


saveWorkbook(Demography_Social_Data,"Social_Data.xlsx",overwrite = TRUE)



