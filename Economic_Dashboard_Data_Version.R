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

fredr_set_key("529a8fe75d1703e7b03fbbb9898730b2")


setwd("D:/economic_dashboard")
#||||||||||||------------------ ECONOMY SECTION----------------------||||||


get_date <- function(df,spread) {
  
  
  # Spread is 1 for monthly, 3 for quartely and 12 for annual data
  date_1 <- tail({{df}}$Date,n=1)
  date_2 <- date_1 - months(spread)
  date_3 <- date_2 - months(12)
  
  date_vector <<- c(date_1,date_2, date_3)
  
}


GDP_Monthly_Industry <-get_cansim("36-10-0434-01")

names(GDP_Monthly_Industry)<-str_replace_all(names(GDP_Monthly_Industry),
                                                     c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

GDP_Monthly_History <- GDP_Monthly_Industry %>% 
                                filter(Seasonal_adjustment=="Seasonally adjusted at annual rates",
                                       North_American_Industry_Classification_System__NAICS_=="All industries [T001]",
                                       Prices=="Chained (2012) dollars") %>% 
                                select(Date,val_norm) %>% 
                            rename(Real_GDP=val_norm) 

get_date(GDP_Monthly_Industry,1)


GDP_Monthly_Variations <- GDP_Monthly_History%>% 
                          filter(Date %in% date_vector)

# Manufacturing sales

Manufacturing <-get_cansim("16-10-0047-01")

names(Manufacturing)<-str_replace_all(names(Manufacturing),
                                      c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

get_date(Manufacturing,1)

Manufacturing_History <- Manufacturing %>% 
                          filter(North_American_Industry_Classification_System__NAICS_=="Manufacturing [31-33]",
                                 Seasonal_adjustment=="Seasonally adjusted",
                                 Principal_statistics=="Sales of goods manufactured (shipments)") %>% 
                          select(Date,val_norm) %>% 
                          rename(Manufacturing_sales=val_norm)


Manufacturing_Variations <- Manufacturing_History %>% 
                            filter(Date %in% date_vector)



# International_Trade

export_canada<- get_cansim_vector("v1001809606",
                                  start_time = "2000-01-01") %>% 
                    select(Date,val_norm) %>% 
                    rename(export = val_norm)

get_date(export_canada,1)

export_canada_variations <- 
  export_canada %>% 
    filter(Date %in% date_vector)
        


# Retail_Trade

retail_trade <-get_cansim("20-10-0008-01")

names(retail_trade)<-str_replace_all(names(retail_trade),
                                     c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

get_date(retail_trade,1)


retail_trade_history <- retail_trade %>% 
                        filter(GEO=="Canada",
                               Adjustments=="Seasonally adjusted",
                               North_American_Industry_Classification_System__NAICS_=="Retail trade [44-45]") %>% 
                        select(Date,val_norm) %>% 
                        rename(Retail_trade = val_norm)

retail_trade_variations <- retail_trade_history %>% 
  filter(Date %in% date_vector )


# Active_businesses


business <-get_cansim("33-10-0270-01")

names(business)<-str_replace_all(names(business),
                                     c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

get_date(business,1) 


Active_Business_History <- business %>%  
  filter(GEO=="Canada",
         Industry=="Business sector industries [T004]",
  Business_dynamics_measure=="Active businesses") %>% 
  select(Date,val_norm) %>% 
  rename(Active_Business = val_norm)

Active_Business_Variations <- Active_Business_History %>% 
  filter(Date %in% date_vector)

# Nominal GDP 

Nominal_GDP <-get_cansim("36-10-0104-01")

names(Nominal_GDP)<-str_replace_all(names(Nominal_GDP),
                                        c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

Nominal_GDP_History <- Nominal_GDP %>%  
  filter(GEO=="Canada",
         Estimates=="Gross domestic product at market prices",
         Prices=="Current prices",
         Date>"1980-12-01",
         Seasonal_adjustment=="Seasonally adjusted at annual rates") %>% 
  select(Date,val_norm) %>% 
  rename(Nominal_GDP = val_norm)


summary_table_economy <- 
        bind_rows(GDP_Monthly_Variations,
                  Manufacturing_Variations,
                  export_canada_variations,
                  retail_trade_variations,
                  Active_Business_Variations)


Economy_data <- createWorkbook()

addWorksheet(Economy_data,"summary_table")
addWorksheet(Economy_data,"GDP_Monthly_History")
addWorksheet(Economy_data,"Nominal_GDP_History")
addWorksheet(Economy_data,"Manufacturing_History")
addWorksheet(Economy_data,"International_Trade_History")
addWorksheet(Economy_data,"Retail_Trade_History")
addWorksheet(Economy_data,"Active_Business_History")


writeData(Economy_data,sheet = "summary_table",x=summary_table_economy)
writeData(Economy_data,sheet = "GDP_Monthly_History",x=GDP_Monthly_History)
writeData(Economy_data,sheet = "Nominal_GDP_History",x=Nominal_GDP_History)
writeData(Economy_data,sheet = "Manufacturing_History",x=Manufacturing_History)
writeData(Economy_data,sheet = "International_Trade_History",x=export_canada)
writeData(Economy_data,sheet = "Retail_Trade_History",x=retail_trade_history)
writeData(Economy_data,sheet = "Active_Business_History",x=Active_Business_History)


saveWorkbook(Economy_data,"Economy_Dashboard_Data.xlsx",overwrite = TRUE)



#||||||||||||------------------ LABOR MARKET ----------------------||||||


employment_canada<- get_cansim_vector("v2062811",
                                      start_time = "2000-01-01") %>% 
                    select(Date,val_norm) %>% 
                    rename(employment = val_norm)

get_date(employment_canada,1)

employment_canada_variations <- 
  employment_canada %>% 
  filter(Date %in% date_vector)

#---------------------------------------------------------------------------

unemployment_canada<- get_cansim_vector("v2062815",
                                        start_time = "2000-01-01") %>% 
  select(Date,val_norm) %>% 
  rename(unemployment = val_norm)

get_date(unemployment_canada,1)

unemployment_canada_variations <- 
  unemployment_canada %>% 
  filter(Date %in% date_vector)


#---------------------------------------------------------------------------

employment_rate_canada<- get_cansim_vector("v2062952",
                                           start_time = "2000-01-01") %>% 
                          select(Date,val_norm) %>% 
                          rename(employment_rate = val_norm)

get_date(employment_rate_canada,1)


employment_rate_canada_variations <- 
  employment_rate_canada %>% 
  filter(Date %in% date_vector)


#---------------------------------------------------------------------------



job_vacancy_quarter<- get_cansim_vector("v104272652", 
                                        end_time = "2020-10-01") %>% 
  select(Date,val_norm) %>% 
  rename(job_vacancy = val_norm)


job_vacancy_rate_quarter<- get_cansim_vector("v104272654",
                                             end_time = "2020-10-01") %>% 
  select(Date,val_norm) %>% 
  rename(job_vacancy_rate = val_norm)


job_vacancy_monthly<- get_cansim_vector("v1212389364",
                                        start_time = "2020-11-01") %>% 
  select(Date,val_norm) %>% 
  rename(job_vacancy = val_norm)

get_date(job_vacancy_monthly,1)

job_vacancy_rate_monthly<- get_cansim_vector("v1212389365",
                                             start_time = "2020-11-01") %>% 
  select(Date,val_norm) %>% 
  rename(job_vacancy_rate = val_norm)


job_vacancy_canada <- 
    bind_rows(job_vacancy_quarter,
              job_vacancy_monthly)

job_vacancy_rate_canada <- 
    bind_rows(job_vacancy_rate_quarter,
              job_vacancy_rate_monthly)

job_vacancy_canada_variations <- 
  job_vacancy_canada %>% 
      filter(Date %in% date_vector)

job_vacancy_rate_canada_variations <- 
  job_vacancy_rate_canada %>% 
  filter(Date %in% date_vector)

# Wages

Wages <-get_cansim("14-10-0320-02")

names(Wages)<-str_replace_all(names(Wages),
                                     c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))
get_date(Wages,1) 

Wages_History <- Wages %>%  
  filter(GEO=="Canada",
         Characteristics=="25 years and over",
         Hours_and_wages=="Full-time employees, average weekly wages") %>% 
  select(Date,val_norm) %>% 
  rename(Wages = val_norm)

Wages_Variations <- Wages_History %>% 
  filter(Date %in% date_vector)


summary_table_labor <- 
        bind_rows(employment_canada_variations,
                  unemployment_canada_variations,
                  employment_rate_canada_variations,
                  job_vacancy_canada_variations,
                  Wages_Variations)


Labor_Data <- createWorkbook()


addWorksheet(Labor_Data,"summary_table")
addWorksheet(Labor_Data,"Total_Jobs_Canada")
addWorksheet(Labor_Data,"Unemployment_Rate_Canada")
addWorksheet(Labor_Data,"Employment_Rate_Canada_25_54")
addWorksheet(Labor_Data,"Job_Vacancy")
addWorksheet(Labor_Data,"Job_Vacancy_Rate")
addWorksheet(Labor_Data,"Wages_History")


writeData(Labor_Data,sheet = "summary_table",x=summary_table_labor)
writeData(Labor_Data,sheet = "Total_Jobs_Canada",x=employment_canada_variations)
writeData(Labor_Data,sheet = "Unemployment_Rate_Canada",x=unemployment_canada)
writeData(Labor_Data,sheet = "Employment_Rate_Canada_25_54",x=employment_rate_canada)
writeData(Labor_Data,sheet = "Job_Vacancy",x=job_vacancy_canada)
writeData(Labor_Data,sheet = "Job_Vacancy_Rate",x=job_vacancy_rate_canada)
writeData(Labor_Data,sheet = "Wages_History",x=Wages_History)


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
           

household_debt_gdp <- merge(x=Household_Liability_History,y=Nominal_GDP_History, by="Date") 

household_debt_gdp <- 
  household_debt_gdp %>%
    mutate(household_debt_gdp = Total_Liability/Nominal_GDP)



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



government_debt_gdp <- merge(x=Government_Debt,y=Nominal_GDP_History, by="Date")

government_debt_gdp <- 
  government_debt_gdp %>% 
      mutate(government_debt_gdp = Government_Liability / Nominal_GDP)
 
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



cpi_all_items<- Inflation %>% 
  filter(GEO=="Canada",
         Products_and_product_groups=="All-items") %>% 
  filter(Date == Inflation_Last_Date | 
           Date == Inflation_Last_Month | 
           Date == Inflation_Last_Year ) %>% 
  select(Date,val_norm) %>% 
  rename(cpi_all_item = val_norm)

cpi_no_energy<- Inflation %>% 
  filter(GEO=="Canada",
         Products_and_product_groups=="All-items excluding energy") %>% 
  filter(Date == Inflation_Last_Date | 
           Date == Inflation_Last_Month | 
           Date == Inflation_Last_Year ) %>% 
  select(Date,val_norm) %>% 
  rename(cpi_no_energy = val_norm)

cpi_food<- Inflation %>% 
  filter(GEO=="Canada",
         Products_and_product_groups=="Food") %>% 
  filter(Date == Inflation_Last_Date | 
           Date == Inflation_Last_Month | 
           Date == Inflation_Last_Year ) %>% 
  select(Date,val_norm) %>% 
  rename(cpi_food = val_norm)

cpi_shelter<- Inflation %>% 
  filter(GEO=="Canada",
         Products_and_product_groups=="Shelter") %>% 
  filter(Date == Inflation_Last_Date | 
           Date == Inflation_Last_Month | 
           Date == Inflation_Last_Year ) %>% 
  select(Date,val_norm) %>% 
  rename(cpi_shelter = val_norm)
  

cpi_transportation<- Inflation %>% 
  filter(GEO=="Canada",
         Products_and_product_groups=="Transportation") %>% 
  filter(Date == Inflation_Last_Date | 
           Date == Inflation_Last_Month | 
           Date == Inflation_Last_Year ) %>% 
  select(Date,val_norm) %>% 
  rename(cpi_transportation = val_norm)


cpi_goods<- Inflation %>% 
  filter(GEO=="Canada",
         Products_and_product_groups=="Goods") %>% 
  filter(Date == Inflation_Last_Date | 
           Date == Inflation_Last_Month | 
           Date == Inflation_Last_Year ) %>% 
  select(Date,val_norm) %>% 
  rename(cpi_goods = val_norm)

cpi_services<- Inflation %>% 
  filter(GEO=="Canada",
         Products_and_product_groups=="Services") %>% 
  filter(Date == Inflation_Last_Date | 
           Date == Inflation_Last_Month | 
           Date == Inflation_Last_Year ) %>% 
  select(Date,val_norm) %>% 
  rename(cpi_services = val_norm)
  
 

cpi_component <- bind_cols(cpi_all_items, 
                           cpi_no_energy %>% select(cpi_no_energy),
                           cpi_food %>% select(cpi_food),
                           cpi_shelter %>% select(cpi_shelter),
                           cpi_transportation %>% select(cpi_transportation),
                           cpi_goods %>% select(cpi_goods),
                           cpi_services %>% select(cpi_services))



Finance_Data <- createWorkbook()


addWorksheet(Finance_Data,"Household_Liability_History")
addWorksheet(Finance_Data,"Household_Liability_Variations")

addWorksheet(Finance_Data,"Government_Debt")
addWorksheet(Finance_Data,"Government_Debt_Variations")

addWorksheet(Finance_Data,"Inflation_History")
addWorksheet(Finance_Data,"Inflation_Variations")

#addWorksheet(Finance_Data,"CREA_Data")

addWorksheet(Finance_Data,"cpi_component")

writeData(Finance_Data,sheet = "Household_Liability_History",x=household_debt_gdp)
writeData(Finance_Data,sheet = "Household_Liability_Variations",x=Household_Liability_Variations)

writeData(Finance_Data,sheet = "Government_Debt",x=government_debt_gdp)
writeData(Finance_Data,sheet = "Government_Debt_Variations",x=Government_Debt_Variations)

writeData(Finance_Data,sheet = "Inflation_History",x=Inflation_History)
writeData(Finance_Data,sheet = "Inflation_Variations",x=Inflation_Variations)

#writeData(Finance_Data,sheet = "CREA_Data",x=CREA_Data)

writeData(Finance_Data,sheet = "cpi_component",x=cpi_component)


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

setwd("D:/economic_dashboard/LFS")


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



setwd("D:/economic_dashboard")

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


### PROVINCE DATA

#------------------- GDP-------------------------------


gdp_annual <-get_cansim("36-10-0222-01")


province_vector <- c(unique(gdp_annual$GEO)[1:12],unique(gdp_annual$GEO)[14:15])


names(gdp_annual )<-str_replace_all(names(gdp_annual ),
                                    c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))                              
get_date(gdp_annual,12)

gdp_annual <- 
  gdp_annual  %>%  
  filter(GEO %in% province_vector, 
         Date %in% date_vector, 
         Prices == "Chained (2012) dollars",
         Estimates=="Gross domestic product at market prices") %>%  
  select(GEO,Date,val_norm) %>% 
  rename(real_gdp= val_norm) %>% 
  arrange(GEO,Date) 

#-------------------MANUFACTURING-------------------------------  

manufacturing <-  get_cansim("16-10-0048-01") 

names(manufacturing )<-str_replace_all(names(manufacturing ),
                                       c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))    

get_date(manufacturing,1)

manufacturing <- 
  manufacturing %>%  
  filter(GEO %in% province_vector, 
         Date %in% date_vector) %>% 
  filter(North_American_Industry_Classification_System__NAICS_=="Manufacturing [31-33]",
         Seasonal_adjustment=="Seasonally adjusted",
         Principal_statistics=="Sales of goods manufactured (shipments)") %>% 
  select(GEO,Date,val_norm) %>% 
  rename(manufacturing_sales= val_norm) %>% 
  arrange(GEO,Date) 



#-------------------EXPORTS-------------------------------  

get_date(export_canada,1)


export_province <- tibble()

 province_vector <- c("v1001819785",
                             "v1001820916",
                             "v1001809606",
                             "v1001817523",
                             "v1001814130",
                             "v1001810737",
                             "v1001824309",
                             "v1001812999",
                             "v1001825440",
                             "v1001816392",
                             "v1001811868",
                             "v1001815261",
                             "v1001818654",
                             "v1001822047"
                             )
 
 province_name_vector <- c("Alberta",
                                  "British Columbia",
                                  "Canada",
                                  "Manitoba",
                                 "New Brunswick",
                                  "Newfoundland and Labrador",
                                  "Northwest Territories",
                                  "Nova Scotia",
                                  "Nunavut",
                                  "Ontario",
                                  "Prince Edward Island",
                                  "Quebec",
                                  "Saskatchewan",
                                  "Yukon")

 
for (j in 1:3) {
 
 for (i in 1:length(province_vector)) {
   
   vector_value <- get_cansim_vector(province_vector[i],
                      start_time = date_vector[j],
                      end_time = date_vector[j])
   
   vector_value <- bind_cols( vector_value,province_name_vector[i])
   export_province <- bind_rows(export_province,vector_value)
   
 }
}
 
 names(export_province)[12] <- "GEO"
 
 export_province <- 
   export_province %>% 
      select(GEO,Date,val_norm) %>% 
      rename(export = val_norm)
 
 

#------------------RETAIL TRADE------------------------------  

get_date(retail_trade,1)

retail_trade_province <- 
  retail_trade %>% 
  filter(GEO %in% province_vector, 
         Date %in% date_vector) %>% 
  filter(Adjustments=="Seasonally adjusted",
         North_American_Industry_Classification_System__NAICS_=="Retail trade [44-45]") %>% 
  select(GEO, Date,val_norm) %>% 
  rename(retail_sale = val_norm) %>% 
  arrange(GEO,Date)



#------------------ACTIVE BUSINESSES------------------------------  

get_date(business,1) 

business_province <- 
  business %>%  
  filter(GEO %in% province_vector, 
         Date %in% date_vector) %>% 
  filter(Industry=="Business sector industries [T004]",
         Business_dynamics_measure=="Active businesses") %>% 
  select(GEO, Date,val_norm) %>% 
  rename(active_business = val_norm) %>% 
  arrange(GEO,Date)




#Mistake in the database 


province_number <- c(1,2,4,5,7,10,16,33,35,38,42,47,48,49)

province_vector_mistake <- c(unique(business$GEO)[1])

for (i in 1:length(province_number)) {
  
  value =  unique(business$GEO)[province_number[i]]
  province_vector_mistake <-  append(province_vector_mistake,values = value)
  
}


business_province <- 
  business %>%  
  filter( Date %in% date_vector) %>% 
  filter(GEO  %in% province_vector_mistake) %>% 
  filter(Industry=="Business sector industries [T004]",
         Business_dynamics_measure=="Active businesses") %>% 
  select(GEO, Date,val_norm) %>% 
  rename(active_business = val_norm) %>% 
  arrange(GEO,Date)


summary_table_economy_province <- 



setwd("D:/economic_dashboard")

province_economy  <- createWorkbook()

addWorksheet(province_economy ,"gdp")
addWorksheet(province_economy ,"manufacturing_sale")
addWorksheet(province_economy ,"export")
addWorksheet(province_economy ,"retail_trade")
addWorksheet(province_economy ,"active_business")

writeData(province_economy ,sheet = "gdp",x=gdp_annual)
writeData(province_economy ,sheet = "manufacturing_sale",x=manufacturing)
writeData(province_economy ,sheet = "export",x=export_province)
writeData(province_economy ,sheet = "retail_trade",x=retail_trade_province)
writeData(province_economy ,sheet = "active_business",x=business_province)


saveWorkbook(province_economy ,"province_economy.xlsx",overwrite = TRUE)


#------------------LABOR STATISTISCS-----------------------------  



# labor <-get_cansim("14-10-0287-03")
# 
# names(labor)<-str_replace_all(names(labor),
#                                             c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))


get_date(employment_canada,1)

employment_province <- tibble()

province_vector <- c("v2062811",
                     "v2063000",
                     "v2063189",
                     "v2063378",
                     "v2063567",
                     "v2063756",
                     "v2063945",
                     "v2064134",
                     "v2064323",
                     "v2064512",
                     "v2064701"
)


province_name_vector <- c("Canada",
                          "Newfoundland and Labrador",
                          "Prince Edward Island",
                          "Nova Scotia",
                          "New Brunswick",
                          "Quebec",
                          "Ontario",
                          "Manitoba",
                          "Saskatchewan",
                          "Alberta",
                          "British Columbia")

for (j in 1:3) {
  
  for (i in 1:length(province_vector)) {
    
    vector_value <- get_cansim_vector(province_vector[i],
                                      start_time = date_vector[j],
                                      end_time = date_vector[j])
    
    vector_value <- bind_cols( vector_value,province_name_vector[i])
    employment_province <- bind_rows(employment_province,vector_value)
    
  }
}

names(employment_province)[12] <- "GEO"

employment_province <- 
  employment_province %>% 
  select(GEO,Date,val_norm) %>% 
  rename(total_employment = val_norm) %>% 
  arrange(GEO,Date)



#-------------------------------------------------------------

get_date(unemployment_canada,1)

unemployment_province <- tibble()

province_vector <- c("v2062815",
                     "v2063004",
                     "v2063193",
                     "v2063382",
                     "v2063571",
                     "v2063760",
                     "v2063949",
                     "v2064138",
                     "v2064327",
                     "v2064516",
                     "v2064705"
)


province_name_vector <- c("Canada",
                          "Newfoundland and Labrador",
                          "Prince Edward Island",
                          "Nova Scotia",
                          "New Brunswick",
                          "Quebec",
                          "Ontario",
                          "Manitoba",
                          "Saskatchewan",
                          "Alberta",
                          "British Columbia")

for (j in 1:3) {
  
  for (i in 1:length(province_vector)) {
    
    vector_value <- get_cansim_vector(province_vector[i],
                                      start_time = date_vector[j],
                                      end_time = date_vector[j])
    
    vector_value <- bind_cols( vector_value,province_name_vector[i])
    unemployment_province <- bind_rows(unemployment_province,vector_value)
    
  }
}

names(unemployment_province)[12] <- "GEO"

unemployment_province <- 
  unemployment_province %>% 
  select(GEO,Date,val_norm) %>% 
  rename(unemployment = val_norm) %>% 
  arrange(GEO,Date)


#-------------------------------------------------------------

get_date(employment_rate_canada,1)

employment_rate_province <- tibble()

province_vector <- c("v2062952",
                     "v2063141",
                     "v2063330",
                     "v2063519",
                     "v2063708",
                     "v2063897",
                     "v2064086",
                     "v2064275",
                     "v2064464",
                     "v2064653",
                     "v2064842"
)


province_name_vector <- c("Canada",
                          "Newfoundland and Labrador",
                          "Prince Edward Island",
                          "Nova Scotia",
                          "New Brunswick",
                          "Quebec",
                          "Ontario",
                          "Manitoba",
                          "Saskatchewan",
                          "Alberta",
                          "British Columbia")

for (j in 1:3) {
  
  for (i in 1:length(province_vector)) {
    
    vector_value <- get_cansim_vector(province_vector[i],
                                      start_time = date_vector[j],
                                      end_time = date_vector[j])
    
    vector_value <- bind_cols( vector_value,province_name_vector[i])
    employment_rate_province <- bind_rows(employment_rate_province,vector_value)
    
  }
}

names(employment_rate_province)[12] <- "GEO"

employment_rate_province <- 
  employment_rate_province %>% 
  select(GEO,Date,val_norm) %>% 
  rename(employment_rate = val_norm) %>% 
  arrange(GEO,Date)



#------------------Job Vacancy-----------------------------  


job_vacancy_monthly <-get_cansim("14-10-0371-01")
get_date(job_vacancy_monthly,1) 


names(job_vacancy_monthly )<-str_replace_all(names(job_vacancy_monthly  ),
                                             c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))
province_vector <- c(unique(gdp_annual$GEO)[1:12],unique(gdp_annual$GEO)[14:15])


job_vacancy_province <- 
  job_vacancy_monthly%>% 
   filter(GEO %in% province_vector, 
          Date %in% date_vector) %>% 
  filter(Statistics=="Job vacancies") %>% 
  select(GEO, Date,val_norm) %>% 
  rename(job_vacancy= val_norm) %>% 
  arrange(GEO,Date)


job_vacancy_rate_province <- 
  job_vacancy_monthly%>% 
  filter(GEO %in% province_vector, 
         Date %in% date_vector) %>% 
  filter(Statistics=="Job vacancy rate") %>% 
  select(GEO, Date,val_norm) %>% 
  rename(job_vacancy_rate= val_norm) %>% 
  arrange(GEO,Date)


job_vacancy <- inner_join(x=job_vacancy_province,
                          y=job_vacancy_rate_province,
                          by=c("GEO","Date")) 



#-------------------------------------------------

wages_canada <- get_cansim_vector("v2133245")

get_date(wages_canada,1)

wages_province <- tibble()

province_vector <- c("v2133245",
                     "v2136665",
                     "v2140085",
                     "v2143505",
                     "v2146925",
                     "v2150345",
                     "v2153765",
                     "v2157185",
                     "v2160605",
                     "v2164025",
                     "v2167445"
)


province_name_vector <- c("Canada",
                          "Newfoundland and Labrador",
                          "Prince Edward Island",
                          "Nova Scotia",
                          "New Brunswick",
                          "Quebec",
                          "Ontario",
                          "Manitoba",
                          "Saskatchewan",
                          "Alberta",
                          "British Columbia")

for (j in 1:3) {
  
  for (i in 1:length(province_vector)) {
    
    vector_value <- get_cansim_vector(province_vector[i],
                                      start_time = date_vector[j],
                                      end_time = date_vector[j])
    
    vector_value <- bind_cols( vector_value,province_name_vector[i])
    wages_province <- bind_rows(wages_province,vector_value)
    
  }
}


names(wages_province)[12] <- "GEO"

wages_province <- 
  wages_province %>% 
  select(GEO,Date,val_norm) %>% 
  rename(wages = val_norm) %>% 
  arrange(GEO,Date)


province_labor  <- createWorkbook()

#------------------------ 
get_date(Inflation,3)

province_vector <- c(unique(gdp_annual$GEO)[1:12],unique(gdp_annual$GEO)[14:15]) 

inflation_province <- 
     Inflation %>% 
          filter(GEO %in% province_vector, 
                 Date %in% date_vector) %>%
          filter(Products_and_product_groups=="All-items") %>% 
          select(GEO,Date, val_norm) %>% 
          rename(cpi = val_norm) %>% 
          arrange(GEO,Date)

#------------------------ 
get_date(Immigration,3)


immigration_province <- 
      Immigration %>% 
            filter(GEO %in% province_vector, 
                   Date %in% date_vector) %>% 
            filter(Components_of_population_growth=="Immigrants") %>% 
            select(GEO,Date, val_norm) %>% 
            rename(immigration = val_norm) %>% 
            arrange(GEO,Date)

        

province_labor  <- createWorkbook()

#-------------------------------------------------
addWorksheet(province_labor ,"total_employment")
addWorksheet(province_labor ,"unemployment_rate")
addWorksheet(province_labor ,"employment_rate")
addWorksheet(province_labor ,"job_vacancy")
addWorksheet(province_labor ,"average_earning")
addWorksheet(province_labor ,"inflation_province")
addWorksheet(province_labor ,"immigration_province")



#-------------------------------------------------
writeData(province_labor,sheet = "total_employment",x=employment_province)
writeData(province_labor,sheet = "unemployment_rate",x=unemployment_province)
writeData(province_labor,sheet = "employment_rate",x=employment_province)
writeData(province_labor,sheet = "job_vacancy",x=job_vacancy)
writeData(province_labor,sheet = "average_earning",x=wages_province)
writeData(province_labor,sheet = "inflation_province",x=inflation_province)
writeData(province_labor,sheet = "immigration_province",x=immigration_province)


saveWorkbook(province_labor,"province_labor_finance.xlsx",overwrite = TRUE)















