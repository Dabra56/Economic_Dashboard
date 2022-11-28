


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
  filter(Date==date_vector[1]) %>% 
  select(Industry,val_norm) %>% 
  rename(Last_Date=val_norm)

GDP_Monthly_NAIC2_Last_Month <- GDP_Monthly_NAIC2 %>%  
  filter(Date==date_vector[2])  %>%  
  select(Industry,val_norm) %>% 
  rename(Last_Month=val_norm, Industry2=Industry)

GDP_Monthly_NAIC2_Last_Year <- GDP_Monthly_NAIC2 %>%  
  filter(Date==date_vector[3]) %>%  
  select(Industry,val_norm) %>% 
  rename(Last_Year=val_norm,Industry3=Industry)

# Change date for pandemic


GDP_Monthly_NAIC2_Variations <- cbind(GDP_Monthly_NAIC2_Last_Date,
                                      GDP_Monthly_NAIC2_Last_Month,
                                      GDP_Monthly_NAIC2_Last_Year)

GDP_Monthly_NAIC2_Variations <- GDP_Monthly_NAIC2_Variations %>% 
  select(Industry,Last_Date,Last_Month,Last_Year)
















wage <-get_cansim("14-10-0063-01")

names(wage)<-str_replace_all(names(wage),
                             c(" " = "_" , 
                               "," = "_", 
                               "[(]" ="_",
                               "[)]"="_"))

get_date(wage,1) 

wage_province <- wage %>%  
  filter(GEO %in% province_vector, 
         Date %in% date_vector) %>% 
  filter(Characteristics=="25 years and over",
         Hours_and_wages=="Full-time employees, average weekly wages") %>% 
  select(GEO, Date,val_norm) %>% 
  rename(wage= val_norm) %>% 
  arrange(GEO,Date)



























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


job_vacancy_monthly  <-get_cansim("14-10-0372-01")

names(job_vacancy_monthly)<-str_replace_all(names(job_vacancy_monthly ),
                                            c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

job_vacancy_monthly  <- 
  job_vacancy_monthly %>% 
  filter(GEO=="Canada",
         North_American_Industry_Classification_System__NAICS_=="Total, all industries",
         Statistics=="Job vacancies") %>% 
  select(Date,
         val_norm) %>% 
  rename(job_vacancy_monthly = val_norm)







# Housing 

# 
# CREA_Data <- read_excel("Not Seasonally Adjusted.xlsx", sheet = "Aggregate")
# 
# CREA_Data <- my_data %>% 
#             mutate(Date2=as.Date(as.numeric(Date),origin="1899-12-30")) %>% 
#             select(Date2,Composite_Benchmark) %>% 
#             rename(Date=Date2) %>% 
#             drop_na(Date)
# 



# rename_column <- function(df) {
#   
#   df <- 
#       df %>% rename_with(.fn=~str_replace_all(names(df), 
#                                               c(" " = "_" ,
#                                                 "," = "_", 
#                                                 "[(]" ="_", 
#                                                 "[)]"="_")))
#   
#   


# }



