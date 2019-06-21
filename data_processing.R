# career_analysis.R
library(readr)
library(dplyr)
library(stringr)
inc5000_2018 <- read_csv("~/Google Drive/Google Drive Personal/College/Resume/Company-Specific/Slalom/Slalom_Data_Challenge/inc5000-2018.csv", 
                         col_types = cols(`_ - partner_lists - partner_lists` = col_skip()))
colnames(inc5000_2018) <- c("ID","URL","Rank","City","Metro_Code","Industry_Code","Growth","Workers","Company","Website","State Long","State Short", "Revenue", "ZIP", "Founded", "Industry", "Latitude", "Metro_Code_Str", "Longitude", "Years_on_list", "Previous_Workers", "Metro_Name")

nrow(inc5000_2018)
any(!complete.cases(inc5000_2018)) # we have some missing data/ empty rows

inc5000_data <- inc5000_2018[!is.na(inc5000_2018$ID),] # only keep rows that have an ID
nrow(inc5000_data)
sum(inc5000_data$Revenue)

# total number of jobs
284499 / sum(inc5000_data$Workers)

sum(inc5000_data$Workers)
  # import income data by NAICS code
NAICS_data <- read_csv("~/Google Drive/Google Drive Personal/College/Resume/Company-Specific/Slalom/Slalom_Data_Challenge/NAICS_data.csv")
NAICS_data$`Top Level Code` <- as.character(NAICS_data$`Top Level Code`)

# import NAICS lookup table
NAICS_lookup <- read_csv("~/Google Drive/Google Drive Personal/College/Resume/Company-Specific/Slalom/Slalom_Data_Challenge/NAICS_lookup.csv", 
                         col_types = cols(NAICS_Code = col_character()))
joined_data <- inc5000_2018 %>% 
  left_join(NAICS_lookup, by = c("Industry" = "INC_5000_Industry")) %>%
  left_join(NAICS_data, by = c("NAICS_Code" = "Top Level Code"))

# add data about household rents
housing_costs <- read_csv("~/Google Drive/Google Drive Personal/College/Resume/Company-Specific/Slalom/Slalom_Data_Challenge/ACS_ZIP.csv", 
                          col_types = cols(Geography = col_character()))

housing_costs <- housing_costs %>% select(Geography,'Estimate; Median gross rent -- - Total: - 1 bedroom') %>% rename(One_Bed_Rent = 'Estimate; Median gross rent -- - Total: - 1 bedroom', ZIP = Geography) %>% mutate(ZIP = substr(ZIP, 7, 12))
housing_costs$One_Bed_Rent <- gsub("[^0-9]", "", housing_costs$One_Bed_Rent) 
housing_costs <- housing_costs[!(housing_costs$ZIP == ""),] # only keep ZIPs that have a rent
housing_costs <- housing_costs[!(housing_costs$One_Bed_Rent == ""),] # same for rent
housing_costs$One_Bed_Rent <- as.integer(housing_costs$One_Bed_Rent)

joined_data_na_rm <- joined_data[!is.na(joined_data$Metro_Code_Str),] # only keep rows that have a Metro_Code_Str
joined_data_na_rm$ZIP <- str_pad(as.character(joined_data_na_rm$ZIP), 5, pad = "0")

jobs_with_housing <- joined_data_na_rm %>%
  left_join(housing_costs, by = 'ZIP')

# fill in the ZIPs without rent with the average
library(tidyr)
jobs_with_housing$One_Bed_Rent <- as.integer(jobs_with_housing$One_Bed_Rent)
jobs_with_housing$One_Bed_Rent <- replace_na(jobs_with_housing$One_Bed_Rent,mean(jobs_with_housing$One_Bed_Rent,na.rm=T))

# find monthly earnings (assuming full-time work)
jobs_with_housing$monthly_earnings <- jobs_with_housing$Mean_hourly_wage * 40 * 4
jobs_with_housing$rent_as_pct_earnings <- jobs_with_housing$One_Bed_Rent / jobs_with_housing$monthly_earnings
summary(jobs_with_housing$rent_as_pct_earnings)
jobs_with_housing$affordable <- ifelse(jobs_with_housing$rent_as_pct_earnings > .33, FALSE, TRUE)

final_enriched_data <- jobs_with_housing %>% select(ID, Growth, Workers, Company, City, Metro_Code_Str, Revenue, ZIP, Industry, Previous_Workers, NAICS_Code, Occupation, Employment, Mean_hourly_wage, One_Bed_Rent, monthly_earnings, rent_as_pct_earnings, affordable)

write_csv(final_enriched_data, "final_enriched_data.csv")
