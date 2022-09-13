#install.packages("tidyverse")
#install.packages("dplyr")

library(tidyverse)
library(dplyr)
library(lubridate)

#Start by downloading the last 10 yrs of inflation data from here https://data.imf.org/?sk=4FFB52B2-3653-409A-B471-D47B46D904B5, 
#by month and all indicators. Don't go too far back or data is non-existant. Call it imfData.csv


#to do - turn this into an API call

imfData <- read.csv("imfData.csv")


#One CSV for Kenya and Ghana

twelveData <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRuwJON5dg_MZ3ycdgaRFcrVnLBZjnRMGARcPnFZonvE1ug2vnY0cM3Hgh8zE_V9In2HQs9hFEAU6Ni/pub?gid=0&single=true&output=csv")
#codeList <- read.csv("codeList.csv")

# kenyaData <- left_join(kenyaData, codeList, by = "Indicator.Name")
#One CSV for RSA, UGA, ETH, TZA, DZA, AGO, BFA, BDI, TCD

fourteenData <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRuwJON5dg_MZ3ycdgaRFcrVnLBZjnRMGARcPnFZonvE1ug2vnY0cM3Hgh8zE_V9In2HQs9hFEAU6Ni/pub?gid=1551906459&single=true&output=csv")

#ugandaData <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRuwJON5dg_MZ3ycdgaRFcrVnLBZjnRMGARcPnFZonvE1ug2vnY0cM3Hgh8zE_V9In2HQs9hFEAU6Ni/pub?gid=1607745865&single=true&output=csv")

#ethiopiaData <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRuwJON5dg_MZ3ycdgaRFcrVnLBZjnRMGARcPnFZonvE1ug2vnY0cM3Hgh8zE_V9In2HQs9hFEAU6Ni/pub?gid=1693981224&single=true&output=csv")

#tanzaniaData <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRuwJON5dg_MZ3ycdgaRFcrVnLBZjnRMGARcPnFZonvE1ug2vnY0cM3Hgh8zE_V9In2HQs9hFEAU6Ni/pub?gid=1099881620&single=true&output=csv")

#adding food inflation for Ghana, others are available but not scraped yet
#ghanaData <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRuwJON5dg_MZ3ycdgaRFcrVnLBZjnRMGARcPnFZonvE1ug2vnY0cM3Hgh8zE_V9In2HQs9hFEAU6Ni/pub?gid=982017806&single=true&output=csv")

#get the latest country data from Google Sheet
countryList <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR6_bOjxa_F2qgn5eOGo6gpOLXKY9WcsebCvTPD4xVVs2yIL0ABtQx3QKhxfaftTI4jVj85mwaQQ0_K/pub?gid=0&single=true&output=csv")
write_csv(countryList, "countryList.csv")

#Get the ISO to IMF code convertor document
countryCodes <- read.csv("tabula-co.csv")


countryList <- select(countryList, Geography)

africaData <- countryList %>%
                    inner_join(countryCodes, by = c("Geography" = "ISO.Code")) %>%
                    inner_join(imfData, by = c("IMF.Code" = "Country.Code")) %>%
                    filter(Attribute == "Value", grepl("Percentage change, Previous year", Indicator.Name)) %>%
                    select(-c("Common.Reference.Period", "X", "Country.Name", "IMF.Code", "Attribute")) %>%
                   # select(-c("Common.Reference.Period", "Country.Name", "IMF.Code", "Attribute")) %>%
                    mutate(across(starts_with("X"), as.numeric)) %>%
                    mutate(across(starts_with("X"), round, 2)) %>%
                   # filter(Geography != "ZAF" & Geography != "UGA" & Geography != "ETH" & Geography != "TZA" & Geography != "KEN" & Geography != "DZA" & Geography != "AGO") %>%
                    filter(!Geography %in% c("ZAF", "UGA", "ETH", "TZA", "KEN", "DZA", "AGO", "BFA", "BDI", "TCD")) %>%
                    full_join(twelveData) %>%
                    full_join(fourteenData) %>%
#                    full_join(ugandaData) %>%  
#                    full_join(ethiopiaData) %>%
#                    full_join(tanzaniaData) %>%
#                    full_join(ghanaData) %>%
                    arrange(Country) %>%
                    mutate(Indicator.Name = str_remove(Indicator.Name, ", Percentage change, Previous year"))

names(africaData) <- sub("^X", "", names(africaData))

dates <- as.character(ym(names(africaData)[-c(1:4)])+months(1)-days(1))
names(africaData)[-c(1:4)] <- dates


write_csv(africaData, "africaInflationData.csv")

##same process for month on month change

africamonthData <- countryList %>%
  inner_join(countryCodes, by = c("Geography" = "ISO.Code")) %>%
  inner_join(imfData, by = c("IMF.Code" = "Country.Code")) %>%
  filter(Attribute == "Value", grepl("Percentage change, Previous period", Indicator.Name)) %>%
  select(-c("Common.Reference.Period", "X", "Country.Name", "IMF.Code", "Attribute")) %>%
  mutate(across(starts_with("X"), as.numeric)) %>%
  mutate(across(starts_with("X"), round, 2)) %>%
  arrange(Country) %>%
  mutate(Indicator.Name = str_remove(Indicator.Name, ", Percentage change, Previous period"))

names(africamonthData) <- sub("^X", "", names(africamonthData))

dates <- as.character(ym(names(africamonthData)[-c(1:4)])+months(1)-days(1))
names(africamonthData)[-c(1:4)] <- dates


write_csv(africamonthData, "africaInflationDatabymonth.csv")

##Reshaping for Flourish

africaData2 <- africaData %>% select(-c("Geography")) %>%
                              pivot_longer(cols = starts_with("2"), names_to = "Year.Month", values_to = "Change.YoY") %>% 
                              pivot_wider(names_from = Country, values_from = Change.YoY)
                                            
write.csv(africaData2, "africaData2.csv")

africamonthData2 <- africamonthData %>% select(-c("Geography")) %>%
  pivot_longer(cols = starts_with("2"), names_to = "Year.Month", values_to = "Change.YoY") %>% 
  pivot_wider(names_from = Country, values_from = Change.YoY)

write.csv(africamonthData2, "africaData2bymonth.csv")

africaData3 <- africaData %>% select(-c("Indicator.Name")) %>%
  pivot_longer(cols = starts_with("2"), names_to = "Year.Month", values_to = "Change.YoY") %>% 
  pivot_wider(names_from = Indicator.Code, values_from = Change.YoY) %>%
  rename(date = Year.Month, iso_code = Geography)

write.csv(africaData3, "africaData3.csv", row.names = FALSE)
