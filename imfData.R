#install.packages("tidyverse")
#install.packages("dplyr")

library(tidyverse)
library(dplyr)
library(lubridate)

#Start by downloading the last 10 yrs of inflation data from here https://data.imf.org/?sk=4FFB52B2-3653-409A-B471-D47B46D904B5, 
#by month and all indicators. Don't go too far back or data is non-existant. Call it imfData.csv


#to do - turn this into an API call

imfData <- read.csv("imfData.csv")

#Kenya's data is a manual PDF scrape at the moment, updated on a Google Sheet in the ADH drive

kenyaData <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRuwJON5dg_MZ3ycdgaRFcrVnLBZjnRMGARcPnFZonvE1ug2vnY0cM3Hgh8zE_V9In2HQs9hFEAU6Ni/pub?gid=0&single=true&output=csv")
codeList <- read.csv("codeList.csv")

kenyaData <- left_join(kenyaData, codeList, by = "Indicator.Name")

#get the latest country data from Google Sheet
countryList <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR6_bOjxa_F2qgn5eOGo6gpOLXKY9WcsebCvTPD4xVVs2yIL0ABtQx3QKhxfaftTI4jVj85mwaQQ0_K/pub?gid=0&single=true&output=csv")
write_csv(countryList, "countryList.csv")

#Get the ISO to IMF code convertor documnet
countryCodes <- read.csv("tabula-co.csv")



countryList <- select(countryList, Geography)

africaData <- countryList %>%
                    inner_join(countryCodes, by = c("Geography" = "ISO.Code")) %>%
                    inner_join(imfData, by = c("IMF.Code" = "Country.Code")) %>%
                    filter(Attribute == "Value", grepl("Percentage change, Previous year", Indicator.Name)) %>%
                    select(-c("Common.Reference.Period", "X", "Country.Name", "IMF.Code", "Attribute")) %>%
                    mutate(across(starts_with("X"), as.numeric)) %>%
                    mutate(across(starts_with("X"), round, 2)) %>%
                    full_join(kenyaData) %>%
                    arrange(Country)

names(africaData) <- sub("^X", "", names(africaData))

dates <- as.character(ym(names(africaData)[-c(1:4)])+months(1)-days(1))
names(africaData)[-c(1:4)] <- dates


write_csv(africaData, "africaInflationData.csv")
