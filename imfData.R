#install.packages("tidyverse")
#install.packages("dplyr")

library(tidyverse)
library(dplyr)

#Start by downloading the last 10 yrs of inflation data from here https://data.imf.org/?sk=4FFB52B2-3653-409A-B471-D47B46D904B5, 
#by month and all indicators. Don't go too far back or data is non-existant. Call it imfData.csv
#to do - turn this into an API call

imfData <- read.csv("imfData.csv")
countryList <- read.csv("countryList.csv")
countryCodes <- read.csv("tabula-co.csv")


countryList <- select(countryList, Geography)

africaData <- countryList %>%
                    inner_join(countryCodes, by = c("Geography" = "ISO.Code")) %>%
                    inner_join(imfData, by = c("IMF.Code" = "Country.Code")) %>%
                    filter(Attribute == "Value", grepl("Percentage change, Previous year", Indicator.Name)) %>%
                    select(-c("Common.Reference.Period", "X", "Country.Name")) %>%
                    mutate(across(starts_with("X"), as.numeric)) %>%
                    mutate(across(starts_with("X"), round, 2))




names(africaData) <- sub("^X", "", names(africaData))

write_csv(africaData, "africaInflationData.csv")