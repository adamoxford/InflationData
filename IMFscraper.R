#install.packages("tidyverse")
#install.packages("dplyr")

library(tidyverse)
library(dplyr)

#Start by downloading the last 10 yrs of inflation data from here https://data.imf.org/?sk=4FFB52B2-3653-409A-B471-D47B46D904B5, 
#by month and all indicators. Don't go too far back or data is non-existant. Call it imfData.csv
#to do - turn this into an API call
#to do - Add ISO country codes to IMF list (see PDF in working directory)

imfData <- read.csv("imfData.csv")
countryList <- read.csv("countryList.csv")

countryList <- select(countryList, Region.Country)

africaData <- countryList %>%
                    inner_join(imfData, by = c("Region.Country" = "Country.Name")) %>%
                    filter(Attribute == "Value", grepl("Percentage change, Previous year", Indicator.Name)) %>%
                    select(-c("Common.Reference.Period", "X"))

names(africaData) <- sub("^X", "", names(africaData))

write_csv(africaData, "africaInflationData.csv")