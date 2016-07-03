library(dplyr)
library(tidyr)
library(ggplot2)

### Download and open refugee data
temp1 <- tempfile()
download.file("http://api.worldbank.org/v2/en/indicator/SM.POP.REFG?downloadformat=csv",temp1)
filenames1 <- unzip(temp1,list = T)
filenames1
refugeeData <- tbl_df(read.csv(unzip(temp1, filenames1[2,1]),skip = 4))
countryData <- tbl_df(read.csv(unzip(temp1, filenames1[3,1]),skip = 0))

### Download and open population data
temp2 <- tempfile()
download.file("http://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=csv", temp2)
filenames2 <- unzip(temp2, list = T)
filenames2
popnData <- tbl_df(read.csv(unzip(temp2, filenames2[2,1]),skip = 4))

### Download and open gdp data
temp3 <- tempfile()
download.file("http://api.worldbank.org/v2/en/indicator/NY.GDP.PCAP.PP.CD?downloadformat=csv", temp3)
filenames3 <- unzip(temp3, list = T)
filenames3
gdpData <- tbl_df(read.csv(unzip(temp3, filenames3[2,1]),skip = 4))

### 'Tidy' and merge data
popnData <- popnData %>% select(2, 35:59) %>% gather("year","popn",2:26)
popnData$year <- gsub("X","", popnData$year)

gdpData <- gdpData %>% select(2, 35:59) %>% gather("year","gdp",2:26)
gdpData$year <- gsub("X","", gdpData$year)

refugeeData <- left_join(refugeeData, countryData, "Country.Code")
refugeeData <- select(refugeeData, -(3:34), -(60:61), -(64:66))
colnames(refugeeData) <- gsub("X","",colnames(refugeeData))

refugeeData <- gather(refugeeData, "year", "numberRefs", 3:27)

fullData <- left_join(refugeeData, popnData)
fullData <- left_join(fullData, gdpData)

write.csv(fullData, "./data/refugeeData.csv")

rm(list = ls())
