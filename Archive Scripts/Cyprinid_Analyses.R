library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)

setwd("M:/DJFMP Not IEP/Native Cypriniform Study/Data_Analyses")

Cyprinid_Catch = read.csv("Cyprinid_Catch.CSV")
head(Cyprinid_Catch)
str(Cyprinid_Catch)

# Changing to factors
Cyprinid_Catch$Month = as.factor(Cyprinid_Catch$Month)
Cyprinid_Catch$Year = as.factor(Cyprinid_Catch$Year)
Cyprinid_Catch$RegionCode = as.factor(Cyprinid_Catch$RegionCode)
Cyprinid_Catch$subarea = as.factor(Cyprinid_Catch$subarea)

# Replacing NAs with 0s
Cyprinid_Catch[is.na(Cyprinid_Catch)]<-0
head(Cyprinid_Catch, 50)

# Length cuttoff at 100 mm for age 0 fish
Cyprinid_Catch = Cyprinid_Catch[Cyprinid_Catch$ForkLength < 101,]

## SASU
SASU_Catch = Cyprinid_Catch[Cyprinid_Catch$OrganismCode == "SASU",]
head(SASU_Catch)

SASU_Month_Agg = aggregate(SumOfCatchCount ~ Month, data = SASU_Catch, sum)
SASU_Month_Agg
# Highest catches in months 4-7; peak in 5 and 6

SASU_Region_Agg = aggregate(SumOfCatchCount ~ RegionCode, data = SASU_Catch, sum)
SASU_Region_Agg
# Highest catches in 1 and 4


## SPLT
SPLT_Catch = Cyprinid_Catch[Cyprinid_Catch$OrganismCode == "SPLT",]
head(SPLT_Catch)

SPLT_Month_Agg = aggregate(SumOfCatchCount ~ Month, data = SPLT_Catch, sum)
SPLT_Month_Agg
# Highest catches in months 5-6

SPLT_Region_Agg = aggregate(SumOfCatchCount ~ RegionCode, data = SPLT_Catch, sum)
SPLT_Region_Agg
# High catches in all regions

## SAPM 
SAPM_Catch = Cyprinid_Catch[Cyprinid_Catch$OrganismCode == "SAPM",]
head(SAPM_Catch)

SAPM_Month_Agg = aggregate(SumOfCatchCount ~ Month, data = SAPM_Catch[SAPM_Catch$ForkLength < 200,], sum)
SAPM_Month_Agg
# Highest catches in months 3 - 7

SAPM_Region_Agg = aggregate(SumOfCatchCount ~ RegionCode, data = SAPM_Catch, sum)
SAPM_Region_Agg
# High catches in 1 and 2