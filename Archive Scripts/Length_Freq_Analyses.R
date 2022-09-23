
# Background --------------------------------------------------------------

# Examining legth frequences to determine what months and length cuttoffs to use to create age-0 abundance indicies for SASU, SAPM, SPLT




# Packages ----------------------------------------------------------------
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)
options(scipen=999)
windowsFonts(Times = windowsFont("Times New Roman"))



# Data --------------------------------------------------------------------
setwd("C:\\Users\\rmckenzie\\Desktop\\Native Cypriniform Study\\Data_Analysis")

Cyprinid_Catch = read.csv("data/Cyprinid_Catch.CSV")
head(Cyprinid_Catch)
str(Cyprinid_Catch)

# Changing to factors
Cyprinid_Catch$Month = as.factor(Cyprinid_Catch$Month)
Cyprinid_Catch$Year = as.factor(Cyprinid_Catch$Year)
Cyprinid_Catch$RegionCode = as.factor(Cyprinid_Catch$RegionCode)
Cyprinid_Catch$subarea = as.factor(Cyprinid_Catch$subarea)


# SAPM --------------------------------------------------------------------
SAPM_Catch_By_Month <- 
  Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", ForkLength < 200)%>%
  group_by(Month)%>% 
  summarise(Sum_Catch_Per_Month = sum(SumOfCatchCount))

# Highest catches in months 3 - 7

SAPM_Catch_By_Month <- 
  Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", ForkLength < 200, Month == 3:7)
 
# Length frequency analyses

# March
Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", ForkLength, Month == 3) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  theme_classic(base_size = 15)

# April
Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", Month == 4) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  theme_classic(base_size = 15)

# May
Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", Month == 5) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  theme_classic(base_size = 15)

# June
Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", Month == 6) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  theme_classic(base_size = 15)

# July
Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", Month == 7) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  theme_classic(base_size = 15)
