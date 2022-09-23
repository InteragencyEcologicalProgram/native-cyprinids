
# Background --------------------------------------------------------------

# Study is trying to determine the influence of biotic and abiotic factors on the age-0 abundanece indicies of SPLT, SASU, SAPM

# Length Frequency: Staring with legth frequency analyses to determine what months and length cutoffs to use when creating age-0 abundance indices 

# Plus Count proportions: # Use FL of Age-0 fishes caught within a seine haul to proporiton FL of plus count fish

# Age-0 Indicies: Using plus count proportioned data to create age-0 abundance indices


# Packages ----------------------------------------------------------------
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)
library(psych)
library(MuMIn)
options(scipen=999)
windowsFonts(Times = windowsFont("Times New Roman"))



# Data --------------------------------------------------------------------
setwd("M:/DJFMP Not IEP/Native Cypriniform Study/Data_Analyses")

Cyprinid_Catch = read.csv("data/Cyprinid_Catch.CSV")
Cyprinid_Catch$Month = as.factor(Cyprinid_Catch$Month)
Cyprinid_Catch$Year = as.factor(Cyprinid_Catch$Year)
Cyprinid_Catch$RegionCode = as.factor(Cyprinid_Catch$RegionCode)
Cyprinid_Catch$subarea = as.factor(Cyprinid_Catch$subarea)

# SAPM
SAPM_Catch_Effort = read.csv("data/SAPM_Catch_Effort.csv")
SAPM_Catch_Effort$SampleID = as.factor(SAPM_Catch_Effort$SampleID)
SAPM_Catch_Effort$Month = as.factor(SAPM_Catch_Effort$Month)
SAPM_Catch_Effort$subarea = as.factor(SAPM_Catch_Effort$subarea)
SAPM_Catch_Effort$Year = as.factor(SAPM_Catch_Effort$Year)
SAPM_Catch_Effort$GearConditionCode = as.factor(SAPM_Catch_Effort$GearConditionCode)
str(SAPM_Catch_Effort)

# SPLT
SPLT_Catch_Effort = read.csv("data/SPLT_Catch_Effort.csv")
SPLT_Catch_Effort$SampleID = as.factor(SPLT_Catch_Effort$SampleID)
SPLT_Catch_Effort$Month = as.factor(SPLT_Catch_Effort$Month)
SPLT_Catch_Effort$subarea = as.factor(SPLT_Catch_Effort$subarea)
SPLT_Catch_Effort$Year = as.factor(SPLT_Catch_Effort$Year)
SPLT_Catch_Effort$GearConditionCode = as.factor(SPLT_Catch_Effort$GearConditionCode)
str(SPLT_Catch_Effort)
head(SPLT_Catch_Effort, 10)

# SASU
SASU_Catch_Effort = read.csv("data/SASU_Catch_Effort.csv")
SASU_Catch_Effort$SampleID = as.factor(SASU_Catch_Effort$SampleID)
SASU_Catch_Effort$Month = as.factor(SASU_Catch_Effort$Month)
SASU_Catch_Effort$subarea = as.factor(SASU_Catch_Effort$subarea)
SASU_Catch_Effort$Year = as.factor(SASU_Catch_Effort$Year)
SASU_Catch_Effort$GearConditionCode = as.factor(SASU_Catch_Effort$GearConditionCode)

# Discharge
Discharge = read.csv("data/dayflow_1994_2017.csv")
head(Discharge)
str(Discharge)
Discharge$Year = as.factor(Discharge$Year)
Discharge$Mo = as.factor(Discharge$Mo)
head(Discharge)


# Zooplankton SPLT
#### Zooplankton Data
Zoop_Raw = read.csv("data/1972-2017CBMatrix.csv", header = T)
head(Zoop_Raw)
str(Zoop_Raw)
Zoop_Raw$Month = as.factor(month(as.POSIXlt(Zoop_Raw$Date, format = "%m/%d/%Y")))

# Length Frequency Analyses -----------------------------------------------

# Creating object to label x axis to better see length cutoffs for age-0 fish
FL_Labels = seq(0, 200, by = 10)


# SAPM --------------------------------------------------------------------

# To see what months have highest catches of SAPM
SAPM_Catch_By_Month <- 
  Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", ForkLength < 200)%>%
  group_by(Month)%>% 
  summarise(Sum_Catch_Per_Month = sum(SumOfCatchCount))

# Highest catches in months 3 - 7

 
# Length frequency analyses

# March
Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", ForkLength, Month == 3, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# April
Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", Month == 4, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# May
Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", Month == 5, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# June
Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", Month == 6, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# July
Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", Month == 7, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# Based on length freq histograms, age 0 fish don't start showing up until June and July
# June cuttoff for age-0 fish is ~ 60 mm
# July cuttoff for age-0 fish is ~ 70 mm


# SPLT --------------------------------------------------------------------

# To see what months have highest catches of SAPM
SPLT_Catch_By_Month <- 
  Cyprinid_Catch %>% 
  filter(OrganismCode == "SPLT", ForkLength < 200)%>%
  group_by(Month)%>% 
  summarise(Sum_Catch_Per_Month = sum(SumOfCatchCount))

# Highest catches in months 5 - 6


# Length frequency analyses

# May
Cyprinid_Catch %>% 
  filter(OrganismCode == "SPLT", ForkLength, Month == 5, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# June
Cyprinid_Catch %>% 
  filter(OrganismCode == "SPLT", ForkLength, Month == 6, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# Based on length freq histograms, age 0 fish make up the majority of the catch during both months
# May cuttoff for age-0 fish is ~ 50 mm
# June cuttoff for age-0 fish is ~ 60 mm


# SASU --------------------------------------------------------------------

# To see what months have highest catches of SAPM
SASU_Catch_By_Month <- 
  Cyprinid_Catch %>% 
  filter(OrganismCode == "SASU", ForkLength < 200)%>%
  group_by(Month)%>% 
  summarise(Sum_Catch_Per_Month = sum(SumOfCatchCount))

# Highest catches in months 4 - 7; Peak in 5 and 6


# Length frequency analyses

# April
Cyprinid_Catch %>% 
  filter(OrganismCode == "SASU", ForkLength, Month == 4, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# May
Cyprinid_Catch %>% 
  filter(OrganismCode == "SASU", ForkLength, Month == 5, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# June
Cyprinid_Catch %>% 
  filter(OrganismCode == "SASU", ForkLength, Month == 6, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# July
Cyprinid_Catch %>% 
  filter(OrganismCode == "SASU", ForkLength, Month == 7, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# Going to use months 5 and 6 because they have the highest catches
# Based on length freq histograms, age 0 fish make up the majority of the catch during both months
# May cuttoff for age-0 fish is ~ 50 mm
# June cuttoff for age-0 fish is ~ 65 mm


# Plus Count Proportions --------------------------------------------------

# I will be using FL of Age-0 fishes caught within a seine haul to proporiton FL of plus count fish
# Formula I am using is  
# Adjusted Count =  Total_Count * (FL_Range_Count/Measured_Count)
# Total_count = All fish in one sample
# FL_Range_Count = Count of fish in the FL range that we are using 
# Measured_Count = Count of measured fish


# FL Count = SumofCatchCount for rows where ForkLength > 0
# Plus Count = SumofCatchCount for rows where ForkLength = 0
# Total Count = sum of FL Count and Plus Count

# CPUE = Adjusted Cout/Seine Volume


# SAPM  -------------------------------------------------------------------

# Based on length freq histograms, age 0 fish don't start showing up until June and July
# June cuttoff for age-0 fish is ~ 60 mm
# July cuttoff for age-0 fish is ~ 70 mm

## June
SAPM_Plus_Count_June <- 
  SAPM_Catch_Effort %>% 
  filter(OrganismCode == "SAPM", ForkLength == 0, Month == 6)%>%
  group_by(SampleID)%>% 
  summarise(Plus_Count = sum(SumOfSumOfCatchCount))

SAPM_FL_Count_June <- 
  SAPM_Catch_Effort %>% 
  filter(OrganismCode == "SAPM", ForkLength > 0, Month == 6)%>%
  group_by(SampleID)%>% 
  summarise(FL_Count = sum(SumOfSumOfCatchCount))

SAPM_Total_June <- 
  full_join(SAPM_FL_Count_June, SAPM_Plus_Count_June,  by = "SampleID")
  colnames(SAPM_Total_June) = c("SampleID", "FL_Count", "Plus_Count") 
  SAPM_Total_June[is.na(SAPM_Total_June)] = 0  
  SAPM_Total_June$Total_Count = SAPM_Total_June$FL_Count + SAPM_Total_June$Plus_Count

SAPM_FL_Range_June <- 
  SAPM_Catch_Effort %>% 
    filter(OrganismCode == "SAPM", ForkLength > 24, ForkLength <61, Month == 6)%>%
    group_by(SampleID)%>% 
    summarise(FL_Range = sum(SumOfSumOfCatchCount))

SAPM_Total_June_Final = left_join(SAPM_Total_June, SAPM_FL_Range_June, by = "SampleID")
colnames(SAPM_Total_June_Final) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
SAPM_Total_June_Final[is.na(SAPM_Total_June_Final)] = 0 
head(SAPM_Total_June_Final)

## July
SAPM_Plus_Count_July <- 
  SAPM_Catch_Effort %>% 
  filter(OrganismCode == "SAPM", ForkLength == 0, Month == 7)%>%
  group_by(SampleID)%>% 
  summarise(Plus_Count = sum(SumOfSumOfCatchCount))

SAPM_FL_Count_July <- 
  SAPM_Catch_Effort %>% 
  filter(OrganismCode == "SAPM", ForkLength > 0, Month == 7)%>%
  group_by(SampleID)%>% 
  summarise(FL_Count = sum(SumOfSumOfCatchCount))

SAPM_Total_July <- 
  full_join(SAPM_FL_Count_July, SAPM_Plus_Count_July,  by = "SampleID")
colnames(SAPM_Total_July) = c("SampleID", "FL_Count", "Plus_Count") 
SAPM_Total_July[is.na(SAPM_Total_July)] = 0  
SAPM_Total_July$Total_Count = SAPM_Total_July$FL_Count + SAPM_Total_July$Plus_Count

SAPM_FL_Range_July <- 
  SAPM_Catch_Effort %>% 
  filter(OrganismCode == "SAPM", ForkLength > 24, ForkLength <71, Month == 7)%>%
  group_by(SampleID)%>% 
  summarise(FL_Range = sum(SumOfSumOfCatchCount))

SAPM_Total_July_Final = left_join(SAPM_Total_July, SAPM_FL_Range_July, by = "SampleID")
colnames(SAPM_Total_July_Final) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
SAPM_Total_July_Final[is.na(SAPM_Total_July_Final)] = 0 
head(SAPM_Total_July_Final)

## Combining all months
SAPM_Final <- 
  bind_rows(SAPM_Total_June_Final, SAPM_Total_July_Final)
str(SAPM_Final)

SAPM_Final$Adjusted_Count <-
  SAPM_Final$Total_Count * (SAPM_Final$FL_Range_Count/SAPM_Final$FL_Count)
SAPM_Final[is.na(SAPM_Final)] = 0

SAPM_Index = full_join(subset(SAPM_Catch_Effort, !duplicated(SampleID)), SAPM_Final, by = "SampleID")
head(SAPM_Index)
SAPM_Index[,15:21][is.na(SAPM_Index[,15:21])] = 0
head(SAPM_Index)

SAPM_Index <- 
  SAPM_Index[SAPM_Index$Month %in% c(6:7),]
head(SAPM_Index,15)

SAPM_Index$CPUE <- 
  SAPM_Index$Adjusted_Count/SAPM_Index$SeineVolume
head(SAPM_Index)



# SPLT --------------------------------------------------------------------
# Based on length freq histograms, age 0 fish make up the majority of the catch during both months
# May cuttoff for age-0 fish is ~ 50 mm
# June cuttoff for age-0 fish is ~ 60 mm

## May
SPLT_Plus_Count_May <- 
  SPLT_Catch_Effort %>% 
  filter(OrganismCode == "SPLT", ForkLength == 0, Month == 5)%>%
  group_by(SampleID)%>% 
  summarise(Plus_Count = sum(SumOfSumOfCatchCount))

SPLT_FL_Count_May <- 
  SPLT_Catch_Effort %>% 
  filter(OrganismCode == "SPLT", ForkLength > 0, Month == 5)%>%
  group_by(SampleID)%>% 
  summarise(FL_Count = sum(SumOfSumOfCatchCount))

SPLT_Total_May <- 
  full_join(SPLT_FL_Count_May, SPLT_Plus_Count_May,  by = "SampleID")
colnames(SPLT_Total_May) = c("SampleID", "FL_Count", "Plus_Count") 
SPLT_Total_May[is.na(SPLT_Total_May)] = 0  
SPLT_Total_May$Total_Count = SPLT_Total_May$FL_Count + SPLT_Total_May$Plus_Count

SPLT_FL_Range_May <- 
  SPLT_Catch_Effort %>% 
  filter(OrganismCode == "SPLT", ForkLength > 24, ForkLength <51, Month == 5)%>%
  group_by(SampleID)%>% 
  summarise(FL_Range = sum(SumOfSumOfCatchCount))

SPLT_Total_May_Final = left_join(SPLT_Total_May, SPLT_FL_Range_May, by = "SampleID")
colnames(SPLT_Total_May_Final) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
SPLT_Total_May_Final[is.na(SPLT_Total_May_Final)] = 0 
head(SPLT_Total_May_Final)

## June
SPLT_Plus_Count_June <- 
  SPLT_Catch_Effort %>% 
  filter(OrganismCode == "SPLT", ForkLength == 0, Month == 6)%>%
  group_by(SampleID)%>% 
  summarise(Plus_Count = sum(SumOfSumOfCatchCount))

SPLT_FL_Count_June <- 
  SPLT_Catch_Effort %>% 
  filter(OrganismCode == "SPLT", ForkLength > 0, Month == 6)%>%
  group_by(SampleID)%>% 
  summarise(FL_Count = sum(SumOfSumOfCatchCount))

SPLT_Total_June <- 
  full_join(SPLT_FL_Count_June, SPLT_Plus_Count_June,  by = "SampleID")
colnames(SPLT_Total_June) = c("SampleID", "FL_Count", "Plus_Count") 
SPLT_Total_June[is.na(SPLT_Total_June)] = 0  
SPLT_Total_June$Total_Count = SPLT_Total_June$FL_Count + SPLT_Total_June$Plus_Count

SPLT_FL_Range_June <- 
  SPLT_Catch_Effort %>% 
  filter(OrganismCode == "SPLT", ForkLength > 24, ForkLength <61, Month == 6)%>%
  group_by(SampleID)%>% 
  summarise(FL_Range = sum(SumOfSumOfCatchCount))

SPLT_Total_June_Final = left_join(SPLT_Total_June, SPLT_FL_Range_June, by = "SampleID")
colnames(SPLT_Total_June_Final) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
SPLT_Total_June_Final[is.na(SPLT_Total_June_Final)] = 0 
head(SPLT_Total_June_Final)

## Combining all months
SPLT_Final <- 
  bind_rows(SPLT_Total_May_Final, SPLT_Total_June_Final)
str(SPLT_Final)

SPLT_Final$Adjusted_Count <-
  SPLT_Final$Total_Count * (SPLT_Final$FL_Range_Count/SPLT_Final$FL_Count)
SPLT_Final[is.na(SPLT_Final)] = 0

SPLT_Index = full_join(subset(SPLT_Catch_Effort, !duplicated(SampleID)), SPLT_Final, by = "SampleID")
head(SPLT_Index)
SPLT_Index[,15:21][is.na(SPLT_Index[,15:21])] = 0
head(SPLT_Index)

SPLT_Index <- 
  SPLT_Index[SPLT_Index$Month %in% c(5:6),]
head(SPLT_Index,15)

SPLT_Index$CPUE <- 
  SPLT_Index$Adjusted_Count/SPLT_Index$SeineVolume
head(SPLT_Index)


# SASU --------------------------------------------------------------------
# Going to use months 5 and 6 because they have the highest catches
# Based on length freq histograms, age 0 fish make up the majority of the catch during both months
# May cuttoff for age-0 fish is ~ 50 mm
# June cuttoff for age-0 fish is ~ 65 mm

## May
SASU_Plus_Count_May <- 
  SASU_Catch_Effort %>% 
  filter(OrganismCode == "SASU", ForkLength == 0, Month == 5)%>%
  group_by(SampleID)%>% 
  summarise(Plus_Count = sum(SumOfSumOfCatchCount))

SASU_FL_Count_May <- 
  SASU_Catch_Effort %>% 
  filter(OrganismCode == "SASU", ForkLength > 0, Month == 5)%>%
  group_by(SampleID)%>% 
  summarise(FL_Count = sum(SumOfSumOfCatchCount))

SASU_Total_May <- 
  full_join(SASU_FL_Count_May, SASU_Plus_Count_May,  by = "SampleID")
colnames(SASU_Total_May) = c("SampleID", "FL_Count", "Plus_Count") 
SASU_Total_May[is.na(SASU_Total_May)] = 0  
SASU_Total_May$Total_Count = SASU_Total_May$FL_Count + SASU_Total_May$Plus_Count

SASU_FL_Range_May <- 
  SASU_Catch_Effort %>% 
  filter(OrganismCode == "SASU", ForkLength > 24, ForkLength <51, Month == 5)%>%
  group_by(SampleID)%>% 
  summarise(FL_Range = sum(SumOfSumOfCatchCount))

SASU_Total_May_Final = left_join(SASU_Total_May, SASU_FL_Range_May, by = "SampleID")
colnames(SASU_Total_May_Final) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
SASU_Total_May_Final[is.na(SASU_Total_May_Final)] = 0 
head(SASU_Total_May_Final)

## June
SASU_Plus_Count_June <- 
  SASU_Catch_Effort %>% 
  filter(OrganismCode == "SASU", ForkLength == 0, Month == 6)%>%
  group_by(SampleID)%>% 
  summarise(Plus_Count = sum(SumOfSumOfCatchCount))

SASU_FL_Count_June <- 
  SASU_Catch_Effort %>% 
  filter(OrganismCode == "SASU", ForkLength > 0, Month == 6)%>%
  group_by(SampleID)%>% 
  summarise(FL_Count = sum(SumOfSumOfCatchCount))

SASU_Total_June <- 
  full_join(SASU_FL_Count_June, SASU_Plus_Count_June,  by = "SampleID")
colnames(SASU_Total_June) = c("SampleID", "FL_Count", "Plus_Count") 
SASU_Total_June[is.na(SASU_Total_June)] = 0  
SASU_Total_June$Total_Count = SASU_Total_June$FL_Count + SASU_Total_June$Plus_Count

SASU_FL_Range_June <- 
  SASU_Catch_Effort %>% 
  filter(OrganismCode == "SASU", ForkLength > 24, ForkLength <66, Month == 6)%>%
  group_by(SampleID)%>% 
  summarise(FL_Range = sum(SumOfSumOfCatchCount))

SASU_Total_June_Final = left_join(SASU_Total_June, SASU_FL_Range_June, by = "SampleID")
colnames(SASU_Total_June_Final) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
SASU_Total_June_Final[is.na(SASU_Total_June_Final)] = 0 
head(SASU_Total_June_Final)

## Combining all months
SASU_Final <- 
  bind_rows(SASU_Total_May_Final, SASU_Total_June_Final)
str(SASU_Final)

SASU_Final$Adjusted_Count <-
  SASU_Final$Total_Count * (SASU_Final$FL_Range_Count/SASU_Final$FL_Count)
SASU_Final[is.na(SASU_Final)] = 0

SASU_Index = full_join(subset(SASU_Catch_Effort, !duplicated(SampleID)), SASU_Final, by = "SampleID")
head(SASU_Index)
SASU_Index[,15:21][is.na(SASU_Index[,15:21])] = 0
head(SASU_Index)

SASU_Index <- 
  SASU_Index[SASU_Index$Month %in% c(5:6),]
head(SASU_Index,15)

SASU_Index$CPUE <- 
  SASU_Index$Adjusted_Count/SASU_Index$SeineVolume
head(SASU_Index)

# Heat Maps ---------------------------------------------------------------

# Looking at CPUEs by site to see how catches are distributed


# SAPM --------------------------------------------------------------------

SAPM_HM <- 
  SAPM_Index %>% 
  group_by(Year, StationCode) %>% 
  summarise(MeanCPUE = mean(CPUE))
head(SAPM_HM)
str(SAPM_HM)

# Heat maps for stations
SAPM_Heat_Map_Plot <- 
  ggplot(SAPM_HM, aes(Year, StationCode)) +
  geom_tile(aes(fill = MeanCPUE), color = "white") +
  ggtitle("SAPM") +
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_fill_gradient(low = "white", high = "steelblue")
SAPM_Heat_Map_Plot

# Output for SAPM Station HM plots
png(file = "Figures/SAPM_Station_HM.png", width = 1500, height = 1850, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SAPM_Heat_Map_Plot, ncol = 1)))         

dev.off()

# Heat maps for subareas
SAPM_Subarea_HM <- 
  SAPM_Index %>% 
  group_by(Year, subarea) %>% 
  summarise(MeanCPUE = mean(CPUE))
head(SAPM_Subarea_HM)
str(SAPM_Subarea_HM)

SAPM_Subarea_Heat_Map_Plot <- 
  ggplot(SAPM_Subarea_HM, aes(Year, subarea)) +
  geom_tile(aes(fill = MeanCPUE), color = "white") +
  ggtitle("SAPM") +
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_fill_gradient(low = "white", high = "steelblue")
SAPM_Subarea_Heat_Map_Plot

# Subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River
# Most catches occured in Sac region with very little catch in SJ
# Some catch in Delta region, espcially in subarea 5 

# Output for SAPM Subarea HM plots
png(file = "Figures/SAPM_Subarea_HM.png", width = 1500, height = 1850, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SAPM_Subarea_Heat_Map_Plot, ncol = 1)))         

dev.off()

# SPLT --------------------------------------------------------------------
SPLT_HM <- 
  SPLT_Index %>% 
  group_by(Year, StationCode) %>% 
  summarise(MeanCPUE = mean(CPUE))
head(SPLT_HM)
str(SPLT_HM)

# Heat maps for stations
SPLT_Heat_Map_Plot <- 
  ggplot(SPLT_HM, aes(Year, StationCode)) +
  geom_tile(aes(fill = MeanCPUE), color = "white") +
  ggtitle("SPLT") +
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_fill_gradient(low = "white", high = "steelblue")
SPLT_Heat_Map_Plot

# Output for SPLT HM plots
png(file = "Figures/SPLT_Station_HM.png", width = 1500, height = 1850, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SPLT_Heat_Map_Plot, ncol = 1)))         

dev.off()

# Heat maps for subareas
SPLT_Subarea_HM <- 
  SPLT_Index %>% 
  group_by(Year, subarea) %>% 
  summarise(MeanCPUE = mean(CPUE))
head(SPLT_Subarea_HM)
str(SPLT_Subarea_HM)

SPLT_Subarea_Heat_Map_Plot <- 
  ggplot(SPLT_Subarea_HM, aes(Year, subarea)) +
  geom_tile(aes(fill = MeanCPUE), color = "white") +
  ggtitle("SPLT") +
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_fill_gradient(low = "white", high = "steelblue")
SPLT_Subarea_Heat_Map_Plot

# Subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River
# Better catches throughout all subareas relative to SAPM
# Either high CPUES or 0s in SJ
# More moderated values in Sac

# Output for SPLT Subarea HM plots
png(file = "Figures/SPLT_Subarea_HM.png", width = 1500, height = 1850, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SPLT_Subarea_Heat_Map_Plot, ncol = 1)))         

dev.off()

# SASU --------------------------------------------------------------------
SASU_HM <- 
  SASU_Index %>% 
  group_by(Year, StationCode) %>% 
  summarise(MeanCPUE = mean(CPUE))
head(SASU_HM)
str(SASU_HM)

# Heat maps for stations
SASU_Heat_Map_Plot <- 
  ggplot(SASU_HM, aes(Year, StationCode)) +
  geom_tile(aes(fill = MeanCPUE), color = "white") +
  ggtitle("SASU") +
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_fill_gradient(low = "white", high = "steelblue")
SASU_Heat_Map_Plot

# Output for SASU HM plots
png(file = "Figures/SASU_Station_HM.png", width = 1500, height = 1850, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SASU_Heat_Map_Plot, ncol = 1)))         

dev.off()

# Heat maps for subareas
SASU_Subarea_HM <- 
  SASU_Index %>% 
  group_by(Year, subarea) %>% 
  summarise(MeanCPUE = mean(CPUE))
head(SASU_Subarea_HM)
str(SASU_Subarea_HM)

SASU_Subarea_Heat_Map_Plot <- 
  ggplot(SASU_Subarea_HM, aes(Year, subarea)) +
  geom_tile(aes(fill = MeanCPUE), color = "white") +
  ggtitle("SASU") +
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_fill_gradient(low = "white", high = "steelblue")
SASU_Subarea_Heat_Map_Plot

# Subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River
# Simillar to SASU, high CPUEs in Sac and low in SJ
# Some catches in subareas 4 and 5 in the Delta Region

# Output for SASU Subarea HM plots
png(file = "Figures/SASU_Subarea_HM.png", width = 1500, height = 1850, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SASU_Subarea_Heat_Map_Plot, ncol = 1)))         

dev.off()

# Indicies ----------------------------------------------------------------


# SAPM --------------------------------------------------------------------

# Calculating mean June and July CPUE for each station 
SAPM_Index_Final <- 
  SAPM_Index %>%
  group_by(StationCode, Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  spread(subarea, CPUE)

# Summing subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River
SAPM_Index_Final$Delta_Index = rowSums(SAPM_Index_Final[,2:6])
SAPM_Index_Final$Sac_River_Index = rowSums(SAPM_Index_Final[,7:9], na.rm = T) # na.rm = T because we are no longer sampling in subarea 8
SAPM_Index_Final$San_Joaquin_River_Index = rowSums(SAPM_Index_Final[,10:11], na.rm = T) # na.rm = T because we didn't sample in subarea 10 in 1995 and 1996

SAPM_Index_Final
SAPM_Index_Final = filter(SAPM_Index_Final, Year %in% c(1995:2017))
write.csv(SAPM_Index_Final, "Output/SAPM_Index_Final.csv")

## Plots

SAPM_SJ_GGPLOT = ggplot(data= SAPM_Index_Final, aes(x=Year, y=San_Joaquin_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,.025), expand = c(0, 0)) +
  ylab("San Joaquin Index") +
  theme_classic()
SAPM_SJ_GGPLOT

SAPM_Sac_GGPLOT = ggplot(data= SAPM_Index_Final, aes(x=Year, y=Sac_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,3), expand = c(0, 0)) +
  ylab("Sacramento Index") + 
  theme_classic()
SAPM_Sac_GGPLOT

SAPM_Delta_GGPLOT = ggplot(data= SAPM_Index_Final, aes(x=Year, y=Delta_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  ylab("Delta Index") + 
  theme_classic()
SAPM_Delta_GGPLOT


# Output for SAPM plots
png(file = "Figures/SAPM.Indicies.png", width = 1500, height = 1850, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SAPM_Sac_GGPLOT, SAPM_Delta_GGPLOT, SAPM_SJ_GGPLOT, ncol = 1,
                                   top =textGrob("SAPM", vjust=2,gp = gpar(fontsize=25,   fontfamily="Times")))))         

dev.off()



# SPLT --------------------------------------------------------------------

# Calculating mean May and June CPUE for each station 
SPLT_Index_Final <- 
  SPLT_Index %>%
  group_by(StationCode, Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  spread(subarea, CPUE)

# Summing subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River
SPLT_Index_Final$Delta_Index = rowSums(SPLT_Index_Final[,2:6])
SPLT_Index_Final$Sac_River_Index = rowSums(SPLT_Index_Final[,7:9], na.rm = T) # na.rm = T because we are no longer sampling in subarea 8
SPLT_Index_Final$San_Joaquin_River_Index = rowSums(SPLT_Index_Final[,10:11], na.rm = T) 

SPLT_Index_Final
SPLT_Index_Final = filter(SPLT_Index_Final, Year %in% c(1995:2017))
write.csv(SPLT_Index_Final, "Output/SPLT_Index_Final.csv")

## Plots

SPLT_SJ_GGPLOT = ggplot(data= SPLT_Index_Final, aes(x=Year, y=San_Joaquin_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,15), expand = c(0, 0)) +
  ylab("San Joaquin Index") +
  theme_classic()
SPLT_SJ_GGPLOT

SPLT_Sac_GGPLOT = ggplot(data= SPLT_Index_Final, aes(x=Year, y=Sac_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0)) +
  ylab("Sacramento Index") + 
  theme_classic()
SPLT_Sac_GGPLOT

SPLT_Delta_GGPLOT = ggplot(data= SPLT_Index_Final[1:22,], aes(x=Year, y=Delta_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,20), expand = c(0, 0)) +
  ylab("Delta Index") + 
  theme_classic()
SPLT_Delta_GGPLOT

# Output for SPLT plots
png(file = "Figures/SPLT.Indicies.png", width = 1500, height = 1850, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SPLT_Sac_GGPLOT, SPLT_Delta_GGPLOT, SPLT_SJ_GGPLOT,  ncol = 1,
                                   top =textGrob("SPLT", vjust=2,gp = gpar(fontsize=25,   fontfamily="Times")))))         

dev.off()


# SASU --------------------------------------------------------------------

# Calculating mean May and June CPUE for each station 
SASU_Index_Final <- 
  SASU_Index %>%
  group_by(StationCode, Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  spread(subarea, CPUE)

# Summing subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River
SASU_Index_Final$Delta_Index = rowSums(SASU_Index_Final[,2:6])
SASU_Index_Final$Sac_River_Index = rowSums(SASU_Index_Final[,7:9], na.rm = T) # na.rm = T because we are no longer sampling in subarea 8
SASU_Index_Final$San_Joaquin_River_Index = rowSums(SASU_Index_Final[,10:11], na.rm = T) 

SASU_Index_Final
SASU_Index_Final = filter(SASU_Index_Final, Year %in% c(1995:2017))
write.csv(SASU_Index_Final, "Output/SASU_Index_Final.csv")

## Plots

SASU_SJ_GGPLOT = ggplot(data= SASU_Index_Final, aes(x=Year, y=San_Joaquin_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  ylab("San Joaquin Index") +
  theme_classic()
SASU_SJ_GGPLOT

SASU_Sac_GGPLOT = ggplot(data= SASU_Index_Final, aes(x=Year, y=Sac_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,10), expand = c(0, 0)) +
  ylab("Sacramento Index") +
  theme_classic()
SASU_Sac_GGPLOT

SASU_Delta_GGPLOT = ggplot(data= SASU_Index_Final[1:22,], aes(x=Year, y=Delta_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,2.5), expand = c(0, 0)) +
  ylab("Delta Index") + 
  theme_classic()
SASU_Delta_GGPLOT

# Output for SASU plots
png(file = "Figures/SASU.Indicies.png", width = 1500, height = 1850, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SASU_Sac_GGPLOT, SASU_Delta_GGPLOT, SASU_SJ_GGPLOT,  ncol = 1,
                                   top =textGrob("SASU", vjust=2,gp = gpar(fontsize=25,   fontfamily="Times")))))         

dev.off()


# Discharge ---------------------------------------------------------------

## Discharge data is taken DNR Day Flow data
# SJ = SJR
# Sac = SAC - not going to use YOLO because the sites are upstream of the bypass
# Delta = OUT

## Centroid
# Looking at peak flow (PF) by julian day in each region
# Formula taken from Sommer et al. 2011
# PF = sum(Julian day * flow)/sum(flow)

# Creating julian day in discharge dataframe
temp = as.POSIXlt(Discharge$Date, format = "%d-%b-%y")

Discharge$Julian_Date <- temp$yday + 1
# I added 1 because this isn't really Julian Date...starts with 0 on Jan 1...so it would have messed up the PF formula

# going to calculate PF for Jan - May because peak spawning for all species should have started by May and high flows earlier may have allowed them to reach the spawning grounds

# Peak Flow by year
Peak_Flow_SJR <- 
  Discharge %>% 
  filter(Mo %in% 1:5 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * SJR)/sum(SJR))
head(Peak_Flow_SJR)

Peak_Flow_SAC <- 
  Discharge %>% 
  filter(Mo %in% 1:5 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * SAC)/sum(SAC))
head(Peak_Flow_SAC)

Peak_Flow_DELTA <- 
  Discharge %>% 
  filter(Mo %in% 1:5 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * OUT)/sum(OUT))
head(Peak_Flow_DELTA)

# SAPM --------------------------------------------------------------------

### Going to use mean discharge data for April and May (because they are spawning and eggs should hatch then)

## San Joaquin
SJ_Mean_Discharge_SAPM <- 
  Discharge %>% 
  filter(Mo %in% 4:5 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SJR_Discharge = mean(SJR))
SJ_Mean_Discharge_SAPM

# Combining PF and mean discharge for SJ
SJ_Discharge_SAPM <- 
  cbind(SJ_Mean_Discharge_SAPM, Peak_Flow_SJR$PF)
SJ_Discharge_SAPM

colnames(SJ_Discharge_SAPM) <- c("Year", "Mean_SJR_Discharge", "Peak_Flow")
SJ_Discharge_SAPM

## Sacramento
SAC_Mean_Discharge_SAPM <- 
  Discharge %>% 
  filter(Mo %in% 4:5 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SACR_Discharge = mean(SAC))
SAC_Mean_Discharge_SAPM

# Combining PF and mean discharge for SAC
SAC_Discharge_SAPM <- 
  cbind(SAC_Mean_Discharge_SAPM, Peak_Flow_SAC$PF)
SAC_Discharge_SAPM

colnames(SAC_Discharge_SAPM) <- c("Year", "Mean_SACR_Discharge", "Peak_Flow")
SAC_Discharge_SAPM

## Delta
DELTA_Mean_Discharge_SAPM <- 
  Discharge %>% 
  filter(Mo %in% 4:5 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_DELTA_Discharge = mean(OUT))
DELTA_Mean_Discharge_SAPM

# Combining PF and mean discharge for DELTA
DELTA_Discharge_SAPM <- 
  cbind(DELTA_Mean_Discharge_SAPM, Peak_Flow_DELTA$PF)
DELTA_Discharge_SAPM

colnames(DELTA_Discharge_SAPM) <- c("Year", "Mean_DELTA_Discharge", "Peak_Flow")
DELTA_Discharge_SAPM

SAPM_DELTA_Discharge_LM = lm(log(SAPM_Index_Final$Delta_Index) ~ DELTA_Discharge_SAPM$Mean_DELTA_Discharge + DELTA_Discharge_SAPM$Peak_Flow)
summary(SAPM_DELTA_Discharge_LM)
# Postive relationship between PF and index
plot(log(SAPM_Index_Final$Delta_Index[1:23]) ~ DELTA_Discharge_SAPM$Peak_Flow, pch = 16)
# Pretty solid relationship

# SPLT --------------------------------------------------------------------
# Used May and June for index
# Peak spawnig in March and April (Moyle)
# Going to use March and April

## San Joaquin
SJ_Mean_Discharge_SPLT <- 
  Discharge %>% 
  filter(Mo %in% 3:4 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SJR_Discharge = mean(SJR))
SJ_Mean_Discharge_SPLT

# Combining PF and mean discharge for SJ
SJ_Discharge_SPLT <- 
  cbind(SJ_Mean_Discharge_SPLT, Peak_Flow_SJR$PF)
SJ_Discharge_SPLT

colnames(SJ_Discharge_SPLT) <- c("Year", "Mean_SJR_Discharge", "Peak_Flow")
SJ_Discharge_SPLT

SPLT_SJ_Discharge_LM = lm(log(SPLT_Index_Final$San_Joaquin_River_Index) ~ SJ_Discharge_SPLT$Mean_SJR_Discharge + SJ_Discharge_SPLT$Peak_Flow)
summary(SPLT_SJ_Discharge_LM)
# Positive relationships, for both discharge and Peak Flow


## Sacramento
SAC_Mean_Discharge_SPLT <- 
  Discharge %>% 
  filter(Mo %in% 3:4 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SACR_Discharge = mean(SAC))
SAC_Mean_Discharge_SPLT

# Combining PF and mean discharge for SAC
SAC_Discharge_SPLT <- 
  cbind(SAC_Mean_Discharge_SPLT, Peak_Flow_SAC$PF)
SAC_Discharge_SPLT

colnames(SAC_Discharge_SPLT) <- c("Year", "Mean_SACR_Discharge", "Peak_Flow")
SAC_Discharge_SPLT

SPLT_SAC_Discharge_LM = lm(log(SPLT_Index_Final$Sac_River_Index) ~ SAC_Discharge_SPLT$Mean_SACR_Discharge + SAC_Discharge_SPLT$Peak_Flow)
summary(SPLT_SAC_Discharge_LM)
# No relationships

## Delta
DELTA_Mean_Discharge_SPLT <- 
  Discharge %>% 
  filter(Mo %in% 3:4 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_DELTA_Discharge = mean(OUT))
DELTA_Mean_Discharge_SPLT

# Combining PF and mean discharge for DELTA
DELTA_Discharge_SPLT <- 
  cbind(DELTA_Mean_Discharge_SPLT, Peak_Flow_DELTA$PF)
DELTA_Discharge_SPLT

colnames(DELTA_Discharge_SPLT) <- c("Year", "Mean_DELTA_Discharge", "Peak_Flow")
DELTA_Discharge_SPLT

SPLT_DELTA_Discharge_LM = lm(log(SPLT_Index_Final$Delta_Index) ~ DELTA_Discharge_SPLT$Mean_DELTA_Discharge + DELTA_Discharge_SPLT$Peak_Flow)
summary(SPLT_DELTA_Discharge_LM)
# Positive relaitonship between discharge and index

# SASU --------------------------------------------------------------------

# Used May and June for index
# Peak spawning in March and April (Moyle)
# Going to look at how discharge in March and April influences abundance index

SJ_Mean_Discharge_SASU <- 
  Discharge %>% 
  filter(Mo %in% 3:4 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SJR_Discharge = mean(SJR))
SJ_Mean_Discharge_SASU

# Combining PF and mean discharge for SJ
SJ_Discharge_SASU <- 
  cbind(SJ_Mean_Discharge_SASU, Peak_Flow_SJR$PF)
SJ_Discharge_SASU

colnames(SJ_Discharge_SASU) <- c("Year", "Mean_SJR_Discharge", "Peak_Flow")
SJ_Discharge_SASU

SASU_SJ_Discharge_LM = lm(log(SASU_Index_Final$San_Joaquin_River_Index) ~ SJ_Discharge_SASU$Mean_SJR_Discharge + SJ_Discharge_SASU$Peak_Flow)
summary(SASU_SJ_Discharge_LM)
# No relationships

## Sacramento
SAC_Mean_Discharge_SASU <- 
  Discharge %>% 
  filter(Mo %in% 3:4 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SACR_Discharge = mean(SAC))
SAC_Mean_Discharge_SASU

# Combining PF and mean discharge for SAC
SAC_Discharge_SASU <- 
  cbind(SAC_Mean_Discharge_SASU, Peak_Flow_SAC$PF)
SAC_Discharge_SASU

colnames(SAC_Discharge_SASU) <- c("Year", "Mean_SACR_Discharge", "Peak_Flow")
SAC_Discharge_SASU

SASU_SAC_Discharge_LM_Full = lm(log(SASU_Index_Final$Sac_River_Index) ~ SAC_Discharge_SASU$Mean_SACR_Discharge + SAC_Discharge_SASU$Peak_Flow)
summary(SASU_SAC_Discharge_LM_Full)
# Negative relationship between discharge and index

plot(log(SASU_Index_Final$Sac_River_Index) ~ SAC_Discharge_SASU$Mean_SACR_Discharge, pch = 16)

## Delta
DELTA_Mean_Discharge_SASU <- 
  Discharge %>% 
  filter(Mo %in% 3:4 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_DELTA_Discharge = mean(OUT))
DELTA_Mean_Discharge_SASU

# Combining PF and mean discharge for DELTA
DELTA_Discharge_SASU <- 
  cbind(DELTA_Mean_Discharge_SASU, Peak_Flow_DELTA$PF)
DELTA_Discharge_SASU

colnames(DELTA_Discharge_SASU) <- c("Year", "Mean_DELTA_Discharge", "Peak_Flow")
DELTA_Discharge_SASU

SASU_DELTA_Discharge_LM = lm(log(SASU_Index_Final$Delta_Index) ~ DELTA_Discharge_SASU$Mean_DELTA_Discharge + DELTA_Discharge_SASU$Peak_Flow)
summary(SASU_DELTA_Discharge_LM)
# Positive relaitonship between peak flow and index
# Nearly...negative relationship between discharge and index
plot(SASU_DELTA_Discharge_LM)

# Zooplankton -------------------------------------------------------------

### Zooplankton data
# Longest time series is from the EMP data...if I used CDFW townet/20 mm survey data I would have to cut ~ 10 years of data

# Function
numMonth <- function(x) {
  months <- list(january=1,february=2,march=3,april=4,may=5,june=6,july=7,august=8,september=9,october=10,november=11,december=12)
  x <- tolower(x)
  sapply(x,function(x) months[[x]])
}

# SAPM --------------------------------------------------------------------
# Based on Nobriga et al. 2006 - For SAPM <150 mm most common prey items were Annelids, Corophiid and Gammarid amphipods. 
# SAPM <150 mm did not consume a lot of fish.
# Got benthic invert data from Betsy Wells
# Used two stations from data; D24-L: Sacramento River downstream of Rio Vista bridge (left); P8-R: San Joaquin River at Buckley Cove (right)
# D24-L doesn't line up well with stations used to make abundance indicies...may have to use other stations that are closer but
# should wait until find the station that we are going to use for discharge first. 


## SJ
SAPM_Diet_SJ = read.csv("data/SAPM_Diet_SJ.csv")
head(SAPM_Diet_SJ)

SAPM_Diet_SJ$Month = as.factor(numMonth(SAPM_Diet_SJ$Month))
SAPM_Diet_SJ$Year = as.factor(SAPM_Diet_SJ$Year)
str(SAPM_Diet_SJ)
tail(SAPM_Diet_SJ)

# Used months 6 through 7 for SAPM index and have years 1995 - 2017
SAPM_Diet_SJ <- 
  SAPM_Diet_SJ %>% 
  filter(Month %in% c(6:7) & Year %in% c(1995:2017))
head(SAPM_Diet_SJ)
# Time series startes in 1996


# Creating a mean per group per year
SAPM_Diet_SJ_Year_Agg <- 
  as.data.frame(SAPM_Diet_SJ %>% 
                  group_by(Year) %>% 
                  summarise_at(c("Annelida", "Corophiidae", "Gammaridae"), mean, na.rm = TRUE))
head(SAPM_Diet_SJ_Year_Agg)
str(SAPM_Diet_SJ_Year_Agg)
unique(SAPM_Diet_SJ_Year_Agg$Year)

# Creating matrix for PCA
Diet_SAPM_SJ_Year_Matrix = SAPM_Diet_SJ_Year_Agg[, -c(1)]
head(Diet_SAPM_SJ_Year_Matrix)

Diet_SAPM_SJ_Year_PC = principal(Diet_SAPM_SJ_Year_Matrix, nfactors = 2, rotate = "varimax") 
Diet_SAPM_SJ_Year_PC
# PC 1 = Corophiidae and Gammaridae
# PC 2 = Annelida

# Scores for analyses
Diet_SAPM_SJ_PC_Scores = as.data.frame(Diet_SAPM_SJ_Year_PC$scores)
colnames(Diet_SAPM_SJ_PC_Scores) <- c("SJ_PC1", "SJ_PC2")
Diet_SAPM_SJ_PC_Scores


## SAC
SAPM_Diet_SAC = read.csv("data/SAPM_Diet_SAC.csv")
head(SAPM_Diet_SAC)

SAPM_Diet_SAC$Month = as.factor(numMonth(SAPM_Diet_SAC$Month))
SAPM_Diet_SAC$Year = as.factor(SAPM_Diet_SAC$Year)
str(SAPM_Diet_SAC)
tail(SAPM_Diet_SAC)

# Used months 6 through 7 for SAPM index and have years 1995 - 2017
SAPM_Diet_SAC <- 
  SAPM_Diet_SAC %>% 
  filter(Month %in% c(6:7) & Year %in% c(1995:2017))
head(SAPM_Diet_SAC)
# Time series startes in 1996


# Creating a mean per group per year
SAPM_Diet_SAC_Year_Agg <- 
  as.data.frame(SAPM_Diet_SAC %>% 
                  group_by(Year) %>% 
                  summarise_at(c("Annelida", "Corophiidae", "Gammaridae"), mean, na.rm = TRUE))
head(SAPM_Diet_SAC_Year_Agg)
str(SAPM_Diet_SAC_Year_Agg)
unique(SAPM_Diet_SAC_Year_Agg$Year)

# Creating matrix for PCA
Diet_SAPM_SAC_Year_Matrix = SAPM_Diet_SAC_Year_Agg[, -c(1)]
head(Diet_SAPM_SAC_Year_Matrix)

Diet_SAPM_SAC_Year_PC = principal(Diet_SAPM_SAC_Year_Matrix, nfactors = 2, rotate = "varimax") 
Diet_SAPM_SAC_Year_PC
# PC 1 = Corophiidae and Gammaridae
# PC 2 = Annelida

# Scores for analyses
Diet_SAPM_SAC_PC_Scores = as.data.frame(Diet_SAPM_SAC_Year_PC$scores)
colnames(Diet_SAPM_SAC_PC_Scores) <- c("SAC_PC1", "SAC_PC2")
Diet_SAPM_SAC_PC_Scores


## DELTA
SAPM_Diet_DELTA = read.csv("data/SAPM_Diet_DELTA.csv")
head(SAPM_Diet_DELTA)

SAPM_Diet_DELTA$Month = as.factor(numMonth(SAPM_Diet_DELTA$Month))
SAPM_Diet_DELTA$Year = as.factor(SAPM_Diet_DELTA$Year)
str(SAPM_Diet_DELTA)
tail(SAPM_Diet_DELTA)


# Used months 6 through 7 for SAPM index and have years 1995 - 2017
SAPM_Diet_DELTA <- 
  SAPM_Diet_DELTA %>% 
  filter(Month %in% c(6:7) & Year %in% c(1995:2017))
head(SAPM_Diet_DELTA)
# Time series startes in 1995


# Creating a mean per group per year
SAPM_Diet_DELTA_Year_Agg <- 
  as.data.frame(SAPM_Diet_DELTA %>% 
                  group_by(Year) %>% 
                  summarise_at(c("Annelida", "Corophiidae", "Gammaridae"), mean, na.rm = TRUE))
head(SAPM_Diet_DELTA_Year_Agg)
str(SAPM_Diet_DELTA_Year_Agg)
unique(SAPM_Diet_DELTA_Year_Agg$Year)

# Creating matrix for PCA
Diet_SAPM_DELTA_Year_Matrix = SAPM_Diet_DELTA_Year_Agg[, -c(1)]
head(Diet_SAPM_DELTA_Year_Matrix)

Diet_SAPM_DELTA_Year_PC = principal(Diet_SAPM_DELTA_Year_Matrix, nfactors = 2, rotate = "varimax") 
Diet_SAPM_DELTA_Year_PC
# PC 1 = Corophiidae and Gammaridae
# PC 2 = Annelida and mid range loadings for Gammaridae

# Scores for analyses
Diet_SAPM_DELTA_PC_Scores = as.data.frame(Diet_SAPM_DELTA_Year_PC$scores)
colnames(Diet_SAPM_DELTA_PC_Scores) <- c("DELTA_PC1", "DELTA_PC2")
Diet_SAPM_DELTA_PC_Scores



# SPLT --------------------------------------------------------------------
### SPLT
# Used catch of fish in months 5 & 6 to create age-0 abundance indicies
# Spawn from late Feb through early May
# ~10 days after hatching they begin exogenous feeding
# Starting feeding on small rotifers and switch to small crustaceans and then dipterans as they grow larger (Splittail WPaper)
# Kurth and Nobriga 2001 (IEP Newsletter) found that larval SPLT fed on cladocerans (56% of diet by dry weight), chironomid larvae (40%) and copepods (4%)
# Rotifers comprised <1%. Other items encountered infrequently included diatoms, detritus, and terrestrial insects
# Daniels and Moyle 1983: SPLT 50-100 mm SL consume primarily detritus but also feed on harpactacoid and calanoid copepods
# Use Zooplankton data from May and June to determine how they influence age-0 abundance indicies


## San Joaquin
Zoop_SPLT_SJ_Raw = Zoop_Raw[Zoop_Raw$Station == "NZ092" & Zoop_Raw$Month %in% c(5:6) & Zoop_Raw$Year %in% c(1995:2017),]
head(Zoop_SPLT_SJ_Raw)

# Averaging zooplankton CPUE by year
Zoop_SPLT_SJ_Year_Agg <- 
  Zoop_SPLT_SJ_Raw %>% group_by(Year) %>%
  summarise_all(funs(mean)) 
head(Zoop_SPLT_SJ_Year_Agg) # 1995:2017

# Going to see if PC will reduce the number of variables
Zoop_SPLT_SJ_Year_Matrix = as.matrix(Zoop_SPLT_SJ_Year_Agg) # Matrix for principal function

# Going to use Calanoids, Harpactacoids, and Cladocera because literature says they can be important diet items for larval SPLT
Zoop_SPLT_SJ_Year_Matrix = Zoop_SPLT_SJ_Year_Matrix[, -c(1:16,18,21:22,24,25)]
head(Zoop_SPLT_SJ_Year_Matrix)

Zoop_SPLT_SJ_Year_PC = principal(Zoop_SPLT_SJ_Year_Matrix, nfactors = 2, rotate = "varimax")  
Zoop_SPLT_SJ_Year_PC
# PC1 = Calanoid adults, Harpactacoids, Cladocera
# PC2 = Calanoid juveniles and mid range loadings for Calanoid adults

Zoop_SPLT_SJ_PC_Scores = as.data.frame(Zoop_SPLT_SJ_Year_PC$scores)
colnames(Zoop_SPLT_SJ_PC_Scores) <- c("SJ_PC1", "SJ_PC2")

## Sacramento
Zoop_SPLT_SAC_Raw = Zoop_Raw[Zoop_Raw$Station == "NZ064" & Zoop_Raw$Month %in% c(5:6) & Zoop_Raw$Year %in% c(1995:2017),]
head(Zoop_SPLT_SAC_Raw)

# Averaging zooplankton CPUE by year
Zoop_SPLT_SAC_Year_Agg <- 
  Zoop_SPLT_SAC_Raw %>% group_by(Year) %>% summarise_all(funs(mean)) 
tail(Zoop_SPLT_SAC_Year_Agg) # 1995:2017

# Going to see if PC will reduce the number of variables
Zoop_SPLT_SAC_Year_Matrix = as.matrix(Zoop_SPLT_SAC_Year_Agg) # Matrix for principal function

# Going to use Calanoids, Harpactacoids, and Cladocera because literature says they can be important diet items for larval SPLT
Zoop_SPLT_SAC_Year_Matrix = Zoop_SPLT_SAC_Year_Matrix[, -c(1:16,18,21:22,24,25)]
head(Zoop_SPLT_SAC_Year_Matrix)

Zoop_SPLT_SAC_Year_PC = principal(Zoop_SPLT_SAC_Year_Matrix, nfactors = 2, rotate = "varimax") 
Zoop_SPLT_SAC_Year_PC
#PC1 = Calanoid adults and juveniles
#PC2 = Cladocera
# Mid range values for Harpact for each PC


Zoop_SPLT_SAC_PC_Scores = as.data.frame(Zoop_SPLT_SAC_Year_PC$scores)
colnames(Zoop_SPLT_SAC_PC_Scores) <- c("SAC_PC1", "SAC_PC2")


## Delta
Zoop_SPLT_DELTA_Raw = Zoop_Raw[Zoop_Raw$Station == "NZ086" & Zoop_Raw$Month %in% c(5:6) & Zoop_Raw$Year %in% c(1995:2017),]
head(Zoop_SPLT_DELTA_Raw)

# Averaging zooplankton CPUE by year
Zoop_SPLT_DELTA_Year_Agg <- 
  Zoop_SPLT_DELTA_Raw %>% group_by(Year) %>% summarise_all(funs(mean)) 
tail(Zoop_SPLT_DELTA_Year_Agg) # 1995:2017

# Going to see if PC will reduce the number of variables
Zoop_SPLT_DELTA_Year_Matrix = as.matrix(Zoop_SPLT_DELTA_Year_Agg) # Matrix for principal function

# Going to use Calanoids, Harpactacoids, and Cladocera because literature says they can be important diet items for larval SPLT
Zoop_SPLT_DELTA_Year_Matrix = Zoop_SPLT_DELTA_Year_Matrix[, -c(1:16,18,21:22,24,25)]
head(Zoop_SPLT_DELTA_Year_Matrix)

Zoop_SPLT_DELTA_Year_PC = principal(Zoop_SPLT_DELTA_Year_Matrix, nfactors = 2, rotate = "varimax") 
Zoop_SPLT_DELTA_Year_PC
# PC1 = Calanoid adults and juveniles, and cladocera
# PC2 = Harpact


Zoop_SPLT_DELTA_PC_Scores = as.data.frame(Zoop_SPLT_DELTA_Year_PC$scores)
colnames(Zoop_SPLT_DELTA_PC_Scores) <- c("DELTA_PC1", "DELTA_PC2")

# Combining Data -------------------------------------------------------------

# SAPM --------------------------------------------------------------------

SAPM_Index_Final
SJ_Discharge_SAPM
SAC_Discharge_SAPM
DELTA_Discharge_SAPM
Diet_SAPM_SJ_PC_Scores
Diet_SAPM_SAC_PC_Scores
Diet_SAPM_DELTA_PC_Scores

## SJ
# Prey data only is available for 1996-2017
SAPM_SJ_Final <- 
  SAPM_Index_Final %>% 
  filter(Year %in% c(1996:2017))
  
  
SAPM_SJ_Final <- cbind(SAPM_SJ_Final$Year, SAPM_SJ_Final$San_Joaquin_River_Index, SJ_Discharge_SAPM[2:23,2:3], Diet_SAPM_SJ_PC_Scores)
colnames(SAPM_SJ_Final)  <- c("Year", "San_Joaquin_River_Index", "Mean_SJR_Discharge", "Peak_Flow", "SJ_PC1", "SJ_PC2")


## SAC
# Same as above...prey data only available for 1996-2017
SAPM_SAC_Final <- 
  SAPM_Index_Final %>% 
  filter(Year %in% c(1996:2017))
SAPM_SAC_Final

SAPM_SAC_Final <- cbind(SAPM_SAC_Final$Year, SAPM_SAC_Final$Sac_River_Index, SAC_Discharge_SAPM[2:23,2:3], Diet_SAPM_SAC_PC_Scores)
colnames(SAPM_SAC_Final)  <- c("Year", "Sac_River_Index", "Mean_SAC_Discharge", "Peak_Flow", "SAC_PC1", "SAC_PC2")

## Delta
SAPM_DELTA_Final <- cbind(SAPM_Index_Final$Year, SAPM_Index_Final$Delta_Index, DELTA_Discharge_SAPM[,2:3], Diet_SAPM_DELTA_PC_Scores)
colnames(SAPM_DELTA_Final) <- c("Year", "DELTA_Index", "Mean_DELTA_Discharge", "Peak_Flow", "DELTA_PC1", "DELTA_PC2")

# SPLT --------------------------------------------------------------------

SPLT_Index_Final
SJ_Discharge_SPLT
SAC_Discharge_SPLT
DELTA_Discharge_SPLT
Zoop_SPLT_SJ_PC_Scores
Zoop_SPLT_SAC_PC_Scores
Zoop_SPLT_DELTA_PC_Scores

SPLT_SJ_Final <- cbind(SPLT_Index_Final$Year, SPLT_Index_Final$San_Joaquin_River_Index, SJ_Discharge_SPLT[,2:3], Zoop_SPLT_SJ_PC_Scores)
colnames(SPLT_SJ_Final)  <- c("Year", "San_Joaquin_River_Index", "Mean_SJR_Discharge", "Peak_Flow", "SJ_PC1", "SJ_PC2")

SPLT_SAC_Final <- cbind(SPLT_Index_Final$Year, SPLT_Index_Final$Sac_River_Index, SAC_Discharge_SPLT[,2:3], Zoop_SPLT_SAC_PC_Scores)
colnames(SPLT_SAC_Final) <- c("Year", "Sac_River_Index", "Mean_SAC_Discharge", "Peak_Flow", "SAC_PC1", "SAC_PC2")

SPLT_DELTA_Final <- cbind(SPLT_Index_Final$Year, SPLT_Index_Final$Delta_Index, DELTA_Discharge_SPLT[,2:3], Zoop_SPLT_DELTA_PC_Scores)
colnames(SPLT_DELTA_Final) <- c("Year", "DELTA_Index", "Mean_DELTA_Discharge", "Peak_Flow", "DELTA_PC1", "DELTA_PC2")



# SASU --------------------------------------------------------------------
SASU_Index_Final
SJ_Discharge_SASU
SAC_Discharge_SASU
DELTA_Discharge_SASU

SASU_SJ_Final <- cbind(SASU_Index_Final$Year, SASU_Index_Final$San_Joaquin_River_Index, SJ_Discharge_SASU[,2:3])
colnames(SASU_SJ_Final)  <- c("Year", "San_Joaquin_River_Index", "Mean_SJR_Discharge", "Peak_Flow")

SASU_SAC_Final <- cbind(SASU_Index_Final$Year, SASU_Index_Final$Sac_River_Index, SAC_Discharge_SASU[,2:3])
colnames(SASU_SAC_Final) <- c("Year", "Sac_River_Index", "Mean_SAC_Discharge", "Peak_Flow")

SASU_DELTA_Final <- cbind(SASU_Index_Final$Year, SASU_Index_Final$Delta_Index, DELTA_Discharge_SASU[,2:3])
colnames(SASU_DELTA_Final) <- c("Year", "DELTA_Index", "Mean_DELTA_Discharge", "Peak_Flow")


# Final Analyses ----------------------------------------------------------


# SAPM --------------------------------------------------------------------
SAPM_SJ_Final
SAPM_SAC_Final
SAPM_DELTA_Final

temp = (SAPM_SJ_Final$Mean_SJR_Discharge - mean(SAPM_SJ_Final$Mean_SJR_Discharge))/sd(SAPM_SJ_Final$Mean_SJR_Discharge)
temp.2 = scale(SAPM_SJ_Final[,2:6])

## SJ
str(SAPM_SJ_Final)
# Global Model
SAPM_SJ_Global = lm(log(San_Joaquin_River_Index + 1) ~ Mean_SJR_Discharge + Peak_Flow + SJ_PC1 + SJ_PC2, data = SAPM_SJ_Final, na.action = "na.fail")
summary(SAPM_SJ_Global)
cor(SAPM_SJ_Final[, 2:6])
acf(SAPM_SJ_Global$residuals) # Autocorreltion seems fine

SAPM_SJ_Global_Dredge = dredge(SAPM_SJ_Global)
importance(SAPM_SJ_Global_Dredge)
# Null model is top model and has highest weights


# Same analyses using standardized variables
SAPM_SJ_Final.Scale = as.data.frame(scale(SAPM_SJ_Final[,3:6]))

SAPM_SJ_Global.Scale = lm(log(San_Joaquin_River_Index + 1) ~ SAPM_SJ_Final.Scale$Mean_SJR_Discharge + SAPM_SJ_Final.Scale$Peak_Flow + SAPM_SJ_Final.Scale$SJ_PC1 + SAPM_SJ_Final.Scale$SJ_PC2, data = SAPM_SJ_Final, na.action = "na.fail")
summary(SAPM_SJ_Global.Scale)

SAPM_SJ_Global_Scale_Dredge = dredge(SAPM_SJ_Global.Scale)
importance(SAPM_SJ_Global_Scale_Dredge)

SAPM_SJ_MA = model.avg(SAPM_SJ_Global_Scale_Dredge)
summary(SAPM_SJ_MA)



## SAC
str(SAPM_SAC_Final)
# Global Model
SAPM_SAC_Global = lm(log(Sac_River_Index) ~ Mean_SAC_Discharge + Peak_Flow + SAC_PC1 + SAC_PC2, data = SAPM_SAC_Final, na.action = "na.fail")
summary(SAPM_SAC_Global)
acf(SAPM_SAC_Global$residuals) # Autocorreltion seems fine

SAPM_SAC_Global_Dredge = dredge(SAPM_SAC_Global)
importance(SAPM_SAC_Global_Dredge)
# Null model is top model and has highest weights


# Same analyses using standardized variables
SAPM_SAC_Final.Scale = as.data.frame(scale(SAPM_SAC_Final[,3:6]))

SAPM_SAC_Global.Scale = lm(log(Sac_River_Index) ~ SAPM_SAC_Final.Scale$Mean_SAC_Discharge + SAPM_SAC_Final.Scale$Peak_Flow + SAPM_SAC_Final.Scale$SAC_PC1 + SAPM_SAC_Final.Scale$SAC_PC2, data = SAPM_SAC_Final, na.action = "na.fail")
summary(SAPM_SAC_Global.Scale)

SAPM_SAC_Global_Scale_Dredge = dredge(SAPM_SAC_Global.Scale)
importance(SAPM_SAC_Global_Scale_Dredge)

SAPM_SAC_MA = model.avg(SAPM_SAC_Global_Scale_Dredge)
summary(SAPM_SAC_MA)



## DELTA
str(SAPM_DELTA_Final)

# Global Model
SAPM_DELTA_Global = lm(log(DELTA_Index) ~ Mean_DELTA_Discharge + Peak_Flow + DELTA_PC1 + DELTA_PC2, data = SAPM_DELTA_Final, na.action = "na.fail")
summary(SAPM_DELTA_Global)
acf(SAPM_DELTA_Global$residuals) # Autocorreltion seems fine


SAPM_DELTA_Global_Dredge = dredge(SAPM_DELTA_Global)
importance(SAPM_DELTA_Global_Dredge)
# Discharge has highest weights (0.91) but PC2 (0.69) and Peak Flow (0.65) also seem important


# Same analyses using standardized variables
SAPM_DELTA_Final.Scale = as.data.frame(scale(SAPM_DELTA_Final[,3:6]))

SAPM_DELTA_Global.Scale = lm(log(DELTA_Index) ~ SAPM_DELTA_Final.Scale$Mean_DELTA_Discharge + SAPM_DELTA_Final.Scale$Peak_Flow + SAPM_DELTA_Final.Scale$DELTA_PC1 + SAPM_DELTA_Final.Scale$DELTA_PC2, data = SAPM_DELTA_Final, na.action = "na.fail")
summary(SAPM_DELTA_Global.Scale)

SAPM_DELTA_Global_Scale_Dredge = dredge(SAPM_DELTA_Global.Scale)
importance(SAPM_DELTA_Global_Scale_Dredge)

SAPM_DELTA_MA = model.avg(SAPM_DELTA_Global_Scale_Dredge)
summary(SAPM_DELTA_MA)



# SPLT --------------------------------------------------------------------
SPLT_SJ_Final
SPLT_SAC_Final
SPLT_DELTA_Final

## SJ
str(SPLT_SJ_Final)
# Global Model
SPLT_SJ_Global = lm(log(San_Joaquin_River_Index) ~ Mean_SJR_Discharge + Peak_Flow + SJ_PC1 + SJ_PC2, data = SPLT_SJ_Final, na.action = "na.fail")
summary(SPLT_SJ_Global)
cor(SPLT_SJ_Final[, 2:6])
acf(SPLT_SJ_Global$residuals) # Autocorreltion seems fine

SPLT_SJ_Global_Dredge = dredge(SPLT_SJ_Global)
importance(SPLT_SJ_Global_Dredge)
# Mean discharge and PC 1 are most important, both have weights > .95

# Same analyses using standardized variables
SPLT_SJ_Final.Scale = as.data.frame(scale(SPLT_SJ_Final[,3:6]))

SPLT_SJ_Global.Scale = lm(log(San_Joaquin_River_Index) ~ SPLT_SJ_Final.Scale$Mean_SJR_Discharge + SPLT_SJ_Final.Scale$Peak_Flow + SPLT_SJ_Final.Scale$SJ_PC1 + SPLT_SJ_Final.Scale$SJ_PC2, data = SPLT_SJ_Final, na.action = "na.fail")
summary(SPLT_SJ_Global.Scale)

SPLT_SJ_Global_Scale_Dredge = dredge(SPLT_SJ_Global.Scale)
importance(SPLT_SJ_Global_Scale_Dredge)

SPLT_SJ_MA = model.avg(SPLT_SJ_Global_Scale_Dredge)
summary(SPLT_SJ_MA)



## SAC
str(SPLT_SAC_Final)
# Global Model
SPLT_SAC_Global = lm(log(Sac_River_Index) ~ Mean_SAC_Discharge + Peak_Flow + SAC_PC1 + SAC_PC2, data = SPLT_SAC_Final, na.action = "na.fail")
summary(SPLT_SAC_Global)
acf(SPLT_SAC_Global$residuals) # Autocorreltion seems fine

SPLT_SAC_Global_Dredge = dredge(SPLT_SAC_Global)
importance(SPLT_SAC_Global_Dredge)
# Null model is top model
# Peak flow and PC1 have highest summed model weights (~0.45)


# Same analyses using standardized variables
SPLT_SAC_Final.Scale = as.data.frame(scale(SPLT_SAC_Final[,3:6]))

SPLT_SAC_Global.Scale = lm(log(Sac_River_Index) ~ SPLT_SAC_Final.Scale$Mean_SAC_Discharge + SPLT_SAC_Final.Scale$Peak_Flow + SPLT_SAC_Final.Scale$SAC_PC1 + SPLT_SAC_Final.Scale$SAC_PC2, data = SPLT_SAC_Final, na.action = "na.fail")
summary(SPLT_SAC_Global.Scale)

SPLT_SAC_Global_Scale_Dredge = dredge(SPLT_SAC_Global.Scale)
importance(SPLT_SAC_Global_Scale_Dredge)

SPLT_SAC_MA = model.avg(SPLT_SAC_Global_Scale_Dredge)
summary(SPLT_SAC_MA)





## DELTA
str(SPLT_DELTA_Final)

# 2017 doesn't have an index estimate so I am going to remove it from the data frame so the models will run...
SPLT_DELTA_Final = SPLT_DELTA_Final[1:22,]

# Global Model
SPLT_DELTA_Global = lm(log(DELTA_Index) ~ Mean_DELTA_Discharge + Peak_Flow + DELTA_PC1 + DELTA_PC2, data = SPLT_DELTA_Final, na.action = "na.fail")
summary(SPLT_DELTA_Global)
acf(SPLT_DELTA_Global$residuals) # Autocorreltion seems fine


SPLT_DELTA_Global_Dredge = dredge(SPLT_DELTA_Global)
importance(SPLT_DELTA_Global_Dredge)
# Discharge has highest weights (0.91) but PC2 (0.69) and Peak Flow (0.65) also seem important


# SASU --------------------------------------------------------------------
SASU_SJ_Final
SASU_SAC_Final
SASU_DELTA_Final

## SJ
# Global Model
SASU_SJ_Global = lm(log(San_Joaquin_River_Index) ~ Mean_SJR_Discharge + Peak_Flow, data = SASU_SJ_Final, na.action = "na.fail")
summary(SASU_SJ_Global)
cor(SASU_SJ_Final[, 2:4]) # Multicolinearity isn't a problem
acf(SASU_SJ_Global$residuals) # Autocorreltion seems fine


SASU_SJ_Global_Dredge = dredge(SASU_SJ_Global)
importance(SASU_SJ_Global_Dredge)
# Null model is second model
# Mean discharge has weight of 0.51

## SAC

# Global Model
SASU_SAC_Global = lm(log(Sac_River_Index) ~ Mean_SAC_Discharge + Peak_Flow, data = SASU_SAC_Final, na.action = "na.fail")
summary(SASU_SAC_Global)
cor(SASU_SAC_Final[, 2:4]) # Multicolinearity isn't a problem
acf(SASU_SAC_Global$residuals) # Autocorreltion seems fine


SASU_SAC_Global_Dredge = dredge(SASU_SAC_Global)
importance(SASU_SAC_Global_Dredge)
# Discharge has highest summed model weights (1.0)

## DELTA
str(SASU_DELTA_Final)

# 2017 doesn't have an index estimate so I am going to remove it from the data frame so the models will run...
SASU_DELTA_Final = SASU_DELTA_Final[1:22,]

# Global Model
SASU_DELTA_Global = lm(log(DELTA_Index) ~ Mean_DELTA_Discharge + Peak_Flow, data = SASU_DELTA_Final, na.action = "na.fail")
summary(SASU_DELTA_Global)
acf(SASU_DELTA_Global$residuals) # Autocorreltion seems fine


SASU_DELTA_Global_Dredge = dredge(SASU_DELTA_Global)
importance(SASU_DELTA_Global_Dredge)
# Peak flows have high weights (0.86) but discharge also seems important (0.60)

