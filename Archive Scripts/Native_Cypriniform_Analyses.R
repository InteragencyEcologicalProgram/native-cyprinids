
# Background --------------------------------------------------------------

# Study is trying to determine the influence of biotic and abiotic factors on the age-0 abundanece indicies of SPLT, SASU, SAPM

# Length Frequency: Staring with legth frequency analyses to determine what months and length cuttoffs to use when creating age-0 abundance indicies 

# Plus Count proportions: # Use FL of Age-0 fishes caught within a seine haul to proporiton FL of plus count fish

# Age-0 Indicies: Using plus count proportioned data to create age-0 abundance indicies


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
write.csv(SAPM_Index_Final, "Output/SAPM_Index_Final.csv")

## Plots

SAPM_SJ_GGPLOT = ggplot(data= SAPM_Index_Final, aes(x=Year, y=San_Joaquin_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,.025), expand = c(0, 0)) +
  theme_classic()
SAPM_SJ_GGPLOT

SAPM_Sac_GGPLOT = ggplot(data= SAPM_Index_Final, aes(x=Year, y=Sac_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,3), expand = c(0, 0)) +
  theme_classic()
SAPM_Sac_GGPLOT

# Output for SAPM plots
png(file = "SAPM.Indicies.png", width = 1500, height = 1850, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SAPM_Sac_GGPLOT, SAPM_SJ_GGPLOT,  ncol = 1,
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
write.csv(SPLT_Index_Final, "Output/SPLT_Index_Final.csv")

## Plots

SPLT_SJ_GGPLOT = ggplot(data= SPLT_Index_Final, aes(x=Year, y=San_Joaquin_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,15), expand = c(0, 0)) +
  theme_classic()
SPLT_SJ_GGPLOT

SPLT_Sac_GGPLOT = ggplot(data= SPLT_Index_Final, aes(x=Year, y=Sac_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0)) +
  theme_classic()
SPLT_Sac_GGPLOT

# Output for SPLT plots
png(file = "SPLT.Indicies.png", width = 1500, height = 1850, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SPLT_Sac_GGPLOT, SPLT_SJ_GGPLOT,  ncol = 1,
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
write.csv(SASU_Index_Final, "Output/SASU_Index_Final.csv")

## Plots

SASU_SJ_GGPLOT = ggplot(data= SASU_Index_Final, aes(x=Year, y=San_Joaquin_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  theme_classic()
SASU_SJ_GGPLOT

SASU_Sac_GGPLOT = ggplot(data= SASU_Index_Final, aes(x=Year, y=Sac_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,10), expand = c(0, 0)) +
  theme_classic()
SASU_Sac_GGPLOT

# Output for SASU plots
png(file = "SASU.Indicies.png", width = 1500, height = 1850, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SASU_Sac_GGPLOT, SASU_SJ_GGPLOT,  ncol = 1,
                                   top =textGrob("SASU", vjust=2,gp = gpar(fontsize=25,   fontfamily="Times")))))         

dev.off()
