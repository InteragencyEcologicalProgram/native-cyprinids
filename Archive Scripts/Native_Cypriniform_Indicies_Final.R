### Update with 2018/2019 catches?



# Background --------------------------------------------------------------

# Objectives of this study are:

# 1) Create time series of and determine if there are trends in age-0 abundance indcies in the Delta and the Sacramento and SJ rivers for Sacramento Splittial, Sucker, and Pikeminnow

# 2) Determine how mean flow during spawning, the timing of flow, and water temperature during first few months influence trends in abundance

# 3) Determine how mean flow during spawning, the timing of flow, and water temperature during first few months influence the center of distribution of these fishes on the Sacramento and SJ rivers.

# 3) I don't know if this is an objective (probably more of a point for the discussion) but we can show with the heatmaps that the south delta is not an important rearing ground for age-0 SAPM and SASU. 

### For Brian
# Can you think of any other objectives that would be good for the paper?
# Should I change any of the analyses (e.g., analyze all systems together instead of models for each region)?
# Look at catches of age-0 suckers (under Length Frequency Analyses -> SASU). The highest catches of age-0 fish occur in May and June but there is another pulse in December. Is there anything interesting we can do with this? 


# Packages ----------------------------------------------------------------
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('lubridate')) install.packages('lubridate'); library('lubridate')
if (!require('grid')) install.packages('grid'); library('grid')
if (!require('gridExtra')) install.packages('gridExtra'); library('gridExtra')
if (!require('psych')) install.packages('psych'); library('psych')
if (!require('MuMIn')) install.packages('MuMIn'); library('MuMIn')
if (!require('plotrix')) install.packages('plotrix'); library('plotrix')
if (!require('Kendall')) install.packages('Kendall'); library('Kendall')
if (!require('dataRetrieval')) install.packages('dataRetrieval'); library('dataRetrieval') # Used to get data from USGS
if (!require('car')) install.packages('car'); library('car')
if (!require('odbc')) install.packages('odbc'); library('odbc')
if (!require('extrafont')) install.packages('extrafont'); library('extrafont')
options(scipen=999)
windowsFonts(Times = windowsFont("Times New Roman"))


# Data --------------------------------------------------------------------
Cyprinid_Catch = read.csv("data/Cyprinid_Catch.CSV")
Cyprinid_Catch$Month = as.factor(Cyprinid_Catch$Month)
Cyprinid_Catch$Year = as.factor(Cyprinid_Catch$Year)
Cyprinid_Catch$RegionCode = as.factor(Cyprinid_Catch$RegionCode)
Cyprinid_Catch$subarea = as.factor(Cyprinid_Catch$subarea)

head(Cyprinid_Catch)

# SAPM
SAPM_Catch_Effort = read.csv("data/SAPM_Catch_Effort.csv")
SAPM_Catch_Effort$SampleID = as.factor(SAPM_Catch_Effort$SampleID)
SAPM_Catch_Effort$Month = as.factor(SAPM_Catch_Effort$Month)
SAPM_Catch_Effort$subarea = as.factor(SAPM_Catch_Effort$subarea)
SAPM_Catch_Effort$Year = as.factor(SAPM_Catch_Effort$Year)
SAPM_Catch_Effort$GearConditionCode = as.factor(SAPM_Catch_Effort$GearConditionCode)
str(SAPM_Catch_Effort)

head(SAPM_Catch_Effort)

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
SASU_Catch_Effort = read.csv("data/SASU_Catch_Effort_2.csv")
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

# River mile
# Will be used to determine center of distribution 
Site_RM = read.csv("Data/Site_Coords.csv")
head(Site_RM)
str(Site_RM)

#### Zooplankton Data
# Not going to use this due to poor overlap with the sampling stations
Zoop_Raw = read.csv("data/1972-2017CBMatrix.csv", header = T)
head(Zoop_Raw)
str(Zoop_Raw)
Zoop_Raw$Month = as.factor(month(as.POSIXlt(Zoop_Raw$Date, format = "%m/%d/%Y")))


# Length Frequency Analyses -----------------------------------------------

## Length Frequency: Staring with length frequency analyses/histrograms to determine what months and length cutoffs to use when creating age-0 abundance indices 


# Creating object to label x axis to better see length cutoffs for age-0 fish
FL_Labels = seq(0, 200, by = 10)


# SAPM --------------------------------------------------------------------

# Looking to see what months have the highest catches and to see how distributions change
Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", ForkLength, ForkLength > 25, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength, color = Area))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  facet_wrap(~Month)
# Looks like age-0 fish are showing up in months 5 and 6 but also high catches in months 3:4, 7, and 12
# Catches in month 12 (3rd highest catches of the year...) occur mainly in the Delta and Sacramento River
# Catches in the SJ occur mainly in months 5 and 6 (but are negligible in general)

# See if months with highest catches depend on year
Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", ForkLength, ForkLength < 60, Month %in% c(4:8, 12)) %>% 
  group_by(Year, Month, ForkLength) %>% 
  summarise(CatchCount = sum(SumOfCatchCount)) %>% 
  ggplot(aes(x = ForkLength, y = CatchCount))+
  geom_line(aes(color = Month))+
  facet_wrap(~Year)+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)
# The months with high catches appear to vary by year


# To see what months have highest catches of SAPM
SAPM_Catch_By_Month <- 
  Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", ForkLength < 60)%>%
  group_by(Month)%>% 
  summarise(Sum_Catch_Per_Month = sum(SumOfCatchCount))
SAPM_Catch_By_Month

# Highest catches in months 3 - 7
str(SAPM_Catch_By_Month)

SAPM_Catch_By_Month_Year <- 
  Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", ForkLength < 60)%>%
  group_by(Month, Year, Area)%>% 
  summarise(Sum_Catch_Per_Month = sum(SumOfCatchCount))
SAPM_Catch_By_Month_Year
SAPM_Catch_By_Month_Year$Month = as.numeric(SAPM_Catch_By_Month_Year$Month)
ggplot(data = SAPM_Catch_By_Month_Year, aes(x = Month, y = Sum_Catch_Per_Month, color = Area)) +
  geom_line()+
  facet_wrap(~ Year)

# Length frequency analyses

# March
Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", ForkLength, Month == 3, ForkLength > 24, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# April
Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", Month == 4, ForkLength > 24, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# May
Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", Month == 5, ForkLength > 24, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# Age-0 fish start showing up in June/July
# We are missing the left tail of the distribution...therefore probably missing important (potential) variability
# June
Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", Month == 6, ForkLength > 24, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# July
Cyprinid_Catch %>% 
  filter(OrganismCode == "SAPM", Month == 7, ForkLength > 24, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# Based on length freq histograms, age 0 fish don't start showing up until June and July
# June cuttoff for age-0 fish is ~ 60 mm
# July cuttoff for age-0 fish is ~ 70 mm


# SPLT --------------------------------------------------------------------

# Looking to see what months have the highest catches and to see how distributions change
Cyprinid_Catch %>% 
  filter(OrganismCode == "SPLT", ForkLength, ForkLength > 24, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  facet_wrap(~Month)
# Much easier than the other species. Only really have catches in May and June. Some catch in July though/ 

# See if months with highest catches depend on year
Cyprinid_Catch %>% 
  filter(OrganismCode == "SPLT", ForkLength, ForkLength > 24, ForkLength < 60, Month %in% 4:8) %>% 
  group_by(Year, Month, ForkLength) %>% 
  summarise(CatchCount = sum(SumOfCatchCount)) %>% 
  ggplot(aes(x = ForkLength, y = CatchCount))+
  geom_line(aes(color = Month))+
  facet_wrap(~Year)+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)
# Months with highest catch do not appear to vary by year

# To see what months have highest catches of SPLT
SPLT_Catch_By_Month <- 
  Cyprinid_Catch %>% 
  filter(OrganismCode == "SPLT", ForkLength < 200)%>%
  group_by(Month)%>% 
  summarise(Sum_Catch_Per_Month = sum(SumOfCatchCount))
SPLT_Catch_By_Month
# Highest catches in months 5 - 6...same as above

SPLT_Catch_By_Month_Year <- 
  Cyprinid_Catch %>% 
  filter(OrganismCode == "SPLT", ForkLength < 60, Month %in% c(4:7))%>%
  group_by(Month, Year, Area)%>% 
  summarise(Sum_Catch_Per_Month = sum(SumOfCatchCount))
SPLT_Catch_By_Month_Year
SPLT_Catch_By_Month_Year$Month = as.numeric(SPLT_Catch_By_Month_Year$Month)
ggplot(data = SPLT_Catch_By_Month_Year, aes(x = Month, y = Sum_Catch_Per_Month, color = Area)) +
  geom_point()+
  facet_wrap(~ Year, scales = "free_y")
## There are a couple of months where catch is higher in month 4 than 6, but this only occurs in one area (2 years in the Delta). Feel confident that we can just focus analyeses on months 5 and 6. 

## Another thing to think about is when catch is higher (i.e., month 5 vs 6). Later on I will try and explain the negative relationship (non-significant) between flow and abundance for SPLT on having to go farther upstream (an thus past yolo bypass) in years with low flow (this coefficient is positive after adding water temperature). I also found a negative realtionship between the center of dist for SPLT and flow. Therefore, it might be interesting to determine what month catch was higher and relate that to flow. High flows should mean they have to travel a shorter distance and catch would be higher in month 5. Brian: do you think this is worth taking a look at? 

# May
Cyprinid_Catch %>% 
  filter(OrganismCode == "SPLT", ForkLength, Month == 5, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength, fill = Year))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# June
Cyprinid_Catch %>% 
  filter(OrganismCode == "SPLT", ForkLength, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  facet_wrap(~Month)


# Based on length freq histograms, age 0 fish make up the majority of the catch during both months
# May cuttoff for age-0 fish is ~ 50 mm
# June cuttoff for age-0 fish is ~ 60 mm


# SASU --------------------------------------------------------------------

# To see what months have highest catches of SASU

# Looking to see what months have the highest catches and to see how distributions change
Cyprinid_Catch %>% 
    filter(OrganismCode == "SASU", ForkLength, ForkLength < 200) %>% 
    ggplot(aes(x = ForkLength, color = Area))+
    geom_histogram(fill = "black")+
    scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
    facet_wrap(~Month)
# Highest catches in months 5 and 6 but also high catches in months 3:4, 7, and 12
# Catches in month 12 (3rd highest catches of the year...) occur mainly in the Delta and Sacramento River
# Catches in the SJ occur mainly in months 5 and 6
# In general, catches are spread out over much more of the year than SPLT, or SAPM

# See if months with highest catches depend on year
Cyprinid_Catch %>% 
    filter(OrganismCode == "SASU", ForkLength, ForkLength < 60, Month %in% c(4:8, 12)) %>% 
    group_by(Year, Month, ForkLength) %>% 
    summarise(CatchCount = sum(SumOfCatchCount)) %>% 
    ggplot(aes(x = ForkLength, y = CatchCount))+
    geom_line(aes(color = Month))+
    facet_wrap(~Year)+
    scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
    theme_classic(base_size = 15)
# Catches tend to be highest in May and June
# In some years catches are highest in December...


# Highest catches in months 4 - 7, 12; Peak in 5 and 6


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

# December
Cyprinid_Catch %>% 
  filter(OrganismCode == "SASU", ForkLength, Month == 12, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# Going to use months 5 and 6 because they have the highest catches of age-0 fish
# May cuttoff for age-0 fish is ~ 50 mm
# June cuttoff for age-0 fish is ~ 65 mm
# I am also going to look at December because it's interesting to have a peak later in the year. They are thought to spawn upstream in tribs so the catches in December might have something to do with flow. Brian...any ideas?


# Plus Count Proportions --------------------------------------------------

## Plus Count proportions: Use FL of Age-0 fishes caught within a seine haul to proporiton FL of plus count fish

# I will be using FL of Age-0 fishes caught within a seine haul to proporiton FL of plus count fish
# Formula I am using is:

# Adjusted Count =  Total_Count * (FL_Range_Count/FL_Count)

# Total_count = All fish in one sample including plus count fish
# FL_Range_Count = Count of fish in the FL range that we are using
# FL_Count = Count of measured fish

# CPUE = Adjusted Cout/Seine Volume


# SAPM  -------------------------------------------------------------------

# Based on length freq histograms, age 0 fish don't start showing up until June and July
# June cuttoff for age-0 fish is ~ 60 mm
# July cuttoff for age-0 fish is ~ 70 mm

###### Original Age-0 Analyses

## June
SAPM_Plus_Count_June <- 
  SAPM_Catch_Effort %>% 
  filter(OrganismCode == "SAPM", ForkLength == 0, Month == 6) %>%
  group_by(SampleID) %>% 
  summarise(Plus_Count = sum(SumOfSumOfCatchCount))

SAPM_FL_Count_June <- 
  SAPM_Catch_Effort %>% 
  filter(OrganismCode == "SAPM", ForkLength > 0, Month == 6)%>%
  group_by(SampleID)%>% 
  summarise(FL_Count = sum(SumOfSumOfCatchCount))

# Count of all fish (including plus counts) that were measured in June
SAPM_Total_June <- 
  full_join(SAPM_FL_Count_June, SAPM_Plus_Count_June,  by = "SampleID")
  colnames(SAPM_Total_June) = c("SampleID", "FL_Count", "Plus_Count") 
  SAPM_Total_June[is.na(SAPM_Total_June)] = 0  
  SAPM_Total_June$Total_Count = SAPM_Total_June$FL_Count + SAPM_Total_June$Plus_Count

# Getting count of all age-0 fish that were caught in June
SAPM_FL_Range_June <- 
  SAPM_Catch_Effort %>% 
    filter(OrganismCode == "SAPM", ForkLength > 24, ForkLength <61, Month == 6) %>%
    group_by(SampleID) %>% 
    summarise(FL_Range = sum(SumOfSumOfCatchCount))

SAPM_Total_June_Final = left_join(SAPM_Total_June, SAPM_FL_Range_June, by = "SampleID")
colnames(SAPM_Total_June_Final) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
SAPM_Total_June_Final[is.na(SAPM_Total_June_Final)] = 0 
head(SAPM_Total_June_Final)

## July
SAPM_Plus_Count_July <- 
  SAPM_Catch_Effort %>% 
  filter(OrganismCode == "SAPM", ForkLength == 0, Month == 7) %>%
  group_by(SampleID) %>% 
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
  filter(OrganismCode == "SAPM", ForkLength > 24, ForkLength <71, Month == 7) %>%
  group_by(SampleID) %>% 
  summarise(FL_Range = sum(SumOfSumOfCatchCount))

SAPM_Total_July_Final = left_join(SAPM_Total_July, SAPM_FL_Range_July, by = "SampleID")
colnames(SAPM_Total_July_Final) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
SAPM_Total_July_Final[is.na(SAPM_Total_July_Final)] = 0 
head(SAPM_Total_July_Final)

## Combining all months
SAPM_Final <- 
  bind_rows(SAPM_Total_June_Final, SAPM_Total_July_Final)
head(SAPM_Final)
str(SAPM_Final)

# Calculating Adjusted Count
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
str(SAPM_Index)

SAPM_Index.NA <- 
  SAPM_Index %>% 
  filter(is.na(CPUE))
SAPM_Index.NA
# There was one NA in the index beacuse volume was not calculated for a seine haul. 
# No SAPM were caught so I am changing CPUE to 0

SAPM_Index$CPUE[is.na(SAPM_Index$CPUE)] = 0

# write.csv(SAPM_Index, "Data/SAPM_Index.csv")

# Age-1 -------------------------------------------------------------------


####### Looking at age-1 fish because the Delta is not an important rearing area for age-0 fish. I don't plan on including this in the paper. So you can skip to next section (SPLT) if you want.

# Age-1 fish cuttoffs
# March 100 mm
# April 120 mm

## March
SAPM_Plus_Count_March_Age_1 <- 
  SAPM_Catch_Effort %>% 
  filter(OrganismCode == "SAPM", ForkLength == 0, Month == 3)%>%
  group_by(SampleID)%>% 
  summarise(Plus_Count = sum(SumOfSumOfCatchCount))

SAPM_FL_Count_March_Age_1 <- 
  SAPM_Catch_Effort %>% 
  filter(OrganismCode == "SAPM", ForkLength > 0, Month == 3)%>%
  group_by(SampleID)%>% 
  summarise(FL_Count = sum(SumOfSumOfCatchCount))

SAPM_Total_March_Age_1 <- 
  full_join(SAPM_FL_Count_March_Age_1, SAPM_Plus_Count_March_Age_1,  by = "SampleID")
colnames(SAPM_Total_March_Age_1) = c("SampleID", "FL_Count", "Plus_Count") 
SAPM_Total_March_Age_1[is.na(SAPM_Total_March_Age_1)] = 0  
SAPM_Total_March_Age_1$Total_Count = SAPM_Total_March_Age_1$FL_Count + SAPM_Total_March_Age_1$Plus_Count

SAPM_FL_Range_March_Age_1 <- 
  SAPM_Catch_Effort %>% 
  filter(OrganismCode == "SAPM", ForkLength > 24, ForkLength <100, Month == 3)%>%
  group_by(SampleID)%>% 
  summarise(FL_Range = sum(SumOfSumOfCatchCount))

SAPM_Total_March_Final_Age_1 = left_join(SAPM_Total_March_Age_1, SAPM_FL_Range_March_Age_1, by = "SampleID")
colnames(SAPM_Total_March_Final_Age_1) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
SAPM_Total_March_Final_Age_1[is.na(SAPM_Total_March_Final_Age_1)] = 0 
head(SAPM_Total_March_Final_Age_1)

## April
SAPM_Plus_Count_April_Age_1 <- 
  SAPM_Catch_Effort %>% 
  filter(OrganismCode == "SAPM", ForkLength == 0, Month == 4)%>%
  group_by(SampleID)%>% 
  summarise(Plus_Count = sum(SumOfSumOfCatchCount))

SAPM_FL_Count_April_Age_1 <- 
  SAPM_Catch_Effort %>% 
  filter(OrganismCode == "SAPM", ForkLength > 0, Month == 4)%>%
  group_by(SampleID)%>% 
  summarise(FL_Count = sum(SumOfSumOfCatchCount))

SAPM_Total_April_Age_1 <- 
  full_join(SAPM_FL_Count_April_Age_1, SAPM_Plus_Count_April_Age_1,  by = "SampleID")
colnames(SAPM_Total_April_Age_1) = c("SampleID", "FL_Count", "Plus_Count") 
SAPM_Total_April_Age_1[is.na(SAPM_Total_April_Age_1)] = 0  
SAPM_Total_April_Age_1$Total_Count = SAPM_Total_April_Age_1$FL_Count + SAPM_Total_April_Age_1$Plus_Count

SAPM_FL_Range_April_Age_1 <- 
  SAPM_Catch_Effort %>% 
  filter(OrganismCode == "SAPM", ForkLength > 24, ForkLength <120, Month == 3)%>%
  group_by(SampleID)%>% 
  summarise(FL_Range = sum(SumOfSumOfCatchCount))

SAPM_Total_April_Final_Age_1 = left_join(SAPM_Total_April_Age_1, SAPM_FL_Range_April_Age_1, by = "SampleID")
colnames(SAPM_Total_April_Final_Age_1) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
SAPM_Total_April_Final_Age_1[is.na(SAPM_Total_April_Final_Age_1)] = 0 
head(SAPM_Total_April_Final_Age_1)

## Combining all months for age-1 fish
SAPM_Final_Age_1 <- 
  bind_rows(SAPM_Total_March_Final_Age_1, SAPM_Total_April_Final_Age_1)
str(SAPM_Final_Age_1)
head(SAPM_Final_Age_1)

SAPM_Final_Age_1$Adjusted_Count <-
  SAPM_Final_Age_1$Total_Count * (SAPM_Final_Age_1$FL_Range_Count/SAPM_Final_Age_1$FL_Count)
SAPM_Final_Age_1[is.na(SAPM_Final_Age_1)] = 0

SAPM_Index_Age_1 = full_join(subset(SAPM_Catch_Effort, !duplicated(SampleID)), SAPM_Final_Age_1, by = "SampleID")
head(SAPM_Index_Age_1, 50)
SAPM_Index_Age_1[,15:21][is.na(SAPM_Index_Age_1[,15:21])] = 0
head(SAPM_Index_Age_1)

SAPM_Index_Age_1 <- 
  SAPM_Index_Age_1[SAPM_Index_Age_1$Month %in% c(3:4),]
head(SAPM_Index_Age_1,15)

SAPM_Index_Age_1$CPUE <- 
  SAPM_Index_Age_1$Adjusted_Count/SAPM_Index_Age_1$SeineVolume
head(SAPM_Index_Age_1)

# GOing to remove records that do have have a volume/CPUE estimate
SAPM_Index_Age_1 <- 
  SAPM_Index_Age_1[!is.na(SAPM_Index_Age_1$CPUE),]
head(SAPM_Index_Age_1)
str(SAPM_Index_Age_1)

# SPLT --------------------------------------------------------------------
# Based on length freq histograms, age 0 fish make up the majority of the catch during both months
# May cuttoff for age-0 fish is ~ 50 mm
# June cuttoff for age-0 fish is ~ 60 mm

## May
SPLT_Plus_Count_May <- 
  SPLT_Catch_Effort %>% 
  filter(OrganismCode == "SPLT", ForkLength == 0, Month == 5) %>%
  group_by(SampleID) %>% 
  summarise(Plus_Count = sum(SumOfSumOfCatchCount))

SPLT_FL_Count_May <- 
  SPLT_Catch_Effort %>% 
  filter(OrganismCode == "SPLT", ForkLength > 0, Month == 5) %>%
  group_by(SampleID) %>% 
  summarise(FL_Count = sum(SumOfSumOfCatchCount))

SPLT_Total_May <- 
  full_join(SPLT_FL_Count_May, SPLT_Plus_Count_May,  by = "SampleID")
colnames(SPLT_Total_May) = c("SampleID", "FL_Count", "Plus_Count") 
SPLT_Total_May[is.na(SPLT_Total_May)] = 0  
SPLT_Total_May$Total_Count = SPLT_Total_May$FL_Count + SPLT_Total_May$Plus_Count

SPLT_FL_Range_May <- 
  SPLT_Catch_Effort %>% 
  filter(OrganismCode == "SPLT", ForkLength > 24, ForkLength <51, Month == 5) %>%
  group_by(SampleID) %>% 
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

SPLT_Index.NA <- 
  SPLT_Index %>% 
  filter(is.na(CPUE))
SPLT_Index.NA

# There are 2 instnaces of when volume was not calculated but SPLT were caught. Going to remove those samples.

SPLT_Index <- 
  SPLT_Index %>% 
  filter(SampleID != "261621", SampleID != "262199")

# There was one NA in the index beacuse volume was not calculated for a seine haul...changing CPUE to 0

SPLT_Index$CPUE[is.na(SPLT_Index$CPUE)] = 0

SPLT_Index.NA <- 
  SPLT_Index %>% 
  filter(is.na(CPUE))
SPLT_Index.NA
# No more NAs


# SASU --------------------------------------------------------------------
# Going to use months 5 and 6 because they have the highest catches
# Based on length freq histograms, age 0 fish make up the majority of the catch during both months
# May cuttoff for age-0 fish is ~ 50 mm
# June cuttoff for age-0 fish is ~ 65 mm

## May
SASU_Plus_Count_May <- 
  SASU_Catch_Effort %>% 
  filter(OrganismCode == "SASU", ForkLength == 0, Month == 5) %>%
  group_by(SampleID) %>% 
  summarise(Plus_Count = sum(SumOfSumOfCatchCount))

SASU_FL_Count_May <- 
  SASU_Catch_Effort %>% 
  filter(OrganismCode == "SASU", ForkLength > 0, Month == 5) %>%
  group_by(SampleID) %>% 
  summarise(FL_Count = sum(SumOfSumOfCatchCount))

SASU_Total_May <- 
  full_join(SASU_FL_Count_May, SASU_Plus_Count_May,  by = "SampleID")
colnames(SASU_Total_May) = c("SampleID", "FL_Count", "Plus_Count") 
SASU_Total_May[is.na(SASU_Total_May)] = 0  
SASU_Total_May$Total_Count = SASU_Total_May$FL_Count + SASU_Total_May$Plus_Count

SASU_FL_Range_May <- 
  SASU_Catch_Effort %>% 
  filter(OrganismCode == "SASU", ForkLength > 24, ForkLength < 51, Month == 5) %>%
  group_by(SampleID) %>% 
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

## December
SASU_Plus_Count_December <- 
  SASU_Catch_Effort %>% 
  filter(OrganismCode == "SASU", ForkLength == 0, Month == 12)%>%
  group_by(SampleID)%>% 
  summarise(Plus_Count = sum(SumOfSumOfCatchCount))

SASU_FL_Count_December <- 
  SASU_Catch_Effort %>% 
  filter(OrganismCode == "SASU", ForkLength > 0, Month == 12)%>%
  group_by(SampleID)%>% 
  summarise(FL_Count = sum(SumOfSumOfCatchCount))

SASU_Total_December <- 
  full_join(SASU_FL_Count_December, SASU_Plus_Count_December,  by = "SampleID")
colnames(SASU_Total_December) = c("SampleID", "FL_Count", "Plus_Count") 
SASU_Total_December[is.na(SASU_Total_December)] = 0  
SASU_Total_December$Total_Count = SASU_Total_December$FL_Count + SASU_Total_December$Plus_Count

SASU_FL_Range_December <- 
  SASU_Catch_Effort %>% 
  filter(OrganismCode == "SASU", ForkLength > 24, ForkLength <66, Month == 12)%>%
  group_by(SampleID)%>% 
  summarise(FL_Range = sum(SumOfSumOfCatchCount))

SASU_Total_December_Final = left_join(SASU_Total_December, SASU_FL_Range_December, by = "SampleID")
colnames(SASU_Total_December_Final) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
SASU_Total_December_Final[is.na(SASU_Total_December_Final)] = 0 
head(SASU_Total_December_Final)

## Combining all months
SASU_Final <- 
  bind_rows(SASU_Total_May_Final, SASU_Total_June_Final, SASU_Total_December_Final)
str(SASU_Final)

SASU_Final$Adjusted_Count <-
  SASU_Final$Total_Count * (SASU_Final$FL_Range_Count/SASU_Final$FL_Count)
SASU_Final[is.na(SASU_Final)] = 0

SASU_Index = full_join(subset(SASU_Catch_Effort, !duplicated(SampleID)), SASU_Final, by = "SampleID")
head(SASU_Index)
SASU_Index[,15:21][is.na(SASU_Index[,15:21])] = 0
head(SASU_Index)

SASU_Index <- 
  SASU_Index[SASU_Index$Month %in% c(5:6, 12),]
head(SASU_Index,15)

SASU_Index$CPUE <- 
  SASU_Index$Adjusted_Count/SASU_Index$SeineVolume
head(SASU_Index, 15)


SASU_Index.NA <- 
  SASU_Index %>% 
  filter(is.na(CPUE))
SASU_Index.NA


# There are 3 instnaces of when volume was not calculated but SPLT were caught. Going to remove those samples.

SASU_Index <- 
  SASU_Index %>% 
  filter(SampleID != "261621", SampleID != "262199", SampleID != "269855")

# There was one NA in the index beacuse volume was not calculated for a seine haul...changing CPUE to 0

SASU_Index$CPUE[is.na(SASU_Index$CPUE)] = 0


# Heat Maps ---------------------------------------------------------------

# Looking at CPUEs by site to see how catches are distributed
# These show that there are low catches/CPUEs in the southern delta for SAPM and SASU


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
# There are quite a few stations that were not sampled at all espically in the SJ

# Output for SAPM Station HM plots
png(file = "Figures/SAPM_Station_HM.png", width = 1500, height = 1850, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SAPM_Heat_Map_Plot, ncol = 1)))         

dev.off()

# Heat maps for subareas
SAPM_Subarea_HM <- 
  SAPM_Index %>%
  group_by(StationCode, Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, subarea) %>% 
  summarise(MeanCPUE = mean(CPUE)) 

SAPM_Subarea_Heat_Map_Plot <- 
  ggplot(SAPM_Subarea_HM, aes(Year, subarea)) +
  geom_tile(aes(fill = MeanCPUE), color = "white") +
  ggtitle("Pikeminnow") +
  ylab("Subarea") +
  theme(plot.title = element_text(hjust=0.5, size = 60), legend.text = element_text (size = 25), text = element_text(size=40)) + 
  scale_x_discrete(breaks=seq(1995, 2018, 2)) + 
  scale_fill_gradient(low = "white", high = "steelblue")
SAPM_Subarea_Heat_Map_Plot

# Subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River
# Subarea 8 is no longer sampled
# Most catches occured in Sac region with very little catch in SJ
# Some catch in Delta region, espcially in subarea 5 (which is northern part of region on the Sac River)

# Output for SAPM Subarea HM plots
png(file = "Figures/SAPM_Subarea_HM.png", width = 4000, height = 3000, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SAPM_Subarea_Heat_Map_Plot, ncol = 1)))         

dev.off()

### Age-1 fish
SAPM_Subarea_HM_Age_1 <- 
  SAPM_Index_Age_1 %>%
  group_by(StationCode, Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, subarea) %>% 
  summarise(MeanCPUE = mean(CPUE)) 

# Here is the heat map for Age 1 SAPM. Still low catches in the SJ but there are many more catches throughout the Delta Region. This makes sense because they are supposed to spawn/rear on tribs outside of the Delta.
SAPM_Subarea_Heat_Map_Plot_Age_1 <- 
  ggplot(SAPM_Subarea_HM_Age_1, aes(Year, subarea)) +
  geom_tile(aes(fill = MeanCPUE), color = "white") +
  ggtitle("SAPM") +
  ylab("Subarea") +
  theme(plot.title = element_text(hjust=0.5, size = 60), legend.text = element_text (size = 25), text = element_text(size=40)) + 
  scale_x_discrete(breaks=seq(1995, 2018, 2)) + 
  scale_fill_gradient(low = "white", high = "steelblue")
SAPM_Subarea_Heat_Map_Plot_Age_1


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
  group_by(StationCode, Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, subarea) %>% 
  summarise(MeanCPUE = mean(CPUE))

SPLT_Subarea_Heat_Map_Plot <- 
  ggplot(SPLT_Subarea_HM, aes(Year, subarea)) +
  geom_tile(aes(fill = MeanCPUE), color = "white") +
  ggtitle("Splittail") +
  ylab("Subarea") +
  theme(plot.title = element_text(hjust=0.5, size = 60), legend.text = element_text (size = 25), text = element_text(size=40)) + 
  scale_x_discrete(breaks=seq(1995, 2018, 2)) + 
  scale_fill_gradient(low = "white", high = "steelblue")
SPLT_Subarea_Heat_Map_Plot

# Subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River
# Better catches throughout all subareas relative to SAPM
# Either high CPUES or 0s in SJ
# More moderated values in Sac

# Output for SPLT Subarea HM plots
png(file = "Figures/SPLT_Subarea_HM.png", width = 4000, height = 3000, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SPLT_Subarea_Heat_Map_Plot, ncol = 1)))         

dev.off()

# SASU --------------------------------------------------------------------
SASU_HM <- 
  SASU_Index %>% 
  filter(Month != 12) %>% # I decided to look how flows influenced catches in december but needed to remove it from this HM
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
  filter(Month != 12) %>% 
  group_by(StationCode, Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, subarea) %>% 
  summarise(MeanCPUE = mean(CPUE))


SASU_Subarea_Heat_Map_Plot <- 
  ggplot(SASU_Subarea_HM, aes(Year, subarea)) +
  geom_tile(aes(fill = MeanCPUE), color = "white") +
  ggtitle("Sucker") +
  ylab("Subarea") +
  theme(plot.title = element_text(hjust=0.5, size = 60), legend.text = element_text (size = 25), text = element_text(size=40)) + 
  scale_x_discrete(breaks=seq(1995, 2018, 2)) + 
  scale_fill_gradient(low = "white", high = "steelblue")
SASU_Subarea_Heat_Map_Plot

# Subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River
# Simillar to SASU, high CPUEs in Sac and low in SJ
# Some catches in subareas 4 and 5 in the Delta Region

# Output for SASU Subarea HM plots
png(file = "Figures/SASU_Subarea_HM.png", width = 4000, height = 3000, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SASU_Subarea_Heat_Map_Plot, ncol = 1)))         

dev.off()    


# Indicies ----------------------------------------------------------------

## Age-0 Indicies: Using plus count proportioned data to create age-0 abundance indices

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

##### Subarea 8 is no longer beind sampled...removing subarea 8 to see if it influences results of Sac Models
SAPM_Index_8_Final <-
SAPM_Index %>%
  filter(subarea %in% c(1:7, 9:10)) %>%
  group_by(StationCode, Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  spread(subarea, CPUE)

# Summing subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River
SAPM_Index_8_Final$Delta_Index = rowSums(SAPM_Index_8_Final[,2:6])
SAPM_Index_8_Final$Sac_River_Index = rowSums(SAPM_Index_8_Final[,7:8]) 
SAPM_Index_8_Final$San_Joaquin_River_Index = rowSums(SAPM_Index_8_Final[,9:10], na.rm = T) # na.rm = T because we didn't sample in subarea 10 in 1995 and 1996

SAPM_Index_8_Final
SAPM_Index_Final


# Age-1 -------------------------------------------------------------------


### Age-1 fish
# Calculating mean March and April CPUE for each station 
SAPM_Index_Final_Age_1 <- 
  SAPM_Index_Age_1 %>%
  group_by(StationCode, Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  spread(subarea, CPUE)

# Summing subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River
SAPM_Index_Final_Age_1$Delta_Index = rowSums(SAPM_Index_Final_Age_1[,2:6])
SAPM_Index_Final_Age_1$Sac_River_Index = rowSums(SAPM_Index_Final_Age_1[,7:9], na.rm = T) # na.rm = T because we are no longer sampling in subarea 8
SAPM_Index_Final_Age_1$San_Joaquin_River_Index = rowSums(SAPM_Index_Final_Age_1[,10:11], na.rm = T) # na.rm = T because we didn't sample in subarea 10 in 1995 and 1996

head(SAPM_Index_Final_Age_1)
tail(SAPM_Index_Final_Age_1)
SAPM_Index_Final_Age_1 = filter(SAPM_Index_Final_Age_1, Year %in% c(1995:2018))
# I need to think about how/if I should be offsetting by a year to match up with age-1 fish. Am I more interested in determining what influences the recruitment success/survival of age-0 fish (but they are in the Delta so I have to infer from age-1 fish) or do I want to try and determine what influences to "occupancy" of the Delta by age-1 fish?
write.csv(SAPM_Index_Final_Age_1, "Output/SAPM_Index_Final_Age_1.csv")

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


##### Subarea 8 is no longer beind sampled...removing subarea 8 to see if it influences results of Sac Models
SPLT_Index_8_Final <-
  SPLT_Index %>%
  filter(subarea %in% c(1:7, 9:10)) %>%
  group_by(StationCode, Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  spread(subarea, CPUE)


# Summing subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River
SPLT_Index_8_Final$Delta_Index = rowSums(SPLT_Index_8_Final[,2:6])
SPLT_Index_8_Final$Sac_River_Index = rowSums(SPLT_Index_8_Final[,7:8]) 
SPLT_Index_8_Final$San_Joaquin_River_Index = rowSums(SPLT_Index_8_Final[,9:10], na.rm = T) # na.rm = T because we didn't sample in subarea 10 in 1995 and 1996

SPLT_Index_8_Final

## Plots

# Plots are not behaving using the tibble or whatever it is. I got the final data from below to make these plots

SPLT_SJ_Final  

SPLT_SJ_GGPLOT = ggplot(data= SPLT_SJ_Final, aes(x=Year, y=San_Joaquin_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0), drop = FALSE, breaks=seq(1995, 2017, 2)) +
  scale_y_continuous(limits = c(0,15), expand = c(0, 0)) +
  ylab("San Joaquin Index") +
  xlab("") +
  theme_classic(base_size = 18)
SPLT_SJ_GGPLOT

SPLT_Sac_GGPLOT = ggplot(data= SPLT_SAC_Final, aes(x=Year, y=Sac_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0), drop = FALSE, breaks=seq(1995, 2017, 2)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0)) +
  ylab("Sacramento Index") +
  xlab("") +
  theme_classic(base_size = 18)
SPLT_Sac_GGPLOT

SPLT_Delta_GGPLOT = ggplot(data= SPLT_DELTA_Final, aes(x=Year, y=DELTA_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0), drop = FALSE, breaks=seq(1995, 2017, 2)) +
  scale_y_continuous(limits = c(0,20), expand = c(0, 0)) +
  ylab("Delta Index") + 
  xlab("") +
  theme_classic(base_size = 18)
SPLT_Delta_GGPLOT

# Output for SPLT plots
png(file = "Figures/SPLT.Indicies.png", width = 1250, height = 1250, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SPLT_Sac_GGPLOT, SPLT_Delta_GGPLOT, SPLT_SJ_GGPLOT,  ncol = 1,
                                   top =textGrob("", vjust=2,gp = gpar(fontsize=25,   fontfamily="Times")))))         

dev.off()




# SASU --------------------------------------------------------------------

# Calculating mean May and June CPUE for each station 
SASU_Index_Final <- 
  SASU_Index %>%
  filter(Month != 12) %>% 
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

# Calculating index for December
# Calculating mean May and June CPUE for each station 
SASU_Index_Final_Dec <- 
  SASU_Index %>%
  filter(Month == 12) %>% 
  group_by(StationCode, Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  spread(subarea, CPUE)

# Summing subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River
SASU_Index_Final_Dec$Delta_Index = rowSums(SASU_Index_Final_Dec[,2:6], na.rm = T)
SASU_Index_Final_Dec$Sac_River_Index = rowSums(SASU_Index_Final_Dec[,7:9], na.rm = T) # na.rm = T because we are no longer sampling in subarea 8
SASU_Index_Final_Dec$San_Joaquin_River_Index = rowSums(SASU_Index_Final_Dec[,10:11], na.rm = T) 

SASU_Index_Final_Dec

##### Subarea 8 is no longer beind sampled...removing subarea 8 to see if it influences results of Sac Models
SASU_Index_8_Final <-
  SASU_Index %>%
  filter(subarea %in% c(1:7, 9:10), Month != 12) %>%
  group_by(StationCode, Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  spread(subarea, CPUE)


# Summing subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River
SASU_Index_8_Final$Delta_Index = rowSums(SASU_Index_8_Final[,2:6])
SASU_Index_8_Final$Sac_River_Index = rowSums(SASU_Index_8_Final[,7:8]) 
SASU_Index_8_Final$San_Joaquin_River_Index = rowSums(SASU_Index_8_Final[,9:10], na.rm = T) # na.rm = T because we didn't sample in subarea 10 in 1995 and 1996

SASU_Index_8_Final

## December w/o subarea 8

SASU_Index_8_Dec_Final <-
  SASU_Index %>%
  filter(subarea %in% c(1:7, 9:10), Month == 12) %>%
  group_by(StationCode, Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Month, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, subarea) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  spread(subarea, CPUE)


# Summing subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River
SASU_Index_8_Dec_Final$Delta_Index_December = rowSums(SASU_Index_8_Dec_Final[,2:6], na.rm = T)
SASU_Index_8_Dec_Final$Sac_River_Index_December = rowSums(SASU_Index_8_Dec_Final[,7:8], na.rm = T) # na.rm = T because we are no longer sampling in subarea 8
SASU_Index_8_Dec_Final$San_Joaquin_River_Index_December = rowSums(SASU_Index_8_Dec_Final[,9:10], na.rm = T) # na.rm = T because we didn't sample in subarea 10 in 1995 and 1996

SASU_Index_8_Dec_Final



## Plots

# Plots are not behaving using the tibble or whatever it is. I got the final data from below to make these plots

SASU_SJ_Final  

SASU_SJ_GGPLOT = ggplot(data= SASU_SJ_Final, aes(x=Year, y=San_Joaquin_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0), drop = FALSE, breaks=seq(1995, 2017, 2)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  ylab("San Joaquin Index") +
  xlab("") +
  theme_classic(base_size = 18)
SASU_SJ_GGPLOT

SASU_Sac_GGPLOT = ggplot(data= SASU_SAC_Final, aes(x=Year, y=Sac_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0), drop = FALSE, breaks=seq(1995, 2017, 2)) +
  scale_y_continuous(limits = c(0,10), expand = c(0, 0)) +
  ylab("Sacramento Index") +
  xlab("") +
  theme_classic(base_size = 18)
SASU_Sac_GGPLOT

SASU_Delta_GGPLOT = ggplot(data= SASU_DELTA_Final, aes(x=Year, y=DELTA_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0), drop = FALSE, breaks=seq(1995, 2017, 2)) +
  scale_y_continuous(limits = c(0,2.5), expand = c(0, 0)) +
  ylab("Delta Index") + 
  xlab("") +
  theme_classic(base_size = 18)
SASU_Delta_GGPLOT

# Output for SASU plots
png(file = "Figures/SASU.Indicies.png", width = 1250, height = 1250, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SASU_Sac_GGPLOT, SASU_Delta_GGPLOT, SASU_SJ_GGPLOT,  ncol = 1,
                                   top =textGrob("", vjust=2,gp = gpar(fontsize=25,   fontfamily="Times")))))         

dev.off()

# Center of Dist --------------------------------------------------------------

## Centroid
# Looking at center of distribution
# Formula taken from Sommer et al. 2011
# CD = sum(RiverKilo * CPUE)/sum(CPUE)
# Going to be used as response variable in model with flow/temp as covariates 


# SAPM --------------------------------------------------------------------

head(SAPM_Index)

head(Site_RM)

SAPM_RM = left_join(SAPM_Index, Site_RM, by = "StationCode")
head(SAPM_RM)
SAPM_RM = SAPM_RM[SAPM_RM$CPUE != 0,] # Removing records where CPUE was 0

# Getting CPUEs for Sacramento River
SAPM_RM_Sac <-
  SAPM_RM %>% 
  filter(River == "Sac")
head(SAPM_RM_Sac)

# Getting CPUEs for SJ River
SAPM_RM_SJ <-
  SAPM_RM %>% 
  filter(River == "SJ")
head(SAPM_RM_SJ)

SAPM_CDist_SAC <- 
  SAPM_RM_Sac %>% 
  filter(Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(CDist = sum(RiverKilo * CPUE)/sum(CPUE))
head(SAPM_CDist_SAC)


SAPM_CDist_SJ <- 
  SAPM_RM_SJ %>% 
  filter(Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(CDist = sum(RiverKilo * CPUE)/sum(CPUE))
head(SAPM_CDist_SJ)


# SPLT --------------------------------------------------------------------

head(SPLT_Index)

head(Site_RM)

SPLT_RM = left_join(SPLT_Index, Site_RM, by = "StationCode")
head(SPLT_RM)
SPLT_RM = SPLT_RM[SPLT_RM$CPUE != 0,] # Removing records where CPUE was 0

SPLT_RM_Sac <-
  SPLT_RM %>% 
  filter(River == "Sac")
head(SPLT_RM_Sac)

SPLT_RM_SJ <-
  SPLT_RM %>% 
  filter(River == "SJ")
head(SPLT_RM_SJ)

SPLT_CDist_SAC <- 
  SPLT_RM_Sac %>% 
  filter(Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(CDist = sum(RiverKilo * CPUE)/sum(CPUE))
head(SPLT_CDist_SAC)


SPLT_CDist_SJ <- 
  SPLT_RM_SJ %>% 
  filter(Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(CDist = sum(RiverKilo * CPUE)/sum(CPUE))
head(SPLT_CDist_SJ)



# SASU --------------------------------------------------------------------

head(SASU_Index)
head(SASU_Index)

head(Site_RM)
head(Site_RM)

SASU_RM = left_join(SASU_Index, Site_RM, by = "StationCode")
head(SASU_RM)
SASU_RM = SASU_RM[SASU_RM$CPUE != 0,] # Removing records where CPUE was 0
head(SASU_RM)

SASU_RM_Sac <-
  SASU_RM %>% 
  filter(River == "Sac")
head(SASU_RM_Sac)
tail(SASU_RM_Sac)

SASU_RM_SJ <-
  SASU_RM %>% 
  filter(River == "SJ")
head(SASU_RM_SJ)

SASU_CDist_SAC <- 
  SASU_RM_Sac %>% 
  filter(Year %in% c(1995:2017), Month != 12) %>% 
  group_by(Year) %>% 
  summarise(CDist = sum(RiverKilo * CPUE)/sum(CPUE))
head(SASU_CDist_SAC)

SASU_CDist_SAC_Dec <- 
  SASU_RM_Sac %>% 
  filter(Year %in% c(1995:2017), Month == 12) %>% 
  group_by(Year) %>% 
  summarise(CDist = sum(RiverKilo * CPUE)/sum(CPUE))
head(SASU_CDist_SAC_Dec)

SASU_CDist_SJ <- 
  SASU_RM_SJ %>% 
  filter(Year %in% c(1995:2017), Month != 12) %>% 
  group_by(Year) %>% 
  summarise(CDist = sum(RiverKilo * CPUE)/sum(CPUE))
head(SASU_CDist_SJ)

# December catches of SASU largely only occur in the Sac so I am not calculating a center of dist for SJ


# Discharge ---------------------------------------------------------------

head(Discharge)

## Discharge data is taken DNR Day Flow data
# SJ = SJR
# Sac = SAC - not going to use YOLO because the sites are upstream of the bypass
# Delta = OUT
# I calculated discharge for before, during, and after spawning but am just going to use mean discharge during spawning
##### Brian...are these what you would use???


## Centroid
# Looking at peak flow (PF) by julian day in each region (really this is timing of flow)
# Formula taken from Sommer et al. 2011
# PF = sum(Julian day * flow)/sum(flow)
# Going to calculate PF for Jan - May because peak spawning for all species should have started by May and high flows earlier may have allowed them to reach the spawning grounds


# Creating julian day in discharge dataframe
JD_Dis = as.POSIXlt(Discharge$Date, format = "%d-%b-%y")

Discharge$Julian_Date <- JD_Dis$yday + 1
# I added 1 because this isn't really Julian Date...starts with 0 on Jan 1...so it would have messed up the PF formula


# Timing of flow by year 
# Going to use for all 3 species
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

### Peak flow data is broken up into before spawning (BS), during spawning (DS), and after spawning (AS) but probably not going to use this...

## SAPM
# Peak spawning in April and May
# Used June and July for indices

# SJ
Peak_Flow_SJR_SAPM_BS <- 
  Discharge %>% 
  filter(Mo %in% 1:3 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * SJR)/sum(SJR))
head(Peak_Flow_SJR)

Peak_Flow_SJR_SAPM_DS <- 
  Discharge %>% 
  filter(Mo %in% 4:5 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * SJR)/sum(SJR))
head(Peak_Flow_SJR)

Peak_Flow_SJR_SAPM_AS <- 
  Discharge %>% 
  filter(Mo %in% 6:7 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * SJR)/sum(SJR))
head(Peak_Flow_SJR)

# Sac
Peak_Flow_SAC_SAPM_BS <- 
  Discharge %>% 
  filter(Mo %in% 1:3 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * SAC)/sum(SAC))
head(Peak_Flow_SAC)

Peak_Flow_SAC_SAPM_DS <- 
  Discharge %>% 
  filter(Mo %in% 4:5 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * SAC)/sum(SAC))
head(Peak_Flow_SAC)

Peak_Flow_SAC_SAPM_AS <- 
  Discharge %>% 
  filter(Mo %in% 6:7 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * SAC)/sum(SAC))
head(Peak_Flow_SAC)

# Delta
Peak_Flow_DELTA_SAPM_BS <- 
  Discharge %>% 
  filter(Mo %in% 1:3 & Year %in% c(1996:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * OUT)/sum(OUT))
head(Peak_Flow_DELTA_SAPM_BS)

Peak_Flow_DELTA_SAPM_DS <- 
  Discharge %>% 
  filter(Mo %in% 4:5 & Year %in% c(1996:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * OUT)/sum(OUT))
head(Peak_Flow_DELTA_SAPM_DS)

Peak_Flow_DELTA_SAPM_AS <- 
  Discharge %>% 
  filter(Mo %in% 6:7 & Year %in% c(1996:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * OUT)/sum(OUT))
head(Peak_Flow_DELTA_SAPM_AS)

### Going to use mean discharge data for April and May (during peak spawning)

## San Joaquin
SJ_Mean_Discharge_SAPM <- 
  Discharge %>% 
  filter(Mo %in% 4:5 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SJR_Discharge = mean(SJR))
SJ_Mean_Discharge_SAPM

SJ_Mean_Discharge_SAPM_BS <- 
  Discharge %>% 
  filter(Mo %in% 2:3 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SJR_Discharge = mean(SJR))
SJ_Mean_Discharge_SAPM_BS

SJ_Mean_Discharge_SAPM_AS <- 
  Discharge %>% 
  filter(Mo %in% 6:7 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SJR_Discharge = mean(SJR))
SJ_Mean_Discharge_SAPM_AS

# Combining PF and mean discharge for SJ
SJ_Discharge_SAPM <- 
  cbind(SJ_Mean_Discharge_SAPM, Peak_Flow_SJR$PF, SJ_Mean_Discharge_SAPM_BS$Mean_SJR_Discharge, SJ_Mean_Discharge_SAPM_AS$Mean_SJR_Discharge)
SJ_Discharge_SAPM

colnames(SJ_Discharge_SAPM) <- c("Year", "Mean_SJR_Discharge", "Peak_Flow", "Mean_SJR_Discharge_BS", "Mean_SJR_Discharge_AS")
SJ_Discharge_SAPM

## Sacramento
SAC_Mean_Discharge_SAPM <- 
  Discharge %>% 
  filter(Mo %in% 4:5 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SACR_Discharge = mean(SAC))
SAC_Mean_Discharge_SAPM

SAC_Mean_Discharge_SAPM_BS <- 
  Discharge %>% 
  filter(Mo %in% 2:3 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SACR_Discharge_BS = mean(SAC))
SAC_Mean_Discharge_SAPM_BS

SAC_Mean_Discharge_SAPM_DS <- 
  Discharge %>% 
  filter(Mo %in% 4:5 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SACR_Discharge_DS = mean(SAC))
SAC_Mean_Discharge_SAPM_DS

SAC_Mean_Discharge_SAPM_AS <- 
  Discharge %>% 
  filter(Mo %in% 5:6 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SACR_Discharge_AS = mean(SAC))
SAC_Mean_Discharge_SAPM_AS

# Combining PF and mean discharge for SAC
SAC_Discharge_SAPM <- 
  cbind(SAC_Mean_Discharge_SAPM, Peak_Flow_SAC$PF, SAC_Mean_Discharge_SAPM_BS$Mean_SACR_Discharge_BS, SAC_Mean_Discharge_SAPM_AS$Mean_SACR_Discharge_AS)
SAC_Discharge_SAPM

colnames(SAC_Discharge_SAPM) <- c("Year", "Mean_SACR_Discharge", "Peak_Flow", "Mean_SACR_Discharge_BS", "Mean_SACR_Discharge_AS")
SAC_Discharge_SAPM

## Delta
DELTA_Mean_Discharge_SAPM <- 
  Discharge %>% 
  filter(Mo %in% 4:5 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_DELTA_Discharge = mean(OUT))
DELTA_Mean_Discharge_SAPM

DELTA_Mean_Discharge_SAPM_BS <- 
  Discharge %>% 
  filter(Mo %in% 2:3 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_DELTA_Discharge_BS = mean(OUT))
DELTA_Mean_Discharge_SAPM_BS

# Combining PF and mean discharge for DELTA
DELTA_Discharge_SAPM <- 
  cbind(DELTA_Mean_Discharge_SAPM, Peak_Flow_DELTA$PF, DELTA_Mean_Discharge_SAPM_BS$Mean_DELTA_Discharge_BS)
DELTA_Discharge_SAPM

colnames(DELTA_Discharge_SAPM) <- c("Year", "Mean_DELTA_Discharge", "Peak_Flow", "Mean_DELTA_Discharge_BS")
head(DELTA_Discharge_SAPM)


# SPLT --------------------------------------------------------------------

## SPLT
# Peak spawning in April and May
# Used June and July for indices

# SJ
Peak_Flow_SJR_SPLT_BS <- 
  Discharge %>% 
  filter(Mo %in% 1:2 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * SJR)/sum(SJR))
head(Peak_Flow_SJR)

Peak_Flow_SJR_SPLT_DS <- 
  Discharge %>% 
  filter(Mo %in% 3:4 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * SJR)/sum(SJR))
head(Peak_Flow_SJR)

Peak_Flow_SJR_SPLT_AS <- 
  Discharge %>% 
  filter(Mo %in% 5:6 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * SJR)/sum(SJR))
head(Peak_Flow_SJR)

# Sac
Peak_Flow_SAC_SPLT_BS <- 
  Discharge %>% 
  filter(Mo %in% 1:2 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * SAC)/sum(SAC))
head(Peak_Flow_SAC)

Peak_Flow_SAC_SPLT_DS <- 
  Discharge %>% 
  filter(Mo %in% 3:4 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * SAC)/sum(SAC))
head(Peak_Flow_SAC)

Peak_Flow_SAC_SPLT_AS <- 
  Discharge %>% 
  filter(Mo %in% 5:6 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * SAC)/sum(SAC))
head(Peak_Flow_SAC)

# Delta
Peak_Flow_DELTA_SPLT_BS <- 
  Discharge %>% 
  filter(Mo %in% 1:2 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * OUT)/sum(OUT))
head(Peak_Flow_DELTA_SPLT_BS)

Peak_Flow_DELTA_SPLT_DS <- 
  Discharge %>% 
  filter(Mo %in% 3:4 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * OUT)/sum(OUT))
head(Peak_Flow_DELTA_SPLT_DS)

Peak_Flow_DELTA_SPLT_AS <- 
  Discharge %>% 
  filter(Mo %in% 5:6 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(PF = sum(Julian_Date * OUT)/sum(OUT))
head(Peak_Flow_DELTA_SPLT_AS)


## Mean flow
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

SJ_Mean_Discharge_SPLT_BS <- 
  Discharge %>% 
  filter(Mo %in% 1:2 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SJR_Discharge = mean(SJR))

SJ_Mean_Discharge_SPLT_AS <- 
  Discharge %>% 
  filter(Mo %in% 5:6 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SJR_Discharge = mean(SJR))



# Combining PF and mean discharge for SJ
SJ_Discharge_SPLT <- 
  cbind(SJ_Mean_Discharge_SPLT, Peak_Flow_SJR$PF, SJ_Mean_Discharge_SPLT_BS$Mean_SJR_Discharge, SJ_Mean_Discharge_SPLT_AS$Mean_SJR_Discharge)
SJ_Discharge_SPLT

colnames(SJ_Discharge_SPLT) <- c("Year", "Mean_SJR_Discharge", "Peak_Flow", "Mean_SJR_Discharge_BS", "Mean_SJR_Discharge_AS")
head(SJ_Discharge_SPLT)

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

SAC_Mean_Discharge_SPLT_BS <- 
  Discharge %>% 
  filter(Mo %in% 1:2 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SACR_Discharge = mean(SAC))

SAC_Mean_Discharge_SPLT_AS <- 
  Discharge %>% 
  filter(Mo %in% 5:6 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SACR_Discharge = mean(SAC))

# Combining PF and mean discharge for SAC
SAC_Discharge_SPLT <- 
  cbind(SAC_Mean_Discharge_SPLT, Peak_Flow_SAC$PF, SAC_Mean_Discharge_SPLT_BS$Mean_SACR_Discharge, SAC_Mean_Discharge_SPLT_AS$Mean_SACR_Discharge)
head(SAC_Discharge_SPLT)

colnames(SAC_Discharge_SPLT) <- c("Year", "Mean_SACR_Discharge", "Peak_Flow", "Mean_SACR_Discharge_BS", "Mean_SACR_Discharge_AS")
SAC_Discharge_SPLT

## Delta
DELTA_Mean_Discharge_SPLT <- 
  Discharge %>% 
  filter(Mo %in% 3:4 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_DELTA_Discharge = mean(OUT))
DELTA_Mean_Discharge_SPLT

DELTA_Mean_Discharge_SPLT_BS <- 
  Discharge %>% 
  filter(Mo %in% 1:2 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_DELTA_Discharge = mean(OUT))

DELTA_Mean_Discharge_SPLT_AS <- 
  Discharge %>% 
  filter(Mo %in% 5:6 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_DELTA_Discharge = mean(OUT))


# Combining PF and mean discharge for DELTA
DELTA_Discharge_SPLT <- 
  cbind(DELTA_Mean_Discharge_SPLT, Peak_Flow_DELTA$PF, DELTA_Mean_Discharge_SPLT_BS$Mean_DELTA_Discharge, DELTA_Mean_Discharge_SPLT_AS$Mean_DELTA_Discharge)
head(DELTA_Discharge_SPLT)

colnames(DELTA_Discharge_SPLT) <- c("Year", "Mean_DELTA_Discharge", "Peak_Flow", "Mean_DELTA_Discharge_BS", "Mean_DELTA_Discharge_AS")
head(DELTA_Discharge_SPLT)


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

SJ_Mean_Discharge_SASU_BS <- 
  Discharge %>% 
  filter(Mo %in% 1:2 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SJR_Discharge = mean(SJR))

SJ_Mean_Discharge_SASU_AS <- 
  Discharge %>% 
  filter(Mo %in% 5:6 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SJR_Discharge = mean(SJR))

# Combining PF and mean discharge for SJ
SJ_Discharge_SASU <- 
  cbind(SJ_Mean_Discharge_SASU, Peak_Flow_SJR$PF, SJ_Mean_Discharge_SASU_BS$Mean_SJR_Discharge, SJ_Mean_Discharge_SASU_AS$Mean_SJR_Discharge)
head(SJ_Discharge_SASU)

colnames(SJ_Discharge_SASU) <- c("Year", "Mean_SJR_Discharge", "Peak_Flow", "Mean_SJR_Discharge_BS", "Mean_SJR_Discharge_AS")
head(SJ_Discharge_SASU)

## Sacramento
SAC_Mean_Discharge_SASU <- 
  Discharge %>% 
  filter(Mo %in% 3:4 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SACR_Discharge = mean(SAC))
SAC_Mean_Discharge_SASU

SAC_Mean_Discharge_SASU_BS <- 
  Discharge %>% 
  filter(Mo %in% 1:2 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SACR_Discharge = mean(SAC))

SAC_Mean_Discharge_SASU_AS <- 
  Discharge %>% 
  filter(Mo %in% 5:6 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SACR_Discharge = mean(SAC))

# Combining PF and mean discharge for SAC
SAC_Discharge_SASU <- 
  cbind(SAC_Mean_Discharge_SASU, Peak_Flow_SAC$PF, SAC_Mean_Discharge_SASU_BS$Mean_SACR_Discharge, SAC_Mean_Discharge_SASU_AS$Mean_SACR_Discharge)
head(SAC_Discharge_SASU)

colnames(SAC_Discharge_SASU) <- c("Year", "Mean_SACR_Discharge", "Peak_Flow", "SAC_Mean_Discharge_SASU_BS", "SAC_Mean_Discharge_SASU_AS")
head(SAC_Discharge_SASU)

## Delta
DELTA_Mean_Discharge_SASU <- 
  Discharge %>% 
  filter(Mo %in% 3:4 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_DELTA_Discharge = mean(OUT))
DELTA_Mean_Discharge_SASU

DELTA_Mean_Discharge_SASU_BS <- 
  Discharge %>% 
  filter(Mo %in% 1:2 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_DELTA_Discharge = mean(OUT))

DELTA_Mean_Discharge_SASU_AS <- 
  Discharge %>% 
  filter(Mo %in% 5:6 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(Mean_DELTA_Discharge = mean(OUT))

# Combining PF and mean discharge for DELTA
DELTA_Discharge_SASU <- 
  cbind(DELTA_Mean_Discharge_SASU, Peak_Flow_DELTA$PF, DELTA_Mean_Discharge_SASU_BS$Mean_DELTA_Discharge, DELTA_Mean_Discharge_SASU_AS$Mean_DELTA_Discharge)
head(DELTA_Discharge_SASU)

colnames(DELTA_Discharge_SASU) <- c("Year", "Mean_DELTA_Discharge", "Peak_Flow", "DELTA_Mean_Discharge_SASU_BS", "DELTA_Mean_Discharge_SASU_AS")
head(DELTA_Discharge_SASU)

# Floodplain Inundation Duration ------------------------------------------

# Taken from Takata 2017
# Calculated as the number of days that mean Yolo Bypass flows met or exceeded 4000 cfs
# This variable was a good idea but is correalted with mean discharge

FP_Inudation <- 
  Discharge %>% 
  filter(Mo %in% 1:6 & Year %in% c(1995:2017)) %>% 
  group_by(Year) %>% 
  summarise(FP_Inudation = sum(YOLO > 3999))
head(FP_Inudation)



# Water Temperature -------------------------------------------------------

SJ_WT <-  readNWISdata(sites = "11303500",parameterCd = "00010", startDate = "1995-01-01", endDate = "2018-01-01", service = "dv")
str(SJ_WT)

# naming columns
colnames(SJ_WT)[colnames(SJ_WT)=="X_00010_00001"] <- "WT_Max"
colnames(SJ_WT)[colnames(SJ_WT)=="X_00010_00002"] <- "WT_Min"
colnames(SJ_WT)[colnames(SJ_WT)=="X_00010_00003"] <- "WT_Mean"
head(SJ_WT)

# Defining month and year variables
SJ_WT$Month <- as.factor(month(SJ_WT$dateTime))
SJ_WT$Year <- as.factor(year(SJ_WT$dateTime))
SJ_WT$Julian_Date <- as.factor(yday(as.POSIXlt(SJ_WT$dateTime, format = "%d-%b-%y")))

# There are a lot of mean water temp values that are missing
# Looking at correlation between max and mean WT to see if I can get away w/ max or min instead of mean
cor(SJ_WT$WT_Max, SJ_WT$WT_Mean, use = "complete.obs")
# 0.998
# Going to use max temp instead of mean

SJ_WT_Final <- 
  SJ_WT %>% 
  group_by(Year, Month) %>% 
  summarise(Mean_WT_Max = mean(WT_Max))
SJ_WT_Final

### Sac River
SAC_WT_Check <-  readNWISdata(sites = c("11390500", "11389500 "), parameterCd = "00010", startDate = "1995-01-01", endDate = "2018-01-01", service = "dv")
SAC_WT_Check$Month <- as.factor(month(SAC_WT_Check$dateTime))
SAC_WT_Check$Year <- as.factor(year(SAC_WT_Check$dateTime))

SAC_WT_Check_C <- 
  SAC_WT_Check %>% 
  filter(site_no == "11389500")

SAC_WT_Check_W <- 
  SAC_WT_Check %>% 
  filter(site_no == "11390500")

SAC_WT_Check_Final = left_join(SAC_WT_Check_C, SAC_WT_Check_W, by = "dateTime")
head(SAC_WT_Check_Final)
cor(SAC_WT_Check_Final$X_00010_00001.x, SAC_WT_Check_Final$X_00010_00001.y, use = "complete.obs")
# Corr between colusa and wilkins slough max values is 0.99 (and .97 between verona and wilkins)

SAC_WT <-  readNWISdata(sites = c("11390500"), parameterCd = "00010", startDate = "1995-01-01", endDate = "2018-01-01", service = "dv")
str(SAC_WT)
head(SAC_WT)

# naming columns
colnames(SAC_WT)[colnames(SAC_WT)=="X_00010_00001"] <- "WT_Max"
colnames(SAC_WT)[colnames(SAC_WT)=="X_00010_00002"] <- "WT_Min"
colnames(SAC_WT)[colnames(SAC_WT)=="X_00010_00003"] <- "WT_Mean"
head(SAC_WT)

# Defining month and year variables
SAC_WT$Month <- as.factor(month(SAC_WT$dateTime))
SAC_WT$Year <- as.factor(year(SAC_WT$dateTime))
head(SAC_WT)

# Examining the correlation between WT_Mean and WT_Max
cor(SAC_WT$WT_Max, SAC_WT$WT_Mean, use = "complete.obs")
# Correlation = .99

SAC_WT_Final <- 
  SAC_WT %>% 
  group_by(Year, Month) %>% 
  summarise(Mean_WT_Max = mean(WT_Max))
SAC_WT_Final

### Delta
# Currently can't find any stations w/ water temps in the Delta...

# SAPM --------------------------------------------------------------------

### From Felipe
# From a recruitment perspective, I think temperature is going to be the biggest player, but I haven’t had much time to consider the temperature window. Pikeminnow begin their spawning migration when water temperature hits 15-16°C, so the date at which mean water temp reaches this range should be a good staring point. You could do the same for suckers since their eggs are often found alongside those of Pikeminnow, indicating that they respond to similar cues. I’d also like to see how well your SASU and SAPM indices correlate. 
#### Brian: I didn't do anything with this...do you think I should? I could use julian date when first temp is 15. Can you think of another variable for WT?

# Using water temps after spawning (the months used to create the abundance indices). We know the fish were there then (because that's when we caught them) and temps during early life should control growth and ecosystem processes


### SJ
SAPM_SJ_WT_AS <- 
  SJ_WT %>%
  filter(Month %in% c(6:7))
# Very few NAs

SAPM_SJ_WT_AS_Final <- 
  SJ_WT %>%
  filter(Month %in% c(6:7)) %>% 
  group_by(Year, Month) %>% 
  summarise(Mean_WT_Max = mean(WT_Max, na.rm = TRUE)) %>% 
  group_by(Year) %>% 
  summarise(Mean_WT_Max = mean(Mean_WT_Max))
SAPM_SJ_WT_AS_Final

# Looking at temp overtime

SJ_WT %>%
filter(Year == 2014) %>% 
ggplot(aes(x = Julian_Date, y = WT_Max)) +
geom_point() +
facet_wrap(~Year)


### Sacramento 
SAPM_SAC_WT <- 
  SAC_WT %>%
  filter(Month %in% c(1:3))
# Very few NAs

SAPM_SAC_WT_BS_Final <- 
  SAC_WT %>%
  filter(Month %in% c(2:3)) %>% 
  group_by(Year, Month) %>% 
  summarise(Mean_WT_Max = mean(WT_Max, na.rm = TRUE)) %>% 
  group_by(Year) %>% 
  summarise(Mean_WT_Max = mean(Mean_WT_Max))
SAPM_SAC_WT_BS_Final

SAPM_SAC_WT_AS_Final <- 
  SAC_WT %>%
  filter(Month %in% c(6:7)) %>% 
  group_by(Year, Month) %>% 
  summarise(Mean_WT_Max = mean(WT_Max, na.rm = TRUE)) %>% 
  group_by(Year) %>% 
  summarise(Mean_WT_Max = mean(Mean_WT_Max))
SAPM_SAC_WT_AS_Final



# SPLT --------------------------------------------------------------------

# SPLT spawn in April and May
# Going to look at after spawning when temps could control growth and food availability 

### San Joaquin
SPLT_SJ_WT <- 
  SJ_WT %>%
  filter(Month %in% c(2:3))

SPLT_SJ_WT_AS <- 
  SJ_WT %>%
  filter(Month %in% c(5:6))
# Very few missing values and no large chuncks gone

# Going to see what months/years have missing values
SPLT_SJ_WT_NAs <- 
  SJ_WT %>%
  filter(Month %in% c(4:5)) %>% 
  group_by(Year, Month) %>% 
  summarise(Mean_WT_Max = mean(WT_Max))
SPLT_SJ_WT_NAs
# April 2004 and May 2009 are only NAs

SPLT_SJ_WT_Ap_04 <- 
  SJ_WT %>%
  filter(Month == 4, Year == 2004)
# Only 1 day w/ missing value

SPLT_SJ_WT_Ma_09 <- 
  SJ_WT %>%
  filter(Month == 5, Year == 2009)
# Only 1 day w/ missing value

SPLT_SJ_WT_Final <- 
  SJ_WT %>%
  filter(Month %in% c(4:5)) %>% 
  group_by(Year, Month) %>% 
  summarise(Mean_WT_Max = mean(WT_Max, na.rm = TRUE)) %>% 
  group_by(Year) %>% 
  summarise(Mean_WT_Max = mean(Mean_WT_Max))

SPLT_SJ_WT_BS_Final <- 
  SJ_WT %>%
  filter(Month %in% c(2:3)) %>% 
  group_by(Year, Month) %>% 
  summarise(Mean_WT_Max = mean(WT_Max, na.rm = TRUE)) %>% 
  group_by(Year) %>% 
  summarise(Mean_WT_Max = mean(Mean_WT_Max))

SPLT_SJ_WT_AS_Final <- 
  SJ_WT %>%
  filter(Month %in% c(5:6)) %>% 
  group_by(Year, Month) %>% 
  summarise(Mean_WT_Max = mean(WT_Max, na.rm = TRUE)) %>% 
  group_by(Year) %>% 
  summarise(Mean_WT_Max = mean(Mean_WT_Max))

### Sacramento 
SPLT_SAC_WT <- 
  SAC_WT %>%
  filter(Month %in% c(2:3))
# There are 4ish missing values in April and May in 2017...not too worried about it

SPLT_SAC_WT_BS <- 
  SAC_WT %>%
  filter(Month %in% c(2:3))

SPLT_SAC_WT_AS <- 
  SAC_WT %>%
  filter(Month %in% c(5:6))
# Very few missing values

SPLT_SAC_WT_Final_BS <- 
  SAC_WT %>%
  filter(Month %in% c(2:3)) %>% 
  group_by(Year, Month) %>% 
  summarise(Mean_WT_Max = mean(WT_Max, na.rm = TRUE)) %>% 
  group_by(Year) %>% 
  summarise(Mean_WT_Max = mean(Mean_WT_Max))
SPLT_SAC_WT_Final_BS

SPLT_SAC_WT_Final_DS <- 
  SAC_WT %>%
  filter(Month %in% c(4:5)) %>% 
  group_by(Year, Month) %>% 
  summarise(Mean_WT_Max = mean(WT_Max, na.rm = TRUE)) %>% 
  group_by(Year) %>% 
  summarise(Mean_WT_Max = mean(Mean_WT_Max))
SPLT_SAC_WT_Final_DS

SPLT_SAC_WT_Final_AS <- 
  SAC_WT %>%
  filter(Month %in% c(5:6)) %>% 
  group_by(Year, Month) %>% 
  summarise(Mean_WT_Max = mean(WT_Max, na.rm = TRUE)) %>% 
  group_by(Year) %>% 
  summarise(Mean_WT_Max = mean(Mean_WT_Max))
SPLT_SAC_WT_Final_AS

# SASU --------------------------------------------------------------------

# Going to use same WT as SPLT because they spawn at the same time
SASU_SJ_WT_AS_Final <- SPLT_SJ_WT_AS_Final
SASU_SAC_WT_Final_AS <- SPLT_SAC_WT_Final_AS
SASU_SJ_WT_Final <- SPLT_SJ_WT_Final
SASU_SAC_WT_Final <- SPLT_SAC_WT_Final

SASU_SAC_WT_Final_BS <- SPLT_SAC_WT_Final_BS
SASU_SAC_WT_Final_DS <- SPLT_SAC_WT_Final_DS
SASU_SAC_WT_Final_AS <- SPLT_SAC_WT_Final_AS

SASU_SJ_WT_BS_Final <- SPLT_SJ_WT_BS_Final
SASU_SJ_WT_DS_Final <- SPLT_SJ_WT_Final
SASU_SJ_WT_AS_Final <- SPLT_SJ_WT_AS_Final


# Zooplankton -------------------------------------------------------------

### Not using this due to poor spatial overlap with sampling sites can skip this section
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


### The stations have terrible overlap for the Sac and SJ regions...I am going to combine a couple of them and use for the Delta region

SAPM_Diet_Combined_Raw = read.csv("Data/SAPM_Diet_Combined.csv")
head(SAPM_Diet_Combined_Raw)

SAPM_Diet_Combined_Raw$Month = as.factor(numMonth(SAPM_Diet_Combined_Raw$Month))
SAPM_Diet_Combined_Raw$Year = as.factor(SAPM_Diet_Combined_Raw$Year)
str(SAPM_Diet_Combined_Raw)
head(SAPM_Diet_Combined_Raw)


SAPM_Diet_Combined_Raw <- 
  SAPM_Diet_Combined_Raw %>% 
  filter(Month %in% c(6:7) & Year %in% c(1995:2017))
head(SAPM_Diet_Combined_Raw)
tail(SAPM_Diet_Combined_Raw)


# Averaging zooplankton CPUE by year
SAPM_Diet_Combined_Year_Agg <- 
  SAPM_Diet_Combined_Raw %>% 
  group_by(StationCode, Year) %>%
  summarise_all(funs(mean)) %>%
  group_by(Year) %>% 
  summarise_all(funs(mean)) 
head(SAPM_Diet_Combined_Year_Agg)
tail(SAPM_Diet_Combined_Year_Agg) # 1995:2017

SAPM_Diet_Combined_Year_Agg = SAPM_Diet_Combined_Year_Agg[, 5:7]

# Going to see if PC will reduce the number of variables
SAPM_Diet_Combined_Year_Matrix = as.matrix(SAPM_Diet_Combined_Year_Agg) # Matrix for principal function
str(SAPM_Diet_Combined_Year_Matrix)


SAPM_Diet_Combined_Year_PC = principal(SAPM_Diet_Combined_Year_Matrix, nfactors = 2, rotate = "varimax") 
SAPM_Diet_Combined_Year_PC
# PC1 = Annelida and Gammaridae
# PC2 = Corophiidae and negative Annelida


SAPM_Diet_Combined_PC_Scores = as.data.frame(SAPM_Diet_Combined_Year_PC$scores)
colnames(SAPM_Diet_Combined_PC_Scores) <- c("DELTA_PC1", "DELTA_PC2")
head(SAPM_Diet_Combined_PC_Scores)


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

# The stations have terrible overlap for the Sac and SJ regions...I am going to combine a couple of them and use for the Delta region
Zoop_Combined_Raw = Zoop_Raw[Zoop_Raw$Station %in% c("NZ092", "NZ064", "NZ086") & Zoop_Raw$Month %in% c(5:6) & Zoop_Raw$Year %in% c(1995:2017),]

# Averaging zooplankton CPUE by year
Zoop_SPLT_Combined_Year_Agg <- 
  Zoop_Combined_Raw %>% 
  group_by(Station, Year) %>%
  summarise_all(funs(mean)) %>%
  group_by(Year) %>% 
  summarise_all(funs(mean)) 
tail(Zoop_SPLT_Combined_Year_Agg) # 1995:2017

# Going to see if PC will reduce the number of variables
Zoop_SPLT_Combined_Year_Matrix = as.matrix(Zoop_SPLT_Combined_Year_Agg) # Matrix for principal function

# Going to use Calanoids, Harpactacoids, and Cladocera because literature says they can be important diet items for larval SPLT
Zoop_SPLT_Combined_Year_Matrix = Zoop_SPLT_Combined_Year_Matrix[, -c(1:16,18,21:22,24,25)]
head(Zoop_SPLT_Combined_Year_Matrix)

Zoop_SPLT_Combined_Year_PC = principal(Zoop_SPLT_Combined_Year_Matrix, nfactors = 2, rotate = "varimax") 
Zoop_SPLT_Combined_Year_PC
# PC1 = Calanoid adults and juveniles, and cladocera
# PC2 = Harpact


Zoop_SPLT_Combined_PC_Scores = as.data.frame(Zoop_SPLT_Combined_Year_PC$scores)
colnames(Zoop_SPLT_Combined_PC_Scores) <- c("DELTA_PC1", "DELTA_PC2")

# Combining Data -------------------------------------------------------------

# SAPM --------------------------------------------------------------------

# SJ data
SAPM_Index_Final
SJ_Discharge_SAPM
SAPM_SJ_WT_AS_Final
Diet_SAPM_SJ_PC_Scores

# Sac data
SAPM_Index_Final
SAPM_Index_8_Final # Subarea 8 is no longer beind sampled...removing subarea 8 from index
SAC_Discharge_SAPM
SAPM_SAC_WT_BS_Final
SAPM_SAC_WT_AS_Final
FP_Inudation
SAPM_CDist_SAC
Diet_SAPM_SAC_PC_Scores

# Delta
SAPM_Index_Final
DELTA_Discharge_SAPM
FP_Inudation
Diet_SAPM_DELTA_PC_Scores
SAPM_Diet_Combined_PC_Scores # These are the data that combined all three stations and will be used for the Delta

## SJ
SAPM_SJ_Final <- 
  SAPM_Index_Final %>% 
  filter(Year %in% c(1995:2017))

SAPM_Index_Final
SJ_Discharge_SAPM
SJ_Mean_Discharge_SAPM_BS
SJ_Mean_Discharge_SAPM_AS
SAPM_SJ_WT_AS_Final
Diet_SAPM_SJ_PC_Scores

# Did not include dist index because there were multiple years when they were not caught in the SJ
SAPM_SJ_Final <- cbind(SAPM_SJ_Final$Year, SAPM_SJ_Final$San_Joaquin_River_Index, SJ_Discharge_SAPM[,2:5], SAPM_SJ_WT_AS_Final$Mean_WT_Max)
colnames(SAPM_SJ_Final)  <- c("Year", "San_Joaquin_River_Index", "Mean_SJR_Discharge", "Peak_Flow", "Mean_SJR_Discharge_BS", "Mean_SJR_Discharge_AS", "Mean_WT_Max")
head(SAPM_SJ_Final)

## SAC
SAPM_SAC_Final <- 
  SAPM_Index_Final %>% 
  filter(Year %in% c(1995:2017))
SAPM_SAC_Final

# Selected 1:23 for SAPM_Index_8_Final because had estimates for 2018
SAPM_SAC_Final <- cbind(SAPM_Index_8_Final$Year[1:23], SAPM_Index_8_Final$Sac_River_Index[1:23], SAC_Discharge_SAPM[,2:5], SAPM_SAC_WT_BS_Final$Mean_WT_Max, SAPM_SAC_WT_AS_Final$Mean_WT_Max, FP_Inudation$FP_Inudation, SAPM_CDist_SAC$CDist)
colnames(SAPM_SAC_Final)  <- c("Year", "Sac_River_Index", "Mean_SAC_Discharge", "Peak_Flow", "Mean_SAC_Discharge_BS", "Mean_SAC_Discharge_AS",  "Mean_WT_Max_BS", "Mean_WT_Max_AS", "FP_Inudation", "CDist_Sac")
head(SAPM_SAC_Final)
pairs(SAPM_SAC_Final, pch = 16)

SAPM_Index_Final
DELTA_Discharge_SAPM
FP_Inudation

## Delta
SAPM_DELTA_Final <- cbind(SAPM_Index_Final$Year, SAPM_Index_Final$Delta_Index, DELTA_Discharge_SAPM[,2:3], FP_Inudation$FP_Inudation)
colnames(SAPM_DELTA_Final) <- c("Year", "DELTA_Index", "Mean_DELTA_Discharge", "Peak_Flow", "FP_Inudation")
head(SAPM_DELTA_Final)


# Age-1 -------------------------------------------------------------------


##### Age-1 SAPM
# Not using for talk but may use these indicies in the future
SAPM_Index_Final_Age_1
# Going to match this data with the explanatory variables from the previous year (1996-2017)

## SJ
# Prey data only is available for 1996-2017
SAPM_SJ_Final_Age_1 <- 
  SAPM_Index_Final_Age_1 
head(SAPM_SJ_Final_Age_1)
tail(SAPM_SJ_Final_Age_1)


SAPM_SJ_WT_AS_Final$Mean_WT_Max

# This first object contains age-1 fish in year t and abiotic variables in year t-1. Trying to see how variables in year 0 influence survival/recruitment success. 
SAPM_SJ_Final_Age_1_Lag <- cbind(SAPM_SJ_Final_Age_1$Year[2:23], SAPM_SJ_Final_Age_1$San_Joaquin_River_Index[2:23], SJ_Discharge_SAPM[1:22,2:5], SAPM_SJ_WT_AS_Final$Mean_WT_Max[1:22])
colnames(SAPM_SJ_Final_Age_1_Lag)  <- c("Year", "San_Joaquin_River_Index", "Mean_SJR_Discharge", "Peak_Flow", "Mean_SJR_Discharge_BS", "Mean_SJR_Discharge_AS", "Mean_WT_Max")
head(SAPM_SJ_Final_Age_1_Lag)

# This includes age-1 fish and abiotic variables for year t. This data will be used to determine if/how abiotic variables influence SAPM inhabiting the Delta. 
SAPM_SJ_Final_Age_1_No_Lag <- cbind(SAPM_SJ_Final_Age_1$Year[1:23], SAPM_SJ_Final_Age_1$San_Joaquin_River_Index[1:23], SJ_Discharge_SAPM[,2:5], SAPM_SJ_WT_AS_Final$Mean_WT_Max)
colnames(SAPM_SJ_Final_Age_1_No_Lag)  <- c("Year", "San_Joaquin_River_Index", "Mean_SJR_Discharge", "Peak_Flow", "Mean_SJR_Discharge_BS", "Mean_SJR_Discharge_AS", "Mean_WT_Max")
head(SAPM_SJ_Final_Age_1_No_Lag)


## SAC
SAPM_SAC_Final_Age_1 <- 
  SAPM_Index_Final_Age_1 
head(SAPM_SAC_Final_Age_1)
tail(SAPM_SAC_Final_Age_1)

# Lag
SAPM_SAC_Final_Age_1_Lag <- cbind(SAPM_SAC_Final_Age_1$Year[2:23], SAPM_SAC_Final_Age_1$Sac_River_Index[2:23], SAC_Discharge_SAPM[1:22,2:5], SAPM_SAC_WT_BS_Final$Mean_WT_Max[1:22], SAPM_SAC_WT_AS_Final$Mean_WT_Max[1:22], FP_Inudation$FP_Inudation[1:22])
colnames(SAPM_SAC_Final_Age_1_Lag)  <- c("Year", "Sac_River_Index", "Mean_SAC_Discharge", "Peak_Flow", "Mean_SAC_Discharge_BS", "Mean_SAC_Discharge_AS",  "Mean_WT_Max_BS", "Mean_WT_Max_AS", "FP_Inudation")
head(SAPM_SAC_Final_Age_1_Lag)

# No lag
SAPM_SAC_Final_Age_1_No_Lag <- cbind(SAPM_SAC_Final_Age_1$Year[1:23], SAPM_SAC_Final_Age_1$Sac_River_Index[1:23], SAC_Discharge_SAPM[,2:5], SAPM_SAC_WT_BS_Final$Mean_WT_Max, SAPM_SAC_WT_AS_Final$Mean_WT_Max, FP_Inudation$FP_Inudation)
colnames(SAPM_SAC_Final_Age_1_No_Lag)  <- c("Year", "Sac_River_Index", "Mean_SAC_Discharge", "Peak_Flow", "Mean_SAC_Discharge_BS", "Mean_SAC_Discharge_AS",  "Mean_WT_Max_BS", "Mean_WT_Max_AS", "FP_Inudation")
head(SAPM_SAC_Final_Age_1_No_Lag)


## Delta
SAPM_DELTA_Final_Age_1 <- 
  SAPM_Index_Final_Age_1 
head(SAPM_DELTA_Final_Age_1)
tail(SAPM_DELTA_Final_Age_1)

SAPM_DELTA_Final_Age_1_Lag <- cbind(SAPM_DELTA_Final_Age_1$Year[2:23], SAPM_DELTA_Final_Age_1$Delta_Index[2:23], DELTA_Discharge_SAPM[1:22,2:4], FP_Inudation$FP_Inudation[1:22])
colnames(SAPM_DELTA_Final_Age_1_Lag) <- c("Year", "DELTA_Index", "Mean_DELTA_Discharge", "Peak_Flow", "Mean_DELTA_Discharge_BS", "FP_Inudation")
head(SAPM_DELTA_Final_Age_1_Lag)

SAPM_DELTA_Final_Age_1_No_Lag <- cbind(SAPM_DELTA_Final_Age_1$Year[1:23], SAPM_DELTA_Final_Age_1$Delta_Index[1:23], DELTA_Discharge_SAPM[,2:4], FP_Inudation$FP_Inudation)
colnames(SAPM_DELTA_Final_Age_1_No_Lag) <- c("Year", "DELTA_Index", "Mean_DELTA_Discharge", "Peak_Flow", "Mean_DELTA_Discharge_BS", "FP_Inudation")
head(SAPM_DELTA_Final_Age_1_No_Lag)

# SPLT --------------------------------------------------------------------

# SJ
SPLT_Index_Final
SJ_Discharge_SPLT
SPLT_SJ_WT_BS_Final
SPLT_SJ_WT_AS_Final
SPLT_SJ_WT_Final
SPLT_CDist_SJ

# Sac
SPLT_Index_Final
SPLT_Index_8_Final
SAC_Discharge_SPLT
SPLT_SAC_WT_Final_BS
SPLT_SAC_WT_Final_DS
SPLT_SAC_WT_Final_AS
FP_Inudation
SPLT_CDist_SAC

# Delta
SPLT_Index_Final
DELTA_Discharge_SPLT
FP_Inudation

# Not doing prey availability 
Zoop_SPLT_SJ_PC_Scores
Zoop_SPLT_SAC_PC_Scores
Zoop_SPLT_DELTA_PC_Scores
Zoop_SPLT_Combined_PC_Scores

SPLT_SJ_Final <- cbind(SPLT_Index_Final$Year, SPLT_Index_Final$San_Joaquin_River_Index, SJ_Discharge_SPLT[,2:5], SPLT_SJ_WT_BS_Final$Mean_WT_Max, SPLT_SJ_WT_Final$Mean_WT_Max, SPLT_SJ_WT_AS_Final$Mean_WT_Max, SPLT_CDist_SJ$CDist)
head(SPLT_SJ_Final)
colnames(SPLT_SJ_Final)  <- c("Year", "San_Joaquin_River_Index", "Mean_SJR_Discharge", "Peak_Flow", "Mean_SJR_Discharge_BS", "Mean_SJR_Discharge_AS", "Mean_WT_Max_BS", "Mean_WT_Max_DS", "Mean_WT_Max_AS", "CDist_SJ")
head(SPLT_SJ_Final)
pairs(SPLT_SJ_Final, pch = 16)


SPLT_SAC_Final <- cbind(SPLT_Index_8_Final$Year[1:23], SPLT_Index_8_Final$Sac_River_Index[1:23], SAC_Discharge_SPLT[,2:5], SPLT_SAC_WT_Final_BS$Mean_WT_Max, SPLT_SAC_WT_Final_DS$Mean_WT_Max, SPLT_SAC_WT_Final_AS$Mean_WT_Max, FP_Inudation$FP_Inudation, SPLT_CDist_SAC$CDist)
head(SPLT_SAC_Final)
colnames(SPLT_SAC_Final) <- c("Year", "Sac_River_Index", "Mean_SAC_Discharge", "Peak_Flow", "Mean_SAC_Discharge_BS", "Mean_SAC_Discharge_AS", "Mean_WT_Max_BS", "Mean_WT_Max_DS", "Mean_WT_Max_AS", "FP_Inudation", "CDist_SAC")
head(SPLT_SAC_Final)
pairs(SPLT_SAC_Final, pch = 16)


SPLT_DELTA_Final <- cbind(SPLT_Index_Final$Year, SPLT_Index_Final$Delta_Index, DELTA_Discharge_SPLT[,2:5], FP_Inudation$FP_Inudation)
head(SPLT_DELTA_Final)
colnames(SPLT_DELTA_Final) <- c("Year", "DELTA_Index", "Mean_DELTA_Discharge", "Peak_Flow", "Mean_DELTA_Discharge_BS", "Mean_DELTA_Discharge_AS", "FP_Inudation")
pairs(SPLT_DELTA_Final, pch = 16)

# SASU --------------------------------------------------------------------

# SJ
SASU_Index_Final
SJ_Discharge_SASU
SASU_SJ_WT_BS_Final
SASU_SJ_WT_DS_Final
SASU_SJ_WT_AS_Final
SASU_CDist_SJ

# Sac
SASU_Index_8_Final
SASU_Index_8_Dec_Final
SAC_Discharge_SASU
SASU_SAC_WT_Final_BS
SASU_SAC_WT_Final_DS
SASU_SAC_WT_Final_AS
FP_Inudation
SASU_CDist_SAC
SASU_CDist_SAC_Dec

# Delta
SASU_Index_Final
DELTA_Discharge_SASU
FP_Inudation

SASU_SJ_Final <- cbind(SASU_Index_Final$Year[1:23], SASU_Index_Final$San_Joaquin_River_Index[1:23], SJ_Discharge_SASU[,2:5], SASU_SJ_WT_BS_Final$Mean_WT_Max, SASU_SJ_WT_DS_Final$Mean_WT_Max, SASU_SJ_WT_AS_Final$Mean_WT_Max, SASU_CDist_SJ$CDist)
colnames(SASU_SJ_Final)  <- c("Year", "San_Joaquin_River_Index", "Mean_SJR_Discharge", "Peak_Flow", "Mean_SJR_Discharge_BS", "Mean_SJR_Discharge_AS", "Mean_WT_Max_BS", "Mean_WT_Max_DS", "Mean_WT_Max_AS", "CDist_SJ")
head(SASU_SJ_Final)
pairs(SASU_SJ_Final, pch = 16)

SASU_SAC_Final <- cbind(SASU_Index_8_Final$Year[1:23], SASU_Index_8_Final$Sac_River_Index[1:23], SASU_Index_8_Dec_Final$Sac_River_Index_December[1:23], SAC_Discharge_SASU[,2:5], SASU_SAC_WT_Final_BS$Mean_WT_Max, SASU_SAC_WT_Final_DS$Mean_WT_Max, SASU_SAC_WT_Final_AS$Mean_WT_Max, FP_Inudation$FP_Inudation, SASU_CDist_SAC$CDist, SASU_CDist_SAC_Dec$CDist)
head(SASU_SAC_Final)
colnames(SASU_SAC_Final) <- c("Year", "Sac_River_Index", "Sac_River_Index_Dec", "Mean_SAC_Discharge", "Peak_Flow", "Mean_SAC_Discharge_BS", "Mean_SAC_Discharge_AS", "Mean_WT_Max_BS", "Mean_WT_Max_DS", "Mean_WT_Max_AS", "FP_Inudation", "CDist_SAC", "CDist_SAC_Dec")
str(SASU_SAC_Final)
pairs(SASU_SAC_Final[, c(1:2)], pch = 16)
pairs(SASU_SAC_Final[, c(2:11)], pch = 16)
# Mean_SAC_Discharge, WT and FP_Inudation

plot(log(SASU_SAC_Final$Sac_River_Index) ~ log(SASU_SAC_Final$Mean_SAC_Discharge), pch = 16)

pairs(SASU_SAC_Final[, c(4:12)], pch = 16)
# Looks like dist is influenced most by Peak_Flow

SASU_DELTA_Final <- cbind(SASU_Index_Final$Year[1:23], SASU_Index_Final$Delta_Index[1:23], DELTA_Discharge_SASU[,2:5], FP_Inudation$FP_Inudation)
head(SASU_DELTA_Final)

colnames(SASU_DELTA_Final) <- c("Year", "DELTA_Index", "Mean_DELTA_Discharge", "Peak_Flow", "Mean_DELTA_Discharge_BS", "Mean_DELTA_Discharge_AS", "FP_Inudation")
head(SASU_DELTA_Final)
pairs(SASU_DELTA_Final, pch = 16)

# Final Analyses ----------------------------------------------------------


# SAPM --------------------------------------------------------------------
SAPM_SJ_Final
SAPM_SAC_Final
SAPM_DELTA_Final


# Trends ------------------------------------------------------------------


### Trends

## SJ
head(SAPM_SJ_Final)
acf(SAPM_SJ_Final$San_Joaquin_River_Index) # no autocorrelation

MannKendall(SAPM_SJ_Final$San_Joaquin_River_Index)
# tau = 0.103, 2-sided pvalue =0.55294

## Sac
# Index calculated w/o using region 8
head(SAPM_SAC_Final)
acf(SAPM_SAC_Final$Sac_River_Index) # no autocorrelation

MannKendall(SAPM_SAC_Final$Sac_River_Index)
# tau = 0.138, 2-sided pvalue =0.36921

## Delta
head(SAPM_DELTA_Final)
acf(SAPM_DELTA_Final$DELTA_Index)

MannKendall(SAPM_DELTA_Final$DELTA_Index)
# tau = -0.115, 2-sided pvalue =0.45961

##### Figures

SAPM_SJ_GGPLOT = ggplot(data= SAPM_Index_Final, aes(x=Year, y=San_Joaquin_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,.025), expand = c(0, 0)) +
  ylab("San Joaquin Index") +
  theme_classic(base_size = 18)
SAPM_SJ_GGPLOT


SAPM_Sac_GGPLOT = ggplot(data= SAPM_Index_Final, aes(x=Year, y=Sac_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,3), expand = c(0, 0)) +
  xlab("") +
  ylab("Sacramento Index") + 
  theme_classic(base_size = 18)
SAPM_Sac_GGPLOT

SAPM_Delta_GGPLOT = ggplot(data= SAPM_Index_Final, aes(x=Year, y=Delta_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  ylab("Delta Index") + 
  xlab("") + 
  theme_classic(base_size = 18)
SAPM_Delta_GGPLOT


# Output for SAPM plots
png(file = "Figures/SAPM.Indicies.png", width = 2000, height = 1500, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SAPM_Sac_GGPLOT, SAPM_Delta_GGPLOT, SAPM_SJ_GGPLOT, ncol = 1,
                                   top =textGrob("SAPM", vjust=2,gp = gpar(fontsize=25,   fontfamily="Times")))))         

dev.off()


SAPM_SJ_Final  

SAPM_SJ_GGPLOT = ggplot(data= SAPM_SJ_Final, aes(x=Year, y=San_Joaquin_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0), drop = FALSE, breaks=seq(1995, 2017, 2)) +
  scale_y_continuous(limits = c(0,.025), expand = c(0, 0)) +
  ylab("San Joaquin Index") +
  xlab("") +
  theme_classic(base_size = 18)
SAPM_SJ_GGPLOT

SAPM_Sac_GGPLOT = ggplot(data= SAPM_SAC_Final, aes(x=Year, y=Sac_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0), drop = FALSE, breaks=seq(1995, 2017, 2)) +
  scale_y_continuous(limits = c(0,3), expand = c(0, 0)) +
  ylab("Sacramento Index") +
  xlab("") +
  theme_classic(base_size = 18)
SAPM_Sac_GGPLOT

SAPM_Delta_GGPLOT = ggplot(data= SAPM_DELTA_Final, aes(x=Year, y=DELTA_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_discrete(expand = c(0, 0), drop = FALSE, breaks=seq(1995, 2017, 2)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  ylab("Delta Index") + 
  xlab("") +
  theme_classic(base_size = 18)
SAPM_Delta_GGPLOT

# Output for SAPM plots
png(file = "Figures/SAPM.Indicies.png", width = 1250, height = 1250, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SAPM_Sac_GGPLOT, SAPM_Delta_GGPLOT, SAPM_SJ_GGPLOT,  ncol = 1,
                                   top =textGrob("", vjust=2,gp = gpar(fontsize=25,   fontfamily="Times")))))         

dev.off()


# Models -----------------------------------------------------------------


## SJ
str(SAPM_SJ_Final)

### Will have to think about this. Very low catches and multiple years where index was 0. Need to find a more appropriate way to analyze the data

# Global Model
SAPM_SJ_Global = lm((San_Joaquin_River_Index) ~ Mean_SJR_Discharge + Peak_Flow, data = SAPM_SJ_Final, na.action = "na.fail")
summary(SAPM_SJ_Global)
cor(SAPM_SJ_Final[, 2:6])
acf(SAPM_SJ_Global$residuals) # Autocorreltion seems fine

# All subset selection
SAPM_SJ_Global_Dredge = dredge(SAPM_SJ_Global)
importance(SAPM_SJ_Global_Dredge)
# Null model is top model and has highest weights

# Same analyses using standardized variables
SAPM_SJ_Final.Scale = as.data.frame(scale(SAPM_SJ_Final[,3:4]))

SAPM_SJ_Global.Scale = lm(log(San_Joaquin_River_Index + 1) ~ SAPM_SJ_Final.Scale$Mean_SJR_Discharge + SAPM_SJ_Final.Scale$Peak_Flow, data = SAPM_SJ_Final, na.action = "na.fail")
summary(SAPM_SJ_Global.Scale)

plot(residuals(SAPM_SJ_Global.Scale) ~ SAPM_SJ_Final$Year, pch = 16)
abline(0,0, lwd = 2)

SAPM_SJ_Global_Scale_Dredge = dredge(SAPM_SJ_Global.Scale)
importance(SAPM_SJ_Global_Scale_Dredge)

SAPM_SJ_MA = model.avg(SAPM_SJ_Global_Scale_Dredge)
summary(SAPM_SJ_MA)



#### SAC

### Abundance index 
## Data used from index was estimated after removing region 8 becuase we are no longer sampling their
head(SAPM_SAC_Final)
str(SAPM_SAC_Final)
pairs(SAPM_SAC_Final[,2:9], pch = 16)

# Peak Flow
plot(log(Sac_River_Index) ~ Peak_Flow, pch = 16, data = SAPM_SAC_Final)
# Fairly strong positive relationship

# WT After Spawning
plot(log(Sac_River_Index) ~ Mean_WT_Max_AS, pch = 16, data = SAPM_SAC_Final)
# Meh...probably sig but not that strong/tight
# Global Model

cor(SAPM_SAC_Final[,2:9])
# Flow variables (and FP Inudation)

# Using standardized variables to be able to compare effect sizes across covariates 
SAPM_SAC_Final_Scale = as.data.frame(scale(SAPM_SAC_Final[,3:9]))
head(SAPM_SAC_Final_Scale)

# Global model for all subset selection
SAPM_SAC_Global_lm <- 
  lm(log(SAPM_SAC_Final$Sac_River_Index) ~ Mean_SAC_Discharge + Peak_Flow + Mean_WT_Max_AS, data = SAPM_SAC_Final_Scale, na.action = "na.fail")
summary(SAPM_SAC_Global_lm)

# Model assumptions
plot(SAPM_SAC_Global_lm) # Assumptions look good
acf(SAPM_SAC_Global_lm$residuals) # Autocorreltion is fine
vif(SAPM_SAC_Global_lm) # All < 2

## All subset selection
SAPM_SAC_Global_Dredge = dredge(SAPM_SAC_Global_lm)
SAPM_SAC_Global_Dredge
# Top model includes Peak_Flow
importance(SAPM_SAC_Global_Dredge)
# Peak_Flow is most important variable; weights = 0.83

# Getting model averaged coefficients for the multiple regression figures below
SAPM_SAC_MA = model.avg(SAPM_SAC_Global_Dredge)
summary(SAPM_SAC_MA)

#### DELTA
head(SAPM_DELTA_Final)
str(SAPM_DELTA_Final)
pairs(SAPM_DELTA_Final, pch = 16)
# Nothing looks great but PF might be OK in the models

SAPM_SAC_PF_Plot <- 
  SAPM_DELTA_Final %>% 
  ggplot(aes(y = log(DELTA_Index), x = (Peak_Flow))) +
  geom_point() +
  theme_classic()
SAPM_SAC_PF_Plot
# When the index is transformed PF looks good

## Scaling variables
SAPM_DELTA_Final_Scale = as.data.frame(scale(SAPM_DELTA_Final[,3:5]))
head(SAPM_DELTA_Final_Scale)
cor(SAPM_DELTA_Final_Scale)

# Global Model
SAPM_DELTA_Global = lm(log(SAPM_DELTA_Final$DELTA_Index) ~ Mean_DELTA_Discharge + Peak_Flow, data = SAPM_DELTA_Final_Scale, na.action = "na.fail")
summary(SAPM_DELTA_Global)

# Model assumptions
plot(SAPM_DELTA_Global) # Looks fine
acf(SAPM_DELTA_Global$residuals) # No autocorreltion
vif(SAPM_DELTA_Global) # Looks good (both < 1.10)

# All subset selection
SAPM_DELTA_Global_Dredge = dredge(SAPM_DELTA_Global)
SAPM_DELTA_Global_Dredge
# Peak flow in the top model

importance(SAPM_DELTA_Global_Dredge)
# PF weights = 0.81

# Model averaged coefs
SAPM_DELTA_MA = model.avg(SAPM_DELTA_Global_Dredge)
summary(SAPM_DELTA_MA)

# Not really sure what the figures below show. I assume that we are looking for trends in the residuals overtime.
# The figure below looks good and it looks like shot gun spread
plot(residuals(SAPM_DELTA_Global) ~ SAPM_DELTA_Final$Year, pch = 16)
abline(0,0, lwd = 2)


# Age-1 -------------------------------------------------------------------


######## Age-1 SAPM

# Too many years with 0 index est. for SJ to use w/ regression. Need more apprpriate technique to analyze
SAPM_SJ_Final_Age_1_Lag
SAPM_SJ_Final_Age_1_NoLag

SAPM_SAC_Final_Age_1_Lag
SAPM_SAC_Final_Age_1_No_Lag

SAPM_DELTA_Final_Age_1_Lag
SAPM_DELTA_Final_Age_1_No_Lag

### Sac

## Lagged data
SAPM_SAC_Final_Age_1_Lag
pairs(SAPM_SAC_Final_Age_1_Lag, pch = 16)
# Nothing great

## No lag
SAPM_SAC_Final_Age_1_No_Lag
pairs(SAPM_SAC_Final_Age_1_No_Lag, pch = 16)
# Looks like negative relationships with flow and positive relationships with temp
# Use BS (Before Spawning esitmates) because these variables were estimated using data collected in Feb and March while and the data that were used to create the index were collected in March and April
cor(SAPM_SAC_Final_Age_1_No_Lag[2:9])

# Sclaing covariates before analyzing the data
SAPM_SAC_Final_Age_1_No_Lag.Scale <- 
  as.data.frame(scale(SAPM_SAC_Final_Age_1_No_Lag[,c(4:5,7,9)]))
head(SAPM_SAC_Final_Age_1_No_Lag.Scale)
cor(SAPM_SAC_Final_Age_1_No_Lag.Scale)
# Not going to use FP Inudation because it's correlated with discharge

# Last estimate is 0...adding 0.01 because 1 is large relative to the index estimates
SAPM_SAC_Global_Age_1.No.Lag = lm(log(SAPM_SAC_Final_Age_1_No_Lag$Sac_River_Index + .01) ~ Peak_Flow + Mean_SAC_Discharge_BS + Mean_WT_Max_BS, data = SAPM_SAC_Final_Age_1_No_Lag.Scale, na.action = "na.fail")
summary(SAPM_SAC_Global_Age_1.No.Lag)
# Negative relationship with discharge

SAPM_SAC_Global_Age_1_No_Lag_Dredge = dredge(SAPM_SAC_Global_Age_1.No.Lag)
SAPM_SAC_Global_Age_1_No_Lag_Dredge
# Discharge is in the top model
importance(SAPM_SAC_Global_Age_1_No_Lag_Dredge)
# Discharge weights = 0.84

SAPM_SAC_Age_1_No_Lag_MA = model.avg(SAPM_SAC_Global_Age_1_No_Lag_Dredge)
summary(SAPM_SAC_Age_1_No_Lag_MA)


### DELTA

## No lag
SAPM_DELTA_Final_Age_1_No_Lag
pairs(SAPM_DELTA_Final_Age_1_No_Lag, pch = 16)
# Nothing great

# Sclaing covariates before analyzing the data
SAPM_DELTA_Final_Age_1_No_Lag.Scale <- 
  as.data.frame(scale(SAPM_DELTA_Final_Age_1_No_Lag[,c(3:6)]))
head(SAPM_DELTA_Final_Age_1_No_Lag.Scale)
cor(SAPM_DELTA_Final_Age_1_No_Lag.Scale)
# Not going to use FP Inudation because it's correlated with discharge

# Last estimate is 0...adding 0.01 because 1 is large relative to the index estimates
SAPM_DELTA_Global_Age_1.No.Lag = lm(log(SAPM_DELTA_Final_Age_1_No_Lag$DELTA_Index) ~ Peak_Flow + Mean_DELTA_Discharge_BS, data = SAPM_DELTA_Final_Age_1_No_Lag.Scale, na.action = "na.fail")
summary(SAPM_DELTA_Global_Age_1.No.Lag)
# Nothing is significant...and low amount of varibility is explained by the covariates

SAPM_DELTA_Global_Age_1_No_Lag_Dredge = dredge(SAPM_DELTA_Global_Age_1.No.Lag)
SAPM_DELTA_Global_Age_1_No_Lag_Dredge
# Discharge is in the top model
importance(SAPM_DELTA_Global_Age_1_No_Lag_Dredge)
# Discharge weights = 0.58

SAPM_DELTA_Age_1_No_Lag_MA = model.avg(SAPM_DELTA_Global_Age_1_No_Lag_Dredge)
summary(SAPM_DELTA_Age_1_No_Lag_MA)
# Nearly positive for discharge

# Figures -----------------------------------------------------------------


### Age-0 Figures with SJ first

# Data
summary(SAPM_SAC_MA)
summary(SAPM_DELTA_MA)
summary(SAPM_SJ_MA) # Do not use. From old model that was estimated using 0 inflated estimates


SAPM_SAC_MA_coef = c(0.5569, 0.2162, 0.1558) # PF, WT, Discharge
SAPM_SAC_MA_stand_error = c(0.2226, 0.2346, 0.2953)

SAPM_DELTA_MA_coef = c(0.5661, 0.2985)
SAPM_DELTA_MA_stand_error = c(0.2339, 0.2465)

SAPM_SJ_MA_coef = c(0, 0) # Used 0s to get a clean plot w/ just the dashed line
SAPM_SJ_MA_stand_error = c(0, 0) # Used 0s to get a clean plot w/ just the dashed line

par(mar=c(5,10,0,9))

tiff("Figures/SAPM.MA.2.tiff", width = 10, height = 10, units = 'in', res = 700)
par(family = "Arial")
pdf("Figures/SAPM.MA.1.pdf", width = 10, height = 10)


# SJ
###
plotCI(x = SAPM_SJ_MA_coef, y = c(-10, -10), uiw = 1.96*SAPM_SJ_MA_stand_error, err = "x", pch = c(16, 17), lwd = 3, cex = 3.5, xlim = c(-2, 2), ylim = c( 1, 2), axes = F, xlab = "Regression Coefficients", ylab = "", cex.lab = 2.5, yaxt = "n", family = "Arial")
axis(1, lwd = 2.5, tck=-.001, mgp=c(1.80,1,0), cex.axis = 2.5, cex = 2)
text(x = 0, y = 2, " ", cex = 3, family = "Arial", font = 2)
mtext((""), side = 2, line = 4, at = c(1.8), las = 1, cex = 3, family = "Arial")
clip(-30, 30, 0, 1.89)
abline(v=0, lwd = 3, lty = 2)
# legend(1.85, 1.85, legend=c("TF", "Flow", "PC1", "PC2"),pch = c(16, 17, 15, 18), cex=3.5, bty = "n")

# DELTA
plotCI(x = SAPM_DELTA_MA_coef, y = c(1.57, 1.50), uiw = 1.96*SAPM_DELTA_MA_stand_error, err = "x", pch = c(16, 17, 15, 18), lwd = 3, cex = 3.5, xlim = c(-2, 2), ylim = c( 1, 2), axes = F, xlab = "Regression Coefficients", ylab = "", cex.lab = 2.5, yaxt = "n", family = "Arial", add = TRUE)
mtext((""), side = 2, line = 1.98, at = c(1.46), las = 1, cex = 3)


# SAC
plotCI(x = SAPM_SAC_MA_coef, y = c(1.28, 1.18, 1.08), uiw = 1.96*SAPM_SAC_MA_stand_error, err = "x", pch = c(16, 17, 15), lwd = 3, cex = 3.5, xlim = c(-2, 2), ylim = c( 1, 2), axes = F, xlab = "Regression Coefficients", ylab = "", cex.lab = 2.5, yaxt = "n", add = TRUE)
mtext((""), side = 2, line = 5, at = c(1.13), las = 1, cex = 3)

dev.off()

remove(SAPM_SJ_MA_coef) # Removing these because these objects were just created to make clean plots
remove(SAPM_SJ_MA_stand_error)


# SPLT --------------------------------------------------------------------
SPLT_SJ_Final
SPLT_SAC_Final
SPLT_DELTA_Final

# Trends ------------------------------------------------------------------


## SJ
head(SPLT_SJ_Final)
acf(SPLT_SJ_Final$San_Joaquin_River_Index) # no autocorrelation

MannKendall(SPLT_SJ_Final$San_Joaquin_River_Index)
# tau = -0.115, 2-sided pvalue = 0.45961

## Sac
head(SPLT_SAC_Final)
acf(SPLT_SAC_Final$Sac_River_Index) # Slightly over the line at time step 5...don't think it's a big deal

MannKendall(SPLT_SAC_Final$Sac_River_Index)
# tau = 0.281, 2-sided pvalue =0.064497

## Delta
head(SPLT_DELTA_Final)
acf(SPLT_DELTA_Final$DELTA_Index)

MannKendall(SPLT_DELTA_Final$DELTA_Index)
# tau = 0.0119, 2-sided pvalue =0.95787


# Models ------------------------------------------------------------------



## SJ
str(SPLT_SJ_Final)
pairs(SPLT_SJ_Final, pch = 16)

plot(log(San_Joaquin_River_Index) ~ Mean_SJR_Discharge, pch = 16, data = SPLT_SJ_Final)

# Global Model
SPLT_SJ_Final_Scale = as.data.frame(scale(SPLT_SJ_Final[,3:10])) # Standardizing variables
head(SPLT_SJ_Final_Scale)

# All subsets selection
SPLT_SJ_Global = lm(log(SPLT_SJ_Final$San_Joaquin_River_Index) ~ Mean_SJR_Discharge + Peak_Flow + Mean_WT_Max_AS, data = SPLT_SJ_Final_Scale, na.action = "na.fail")
summary(SPLT_SJ_Global)

# Model assumptions
acf(SPLT_SJ_Global$residuals) # No auto cor
vif(SPLT_SJ_Global) # all < 2.5
plot(SPLT_SJ_Global)
plot(residuals(SPLT_SJ_Global) ~ SPLT_SJ_Final$Year, pch = 16)
abline(0,0, lwd = 2)

# All subsets selection
SPLT_SJ_Global_Dredge = dredge(SPLT_SJ_Global)
importance(SPLT_SJ_Global_Dredge)

# Model averaged coefs
SPLT_SJ_MA = model.avg(SPLT_SJ_Global_Dredge)
summary(SPLT_SJ_MA)


### SAC
str(SPLT_SAC_Final)
pairs(SPLT_SAC_Final, pch = 16)

SPLT_SAC_Final_Scale = as.data.frame(scale(SPLT_SAC_Final[,3:11]))
head(SPLT_SAC_Final_Scale)

# Global Model
SPLT_SAC_Global = lm(log(SPLT_SAC_Final$Sac_River_Index) ~ Mean_SAC_Discharge + Peak_Flow + Mean_WT_Max_AS, data = SPLT_SAC_Final_Scale, na.action = "na.fail")
summary(SPLT_SAC_Global)

# Model assumptions
acf(SPLT_SAC_Global$residuals) # Good
vif(SPLT_SAC_Global) # All < than 2
plot(SPLT_SAC_Global)
plot(residuals(SPLT_SAC_Global) ~ SPLT_SJ_Final$Year, pch = 16)
abline(0,0, lwd = 2)

# All subsets selection
SPLT_SAC_Global_Dredge = dredge(SPLT_SAC_Global)
importance(SPLT_SAC_Global_Dredge)

# Model averaged coefs
SPLT_SAC_MA = model.avg(SPLT_SAC_Global_Dredge)
summary(SPLT_SAC_MA)

# The note below is no longer the case. After adding water temp the reg coef becomes positive...but still looking at how variable influence the dist of splt below
# There is a negative regression coef for SAC discharge. At first glance this does not make sense because they are flood plain spawners. But consideirng Fyrer's paper, in dry years fish have to move farther upstream to spawn and other papers mention that they may spawn on stream margins. Because these sites are above Yolo Bypass, in wet years fish probably don't migrate as far. 


## DELTA
str(SPLT_DELTA_Final)
pairs(SPLT_DELTA_Final, pch = 16)

# Same analyses using standardized variables
SPLT_DELTA_Final_Scale = as.data.frame(scale(SPLT_DELTA_Final[,3:7]))
head(SPLT_DELTA_Final_Scale)

SPLT_DELTA_Global = lm(log(SPLT_DELTA_Final$DELTA_Index) ~ Mean_DELTA_Discharge + Peak_Flow, data = SPLT_DELTA_Final_Scale, na.action = "na.fail")
summary(SPLT_DELTA_Global)

# Model assumptions
acf(SPLT_DELTA_Global$residuals) # Good
vif(SPLT_DELTA_Global) # Both ~1
plot(SPLT_DELTA_Global)
plot(residuals(SPLT_DELTA_Global) ~ SPLT_DELTA_Final$Year, pch = 16)
abline(0,0, lwd = 2)

# All subsets selection
SPLT_DELTA_Global_Dredge = dredge(SPLT_DELTA_Global)
importance(SPLT_DELTA_Global_Dredge)

# Model averaged coefs
SPLT_DELTA_MA = model.avg(SPLT_DELTA_Global_Dredge)
summary(SPLT_DELTA_MA)


# Figures -----------------------------------------------------------------

# Data
summary(SPLT_SAC_MA)
summary(SPLT_DELTA_MA)
summary(SPLT_SJ_MA)

SPLT_SAC_MA_coef = c(0.1755, 0.2705, 0.3615)
SPLT_SAC_MA_stand_error = c(0.2315, 0.2778, 0.2540)

SPLT_DELTA_MA_coef = c(0.3852, 0.7825)
SPLT_DELTA_MA_stand_error = c(0.2054, 0.2107)

SPLT_SJ_MA_coef = c(0.7018, 1.8424, -0.5404)
SPLT_SJ_MA_stand_error = c(0.3525, 0.4222, 0.5936)


par(mar=c(5,10,0,9))

tiff("Figures/SPLT.MA.3.tiff", width = 10, height = 10, units = 'in', res = 700)
par(family = "Arial")

# SJ
plotCI(x = SPLT_SJ_MA_coef, y = c(1.85, 1.75, 1.65), uiw = 1.96*SPLT_SJ_MA_stand_error, err = "x", pch = c(16, 17, 15), lwd = 3, cex = 3.5, xlim = c(-3, 3), ylim = c( 1, 2), axes = F, xlab = "Regression Coefficients", ylab = "", cex.lab = 2.5, yaxt = "n", family = "Arial")
axis(1, lwd = 2.5, tck=-.001, mgp=c(1.80, 1,0), cex.axis = 2.5, cex = 2)
text(x = 0, y = 2, " ", cex = 3, family = "Times", font = 2)
mtext((""), side = 2, line = 4, at = c(1.8), las = 1, cex = 3, family = "Arial")
clip(-30, 30, 0, 1.89)
abline(v=0, lwd = 3, lty = 2)
# legend(1.85, 1.85, legend=c("TF", "Flow", "PC1", "PC2"),pch = c(16, 17, 15, 18), cex=3.5, bty = "n")


# DELTA
plotCI(x = SPLT_DELTA_MA_coef, y = c(1.50, 1.40), uiw = 1.96*SPLT_DELTA_MA_stand_error, err = "x", pch = c(16, 17), lwd = 3, cex = 3.5, xlim = c(-1, 1), ylim = c( 1, 2), axes = F, xlab = "Regression Coefficients", ylab = "", cex.lab = 2.5, yaxt = "n", family = "Arial", add = TRUE)
mtext((""), side = 2, line = 1.98, at = c(1.46), las = 1, cex = 3, family = "Arial")

# SAC
plotCI(x = SPLT_SAC_MA_coef, y = c(1.25, 1.15, 1.05), uiw = 1.96*SPLT_SAC_MA_stand_error, err = "x", pch = c(16, 17, 15), lwd = 3, cex = 3.5, xlim = c(-1, 1), ylim = c( 1, 2), axes = F, xlab = "Regression Coefficients", ylab = "", cex.lab = 2.5, yaxt = "n", family = "Arial", add = TRUE)
mtext((""), side = 2, line = 5, at = c(1.13), las = 1, cex = 3, family = "Arial")

dev.off()


# SASU --------------------------------------------------------------------
SASU_SJ_Final
SASU_SAC_Final
SASU_DELTA_Final


# Trends ------------------------------------------------------------------



## SJ
head(SASU_SJ_Final)
acf(SASU_SJ_Final$San_Joaquin_River_Index) # no autocorrelation

MannKendall(SASU_SJ_Final$San_Joaquin_River_Index)
# tau = -0.00395, 2-sided pvalue =1

## Sac
head(SASU_SAC_Final)
acf(SASU_SAC_Final$Sac_River_Index) # no autocorrelation

MannKendall(SASU_SAC_Final$Sac_River_Index)
# tau = 0.439, 2-sided pvalue =0.0036708

## Delta
head(SASU_DELTA_Final)
acf(SASU_DELTA_Final$DELTA_Index)

MannKendall(SASU_DELTA_Final$DELTA_Index)
# tau = 0.462, 2-sided pvalue =0.0021868


# Models ------------------------------------------------------------------


## SJ

# Scaling predictor variables
SASU_SJ_Final_Scale = as.data.frame(scale(SASU_SJ_Final[,3:9]))
head(SASU_SJ_Final_Scale)

# Global Model
SASU_SJ_Global = lm(log(SASU_SJ_Final$San_Joaquin_River_Index) ~ Mean_SJR_Discharge + Peak_Flow + Mean_WT_Max_AS, data = SASU_SJ_Final_Scale, na.action = "na.fail")
summary(SASU_SJ_Global)

# Model assumptions
acf(SASU_SJ_Global$residuals) # Good
vif(SASU_SJ_Global) # all < 2.5
plot(SASU_SJ_Global)
plot(residuals(SASU_SJ_Global) ~ SASU_SJ_Final$Year, pch = 16)
abline(0,0, lwd = 2)

# All subset selection
SASU_SJ_Global_Dredge = dredge(SASU_SJ_Global)
importance(SASU_SJ_Global_Dredge)

# Getting model averaged coefs
SASU_SJ_MA = model.avg(SASU_SJ_Global_Dredge)
summary(SASU_SJ_MA)


## SAC

# Scaling predictor variables
SASU_SAC_Final_Scale = as.data.frame(scale(SASU_SAC_Final[,4:11]))
head(SASU_SAC_Final_Scale)

# Global Model
SASU_SAC_Global = lm(log(SASU_SAC_Final$Sac_River_Index) ~ Mean_SAC_Discharge + Peak_Flow + Mean_WT_Max_AS, data = SASU_SAC_Final_Scale, na.action = "na.fail")
summary(SASU_SAC_Global)

# Model assumptions
vif(SASU_SAC_Global) # All < than 2
acf(SASU_SAC_Global$residuals) # Good
plot(SASU_SAC_Global) # Normality is not great but constant variance is...reggression is robust to violations to normality
plot(residuals(SASU_SAC_Global) ~ SASU_SAC_Final$Year, pch = 16)
abline(0,0, lwd = 2)

# All subsets selection
SASU_SAC_Global_Dredge = dredge(SASU_SAC_Global)
importance(SASU_SAC_Global_Dredge)

# Model averaged components 
SASU_SAC_MA = model.avg(SASU_SAC_Global_Dredge)
summary(SASU_SAC_MA)

## DELTA
str(SASU_DELTA_Final)

# Same analyses using standardized variables
SASU_DELTA_Final_Scale = as.data.frame(scale(SASU_DELTA_Final[,3:7]))
head(SASU_DELTA_Final_Scale)

# Global Model
SASU_DELTA_Global = lm(log(SASU_DELTA_Final$DELTA_Index) ~ Mean_DELTA_Discharge + Peak_Flow, data = SASU_DELTA_Final_Scale, na.action = "na.fail")
summary(SASU_DELTA_Global)

# Model assumptions
vif(SASU_DELTA_Global) # Both ~1.04
acf(SASU_DELTA_Global$residuals) # Good
plot(SASU_DELTA_Global) # Meh...ok
plot(residuals(SASU_DELTA_Global) ~ SASU_DELTA_Final$Year, pch = 16)
abline(0,0, lwd = 2)

# All subsets selection
SASU_DELTA_Global_Dredge = dredge(SASU_DELTA_Global)
importance(SASU_DELTA_Global_Dredge)

# Model averaged coefs
SASU_DELTA_MA = model.avg(SASU_DELTA_Global_Dredge)
summary(SASU_DELTA_MA)

### Model Average Figures

# Figures -----------------------------------------------------------------


# Data
summary(SASU_SAC_MA)
summary(SASU_DELTA_MA)
summary(SASU_SJ_MA)

SASU_SAC_MA_coef = c(0.2918, -0.8615, 0.2808)
SASU_SAC_MA_stand_error = c(0.1905, 0.2166, 0.2146)

SASU_DELTA_MA_coef = c(0.3640, -0.1783)
SASU_DELTA_MA_stand_error = c(0.1735, 0.1805)

SASU_SJ_MA_coef = c(-0.05901, 0.14404, -0.47380)
SASU_SJ_MA_stand_error = c(0.20814, 0.28481, 0.19785)


par(mar=c(5,10,0,9))

tiff("Figures/SASU.MA.3.tiff", width = 10, height = 10, units = 'in', res = 700)
par(family = "Arial")

# SJ
plotCI(x = SASU_SJ_MA_coef, y = c(1.85, 1.75, 1.65), uiw = 1.96*SASU_SJ_MA_stand_error, err = "x", pch = c(16, 17, 15), 
       lwd = 3, cex = 3.5, xlim = c(-2, 2), ylim = c( 1, 2), axes = F, xlab = "Regression Coefficients", ylab = "", 
       cex.lab = 2.5, yaxt = "n", family = "Arial")
axis(1, lwd = 2.5, tck=-.001, mgp=c(1.80,1,0), cex.axis = 2.5, cex = 2)
text(x = 0, y = 2, " ", cex = 3, family = "Arial", font = 2)
mtext((""), side = 2, line = 4, at = c(1.8), las = 1, cex = 3, family = "Arial")
clip(-30, 30, 0, 1.89)
abline(v=0, lwd = 3, lty = 2)
# legend(1.85, 1.85, legend=c("TF", "Flow", "PC1", "PC2"),pch = c(16, 17, 15, 18), cex=3.5, bty = "n")


# DELTA
plotCI(x = SASU_DELTA_MA_coef, y = c(1.50, 1.40), uiw = 1.96*SASU_DELTA_MA_stand_error, err = "x", pch = c(16, 17), lwd = 3, cex = 3.5, xlim = c(-2, 2), ylim = c( 1, 2), axes = F, xlab = "Regression Coefficients", ylab = "", cex.lab = 2.5, yaxt = "n", family = "Arial", add = TRUE)
mtext((""), side = 2, line = 1.98, at = c(1.46), las = 1, cex = 3, family = "Arial")

# SAC
plotCI(x = SASU_SAC_MA_coef, y = c(1.25, 1.15, 1.10), uiw = 1.96*SASU_SAC_MA_stand_error, err = "x", pch = c(16, 17, 15), lwd = 3, cex = 3.5, xlim = c(-2, 2), ylim = c( 1, 2), axes = F, xlab = "Regression Coefficients", ylab = "", cex.lab = 2.5, yaxt = "n", family = "Arial", add = TRUE)
mtext((""), side = 2, line = 5, at = c(1.13), las = 1, cex = 3, family = "Arial")

dev.off()



# Center of Dist ----------------------------------------------------------------

# Look at center of distribution (what Ted did) based on river KM

### Vary months used for the distribution (later then what was used for the indices) and maybe look at different months of flow too

# When considering months of flow, it probably doesn't make sense to look at flows before spawning...we are using center of dist that was calculated after those flows. It probably doesn't make sense to look at flows during spawning either unless some of the earlier hatched fish are washed DS

# SAPM --------------------------------------------------------------------

### Center of distribution

SAPM_SAC_Final
SAPM_SAC_Final_Scale # I ended up scaling a new df that had log mean discharge below

## Sac
### Data taken from indices that were estimated without using region 8 because we no longer sample there

pairs(SAPM_SAC_Final[,3:10], pch = 16)
# Looks like best relationship with Discharge

SAPM_SAC_CDist_Dis_Plot <- 
  SAPM_SAC_Final %>% 
  ggplot(aes(y = log(SAPM_CDist_SAC$CDist), x = log(Mean_SAC_Discharge))) +
  geom_point() +
  theme_classic()
SAPM_SAC_CDist_Dis_Plot
# Most linear when both are log transformed


SAPM_SAC_CDist = lm(log(SAPM_CDist_SAC$CDist) ~ log(Mean_SAC_Discharge) + Peak_Flow + Mean_WT_Max_AS, data = SAPM_SAC_Final, na.action = "na.fail")
vif(SAPM_SAC_CDist_Global)
summary(SAPM_SAC_CDist)
# When mean discharge is log transformed then this variable and timing of flow explain 46% of the variability in the distribution of pikeminnow

### The best linear relationship between discharge and distribution results from log transforming both variables. However, I want to scale the covariates (which creates negative values of discharge) and it's not possible to take the log of scaled variables...maybe take log then scale??

SAPM_SAC_Final_CDist = SAPM_SAC_Final
SAPM_SAC_Final_CDist$Log_Mean_Dis <- log(SAPM_SAC_Final$Mean_SAC_Discharge)


SAPM_SAC_Final_CDist_Scale = as.data.frame(scale(SAPM_SAC_Final_CDist[,3:11]))
head(SAPM_SAC_Final_CDist_Scale)

# With scaled predictors
SAPM_SAC_Dist_Global_lm <- 
  lm(log(SAPM_SAC_Final$CDist_Sac) ~ Log_Mean_Dis + Peak_Flow + Mean_WT_Max_AS, data = SAPM_SAC_Final_CDist_Scale, na.action = "na.fail")
summary(SAPM_SAC_Dist_Global_lm)
# Taking log then scaling worked well...however it may be hard to interpret log then scaled discharge?

# Model assumptions
plot(SAPM_SAC_Dist_Global_lm) # Assumptions look good
acf(SAPM_SAC_Dist_Global_lm$residuals) # Autocorreltion is fine
vif(SAPM_SAC_Dist_Global_lm) # All < 2.1

## All subset selection
SAPM_SAC_Dist_Global_Dredge = dredge(SAPM_SAC_Dist_Global_lm)
SAPM_SAC_Dist_Global_Dredge
# Top model includes discharge
importance(SAPM_SAC_Dist_Global_Dredge)
# Discharge is most important variable; weights = 0.99

SAPM_SAC_CDist_MA = model.avg(SAPM_SAC_Dist_Global_Dredge)
summary(SAPM_SAC_CDist_MA)


## SJ
# Does not work because there are multiple years when we did not catch any age-0 SAPM in the SJ



# SPLT --------------------------------------------------------------------

###### Sac
### Data taken from indices that were estimated without using region 8 because we no longer sample there
SPLT_SAC_Final
SPLT_SAC_Final_Scale

pairs(SPLT_SAC_Final[,3:11], pch = 16)

# CDist vs mean discharge during spawning
SPLT_SAC_CDist_Dis_Plot <- 
  SPLT_SAC_Final_Scale %>% 
  ggplot(aes(x = Mean_SAC_Discharge, y = log(SPLT_SAC_Final$CDist))) +
  geom_point() +
  theme_classic()
SPLT_SAC_CDist_Dis_Plot  


SPLT_SAC_Dist_Global_lm <- 
  lm(log(SPLT_SAC_Final$CDist) ~ Mean_SAC_Discharge + Peak_Flow + Mean_WT_Max_AS, data = SPLT_SAC_Final_Scale, na.action = "na.fail")
summary(SPLT_SAC_Dist_Global_lm)
# Negative relationship with mean discharge during spawning (p = 0.03). The model only explains ~17% of the variability though

# Model assumptions
plot(SPLT_SAC_Dist_Global_lm) # Assumptions look good
acf(SPLT_SAC_Dist_Global_lm$residuals) # Autocorreltion is fine
vif(SPLT_SAC_Dist_Global_lm) # All < 2

## All subset selection
SPLT_SAC_Dist_Global_Dredge = dredge(SPLT_SAC_Dist_Global_lm)
SPLT_SAC_Dist_Global_Dredge
# Top model includes discharge
importance(SPLT_SAC_Dist_Global_Dredge)
# Discharge is most important variable; weights = 0.87 which isn't bad

SPLT_SAC_CDist_MA = model.avg(SPLT_SAC_Dist_Global_Dredge)
summary(SPLT_SAC_CDist_MA)

###### SJ
SPLT_SJ_Final
SPLT_SJ_Final_Scale


pairs(SPLT_SJ_Final[,3:10], pch = 16)
# Nothing really jumping out

SPLT_SJ_Dist_Global_lm <- 
  lm(log(SPLT_SJ_Final$CDist_SJ) ~ Mean_SJR_Discharge + Peak_Flow + Mean_WT_Max_AS, data = SPLT_SJ_Final_Scale, na.action = "na.fail")
summary(SPLT_SJ_Dist_Global_lm)
# Nothing...

# Model assumptions
plot(SPLT_SJ_Dist_Global_lm) # Normailtiy is iffy but probably fine...
acf(SPLT_SJ_Dist_Global_lm$residuals) # Autocorreltion is fine
vif(SPLT_SJ_Dist_Global_lm) # All < 2.5

## All subset selection
SPLT_SJ_Dist_Global_Dredge = dredge(SPLT_SJ_Dist_Global_lm)
SPLT_SJ_Dist_Global_Dredge
# Top model is null model
importance(SPLT_SJ_Dist_Global_Dredge)
# No variables are important 

SPLT_SJ_CDist_MA = model.avg(SPLT_SJ_Dist_Global_Dredge)
summary(SPLT_SJ_CDist_MA)

# SASU --------------------------------------------------------------------

###### Sac
SASU_SAC_Final
SASU_SAC_Final_Scale

pairs(SASU_SAC_Final[,3:13], pch = 16)
# Negative with peak flow (timing of flow)
# Nothing with CDist in December

# CDist vs mean discharge during spawning
SASU_SAC_CDist_Dis_Plot <- 
  SASU_SAC_Final_Scale %>% 
  ggplot(aes(x = Peak_Flow, y = log(SASU_SAC_Final$CDist_SAC))) +
  geom_point() +
  theme_classic()
SASU_SAC_CDist_Dis_Plot  
# Fairly good relationship

SASU_SAC_CDist_Global = lm(log(SASU_SAC_Final$CDist_SAC) ~ Mean_SAC_Discharge + Peak_Flow + Mean_WT_Max_AS, data = SASU_SAC_Final_Scale, na.action = "na.fail")
summary(SASU_SAC_CDist_Global)
# Peak flow appears to be most important variable
# Negative relationship
# High flows might push SASU from their natal tribs or just push farther DS

# Model assumptions
plot(SASU_SAC_CDist_Global) # Assumptions look good
acf(SASU_SAC_CDist_Global$residuals) # Autocorreltion is fine
vif(SASU_SAC_CDist_Global) # All < 2

## All subset selection
SASU_SAC_Dist_Global_Dredge = dredge(SASU_SAC_CDist_Global)
SASU_SAC_Dist_Global_Dredge
# Top model includes peak flow
importance(SASU_SAC_Dist_Global_Dredge)
# Peak Flow is most important variable; weights = 0.74

SASU_SAC_CDist_MA = model.avg(SASU_SAC_Dist_Global_Dredge)
summary(SASU_SAC_CDist_MA)


######## SJ
SASU_SJ_Final
SASU_SJ_Final_Scale

pairs(SASU_SJ_Final[,3:10], pch = 16)
# Nothing jumping out...maybe peak flow

# CDist vs mean discharge during spawning
SASU_SJ_CDist_Dis_Plot <- 
  SASU_SJ_Final_Scale %>% 
  ggplot(aes(x = Peak_Flow, y = log(SASU_SJ_Final$CDist_SJ))) +
  geom_point() +
  theme_classic()
SASU_SJ_CDist_Dis_Plot  
# Meh

SASU_SJ_CDist_Global = lm(log(SASU_SJ_Final$CDist_SJ) ~ Mean_SJR_Discharge + Peak_Flow + Mean_WT_Max_AS, data = SASU_SJ_Final_Scale, na.action = "na.fail")
summary(SASU_SJ_CDist_Global)
# Nothing unless r2adjusted of -0.13 is good...

# Model assumptions
plot(SASU_SJ_CDist_Global) # Normality is iffy but probably fine...
acf(SASU_SJ_CDist_Global$residuals) # Autocorreltion is fine
vif(SASU_SJ_CDist_Global) # All < 2.5

## All subset selection
SASU_SJ_Dist_Global_Dredge = dredge(SASU_SJ_CDist_Global)
SASU_SJ_Dist_Global_Dredge
# Top model is null model
importance(SASU_SJ_Dist_Global_Dredge)
# All are ~ 0.2

SASU_SJ_CDist_MA = model.avg(SASU_SJ_Dist_Global_Dredge)
summary(SASU_SJ_CDist_MA)

