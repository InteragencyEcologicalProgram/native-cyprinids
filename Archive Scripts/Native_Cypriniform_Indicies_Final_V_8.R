# Background --------------------------------------------------------------
# V_8 Gets rid of the DB code that takes a little while to run

# V_7 Updates DB code, removes subregion from index and environmental data calculations, removed FP inundation code, and removed WY flow code from scaled covariate dataframe 

# V_5 changes: changed Delta flow to SJ + SJR
# Previous versions used Sac outflow for Delta which discounted the importance of SJR

# Objectives of this study are:

# 1) Create time series of and determine if there are trends in age-0 abundance indcies in the Delta and the Sacramento and SJ rivers for Sacramento Splittial, Sucker, and Pikeminnow

# 2) Determine how mean flow during spawning, the timing (centroid) of flow, and mean water temp influence abundance

# 3) Determine how mean flow during spawning, the timing (centroid) of flow, and mean water temp influence the center of distribution of these fishes in the Sacramento River.

# 4) Discuss what regions are used for age-0 rearing (i.e. south delta is not an important rearing ground for age-0 SAPM and SASU). Can show with abundance index and heatmaps

# Packages ----------------------------------------------------------------

if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('patchwork')) install.packages('patchwork'); library('patchwork')
if (!require('lubridate')) install.packages('lubridate'); library('lubridate')
if (!require('odbc')) install.packages('odbc'); library('odbc') # database functions
if (!require('Kendall')) install.packages('Kendall'); library('Kendall')
if (!require('car')) install.packages('car'); library('car')
if (!require('MuMIn')) install.packages('MuMIn'); library('MuMIn')
if (!require('lme4')) install.packages('lme4'); library('lme4')
if (!require('afex')) install.packages('afex'); library('afex')
if (!require('trend')) install.packages('trend'); library('trend')
if (!require('rms')) install.packages('rms'); library('rms')
options(scipen=999) # Getting rid of scientific notation
windowsFonts(Times = windowsFont("Times New Roman"))



# _______Sample Data_______ -------------------------------------------------------

# Importing sample data that was obtained in Native_Cypriniform_Indicies_Final_V_7.R

# Outputting final sample data
Sample_Table <- 
read.csv("Data/Sample_Table_Final.csv")

Sample_Table_Final <- 
  Sample_Table %>%
  mutate(Year = as.factor(Year),
         Month = as.factor(Month))

head(Sample_Table_Final)
str(Sample_Table_Final)

# ________Catch Data________ --------------------------------------

# Importing catch data that was obtained in Native_Cypriniform_Indicies_Final_V_7.R

Catch_Table <-
read.csv("Data/Catch_Table.csv")

head(Catch_Table)

# ________Other Data________ --------------------------------------

# Discharge
# Importing discharge data
# Received from DAYFLOW

Discharge <- 
  read.csv("data/dayflow_1994_2019.csv")
head(Discharge)
tail(Discharge)
str(Discharge)
Discharge$Year <- as.factor(Discharge$Year)
Discharge$Mo <- as.factor(Discharge$Mo)

# River mile
# Will be used to determine center of distribution 

Site_RM <- 
  read.csv("Data/Site_Coords.csv")
head(Site_RM)
str(Site_RM)


# Selecting Stations ------------------------------------------------------

Sample_Data <- 
  Sample_Table_Final %>% 
  filter(Month %in% c(5:7)) %>% 
  mutate(DV = 1)
head(Sample_Data)

Sample_Data_HM <- 
  Sample_Data %>% 
  filter(GearConditionCode   != 4,
         !(StationCode %in% c("SJ079E", "SJ076W", 
                              "SJ074A", "SJ058E"))) %>% 
  group_by(StationCode, Year, Month) %>% 
  summarise(SampleCount = sum(DV))
head(Sample_Data_HM)  

Sample_Data_HM_Plot <- 
  ggplot(Sample_Data_HM, aes(Year, StationCode)) +
  geom_tile(aes(fill = SampleCount), color = "white") +
  ggtitle("Sample_HM") +
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_fill_gradient(low = "white", high = "steelblue")
Sample_Data_HM_Plot

# Final stations that will be used in the analyses
Stations_Abridged <- 
  unique(Sample_Data_HM$StationCode)


# SAPM Data ---------------------------------------------------------------

Catch_Table_SAPM <- 
  Catch_Table %>% 
  select(CatchID, SampleID, OrganismCode, ForkLength, CatchCount) %>% 
  filter(OrganismCode == "SAPM")  # Only keeping catch for SAPM
head(Catch_Table_SAPM)
str(Catch_Table_SAPM)

# Linking SAPM catch data to sample DF
SAPM_Data <- 
  Sample_Table_Final %>% 
  left_join(Catch_Table_SAPM, 
            by = "SampleID") %>% 
  filter(!(is.na(SeineVolume)), # Removing samples without volume data
         StationCode %in% Stations_Abridged) %>% 
  select(SampleID, OrganismCode, StationCode, subarea, 
         Area, SampleDate,
         Julian_Date,Year, Month, SampleTime, MethodCode,
         GearConditionCode, DO,
         WaterTemperature, Turbidity, Secchi,
         WaterVelocity, Conductivity,
         SeineLength, SeineWidth, SeineDepth,SeineVolume, ForkLength, 
         CatchCount) %>% 
  replace_na(list(OrganismCode = ""))
head(SAPM_Data)
tail(SAPM_Data)
str(SAPM_Data)

# SPLT Data ---------------------------------------------------------------

Catch_Table_SPLT <- 
  Catch_Table %>% 
  select(CatchID, SampleID, OrganismCode, ForkLength, CatchCount) %>% 
  filter(OrganismCode == "SPLT")  # Only keeping catch for SPLT
head(Catch_Table_SPLT)

SPLT_Data <- 
  Sample_Table_Final %>% 
  left_join(Catch_Table_SPLT, 
            by = "SampleID") %>% 
  filter(!(is.na(SeineVolume)), # Removing samples without volume data
         StationCode %in% Stations_Abridged) %>%
  select(SampleID, OrganismCode, StationCode,
         subarea, Area, SampleDate,
         Julian_Date,Year, Month, SampleTime, 
         MethodCode, GearConditionCode, DO,
         WaterTemperature, Turbidity, 
         Secchi, WaterVelocity, Conductivity,
         SeineLength, SeineWidth, SeineDepth,SeineVolume, ForkLength, 
         CatchCount) %>% 
  replace_na(list(OrganismCode = ""))
head(SPLT_Data)
tail(SPLT_Data)
str(SPLT_Data)

# SASU Data ---------------------------------------------------------------

Catch_Table_SASU <- 
  Catch_Table %>% 
  select(CatchID, SampleID, OrganismCode, ForkLength, CatchCount) %>% 
  filter(OrganismCode == "SASU")  # Only keeping catch for SASU
head(Catch_Table_SASU)

SASU_Data <- 
  Sample_Table_Final %>% 
  left_join(Catch_Table_SASU, 
            by = "SampleID") %>% 
  filter(!(is.na(SeineVolume)), # Removing samples without volume data
         StationCode %in% Stations_Abridged) %>% # Selecting stations 
  select(SampleID, OrganismCode, StationCode, subarea, Area, SampleDate,
         Julian_Date,Year, Month, SampleTime, 
         MethodCode, GearConditionCode, DO,
         WaterTemperature, Turbidity, Secchi, WaterVelocity, Conductivity,
         SeineLength, SeineWidth, SeineDepth,SeineVolume, ForkLength, 
         CatchCount) %>%  
  replace_na(list(OrganismCode = ""))
head(SASU_Data)
tail(SASU_Data)
str(SASU_Data)

# ***Length Frequency Analyses -----------------------------------------------

## Length Frequency: Staring with length frequency analyses/histrograms to determine what months and length cutoffs to use when creating age-0 abundance indices 

# Creating object to label x axis to better see length cutoffs for age-0 fish
FL_Labels <- seq(0, 200, by = 10)



# SAPM --------------------------------------------------------------------

# Looking to see what months have the highest catches and to see how distributions change
SAPM_Data %>% 
  filter(OrganismCode == "SAPM", 
         ForkLength > 24, ForkLength < 200) %>%
  ggplot(aes(x = ForkLength, color = Area))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  facet_wrap(~Month)
# Looks like age-0 fish are showing up in months 6 but high through 8
# Catches of age-0 fish in month 12 occur mainly in the Delta and Sacramento River
# Catches in the SJ occur mainly in months 5 and 6 (but are negligible in general)

# See if months with highest catches depend on year
SAPM_Data %>% 
  filter(OrganismCode == "SAPM", 
         ForkLength > 24, ForkLength < 60,
         Month %in% c(3:8)) %>% 
  group_by(Year, Month, ForkLength) %>% 
  summarise(CatchCount = sum(CatchCount)) %>% 
  ggplot(aes(x = ForkLength, y = CatchCount))+
  geom_line(aes(color = Month))+
  facet_wrap(~Year)+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)
# Too busy to determine much but months with the highest catches of age-0 fish depend on year

# To see what months have highest catches of SAPM
SAPM_Catch_By_Month <- 
  SAPM_Data %>% 
  filter(OrganismCode == "SAPM", 
         ForkLength > 24, ForkLength < 60)%>%
  group_by(Month)%>% 
  summarise(Sum_Catch_Per_Month = sum(CatchCount))
SAPM_Catch_By_Month

# Highest catches in months 3 - 7
str(SAPM_Catch_By_Month)

SAPM_Catch_By_Month_Year <- 
  SAPM_Data %>% 
  filter(OrganismCode == "SAPM", 
         ForkLength > 24, ForkLength < 60)%>%
  group_by(Month, Year, Area)%>% 
  summarise(Sum_Catch_Per_Month = sum(CatchCount))
SAPM_Catch_By_Month_Year

SAPM_Catch_By_Month_Year$Month = as.numeric(SAPM_Catch_By_Month_Year$Month)
ggplot(data = SAPM_Catch_By_Month_Year, 
       aes(x = Month, y = Sum_Catch_Per_Month, color = Area)) +
  geom_line() +
  facet_wrap(~ Year)

# Length frequency analyses

# March
SAPM_Data %>% 
  filter(OrganismCode == "SAPM", 
         Month == 3, 
         ForkLength > 24, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# April
SAPM_Data %>% 
  filter(OrganismCode == "SAPM",
         Month == 4, 
         ForkLength > 24, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# May
SAPM_Data %>% 
  filter(OrganismCode == "SAPM",
         Month == 5, 
         ForkLength > 24, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# Age-0 fish start showing up in June/July
# We are missing the left tail of the distribution...therefore probably missing important (potential) variability
# June
SAPM_Data %>% 
  filter(OrganismCode == "SAPM", 
         Month == 6, 
         ForkLength > 24, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# July
SAPM_Data %>% 
  filter(OrganismCode == "SAPM", 
         Month == 7, 
         ForkLength > 24, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# Based on length freq histograms, age 0 fish don't start showing up until June and July
# June cuttoff for age-0 fish is ~ 60 mm
# July cuttoff for age-0 fish is ~ 70 mm


# SPLT --------------------------------------------------------------------

# Looking to see what months have the highest catches and to see how distributions change
SPLT_Data %>% 
  filter(OrganismCode == "SPLT", 
         ForkLength > 24, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  facet_wrap(~Month)
# Much easier than the other species. Only really catch in May and June. Some catch in July though 

# See if months with highest catches depend on year
SPLT_Data %>% 
  filter(OrganismCode == "SPLT",
         ForkLength > 24, ForkLength < 60, Month %in% 4:8) %>% 
  group_by(Year, Month, ForkLength) %>% 
  summarise(CatchCount = sum(CatchCount)) %>% 
  ggplot(aes(x = ForkLength, y = CatchCount))+
  geom_line(aes(color = Month))+
  facet_wrap(~Year)+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)
# Highest catch consistently in May. Year or two in June

# To see what months have highest catches of SPLT
SPLT_Catch_By_Month <- 
  SPLT_Data %>% 
  filter(OrganismCode == "SPLT", 
         ForkLength > 24, ForkLength < 200)%>%
  group_by(Month)%>% 
  summarise(Sum_Catch_Per_Month = sum(CatchCount))
SPLT_Catch_By_Month
# Highest catches in months 5 - 6...same as above

SPLT_Catch_By_Month_Year <- 
  SPLT_Data %>% 
  filter(OrganismCode == "SPLT", 
         ForkLength > 24, ForkLength < 60, 
         Month %in% c(4:7))%>%
  group_by(Month, Year, Area)%>% 
  summarise(Sum_Catch_Per_Month = sum(CatchCount))
SPLT_Catch_By_Month_Year

SPLT_Catch_By_Month_Year$Month = as.numeric(SPLT_Catch_By_Month_Year$Month)
ggplot(data = SPLT_Catch_By_Month_Year, aes(x = Month, y = Sum_Catch_Per_Month, color = Area)) +
  geom_point()+
  facet_wrap(~ Year, scales = "free_y")
## There are a couple of months where catch is higher in month 4 than 6, but this only occurs in one area (2 years in the Delta). Feel confident that we can just focus analyeses on months 5 and 6. 

# May
SPLT_Data %>% 
  filter(OrganismCode == "SPLT",
         Month == 5, 
         ForkLength > 24, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength, fill = Year))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# June
SPLT_Data %>% 
  filter(OrganismCode == "SPLT",
         Month == 6, 
         ForkLength > 24, ForkLength < 200) %>% 
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
SASU_Data %>% 
  filter(OrganismCode == "SASU",
         ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength, color = Area))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  facet_wrap(~Month)
# Highest catches in months 5 and 6 but also high catches in months 3:4, 7, and 12
# Catches in month 12 (3rd highest catches of the year...) occur mainly in the Delta and Sacramento River
# Catches in the SJ occur mainly in months 5 and 6
# In general, catches are spread out over much more of the year than SPLT, or SAPM

# See if months with highest catches depend on year
SASU_Data %>% 
  filter(OrganismCode == "SASU", 
         ForkLength > 24, ForkLength < 60, 
         Month %in% c(4:8, 12)) %>% 
  group_by(Year, Month, ForkLength) %>% 
  summarise(CatchCount = sum(CatchCount)) %>% 
  ggplot(aes(x = ForkLength, y = CatchCount))+
  geom_line(aes(color = Month))+
  facet_wrap(~Year)+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)
# Too busy but...catches tend to be highest in May and June
# In some years catches are highest in December...
# This might be interesting to look at in a different project

# Highest catches in months 4 - 7, 12; Peak in 5 and 6

# Length frequency analyses

# April
SASU_Data %>% 
  filter(OrganismCode == "SASU", ForkLength, Month == 4, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# May
SASU_Data %>% 
  filter(OrganismCode == "SASU", ForkLength, Month == 5, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# June
SASU_Data %>% 
  filter(OrganismCode == "SASU", ForkLength, Month == 6, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# July
SASU_Data %>% 
  filter(OrganismCode == "SASU", ForkLength, Month == 7, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# December
SASU_Data %>% 
  filter(OrganismCode == "SASU", ForkLength, Month == 12, ForkLength < 200) %>% 
  ggplot(aes(x = ForkLength))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = c(0,.5), labels = FL_Labels, breaks = FL_Labels)+
  theme_classic(base_size = 15)

# Going to use months 5 and 6 because they have the highest catches of age-0 fish
# May cuttoff for age-0 fish is ~ 50 mm
# June cuttoff for age-0 fish is ~ 65 mm
# I am also going to look at December because it's interesting to have a peak later in the year. They are thought to spawn upstream in tribs so the catches in December might have something to do with flow. Brian...any ideas?


#***Plus Count Proportions --------------------------------------------------

# I will be using FL of Age-0 fishes caught within a seine haul to proporiton FL of plus count fish

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
  SAPM_Data %>% 
  filter(OrganismCode == "SAPM", 
         ForkLength == 0, 
         Month == 6) %>%
  group_by(SampleID) %>% 
  summarise(Plus_Count = sum(CatchCount))

SAPM_FL_Count_June <- 
  SAPM_Data %>% 
  filter(OrganismCode == "SAPM", 
         ForkLength > 0, 
         Month == 6) %>%
  group_by(SampleID)%>% 
  summarise(FL_Count = sum(CatchCount))

# Count of all fish (including plus counts) that were measured in June
SAPM_Total_June <- 
  full_join(SAPM_FL_Count_June, 
            SAPM_Plus_Count_June, 
            by = "SampleID") %>% 
  replace_na(list(FL_Count = 0, 
                  Plus_Count = 0)) %>% 
  mutate(Total_Count = FL_Count + Plus_Count)
head(SAPM_Total_June)
str(SAPM_Total_June)
# Getting count of all age-0 fish that were caught in June (60 mm cutoff)

SAPM_FL_Range_June <- 
  SAPM_Data %>% 
  filter(OrganismCode == "SAPM", 
         ForkLength > 24, ForkLength < 61, 
         Month == 6) %>%
  group_by(SampleID) %>% 
  summarise(FL_Range_Count = sum(CatchCount))

SAPM_Total_June_Final <- 
  left_join(SAPM_Total_June, 
            SAPM_FL_Range_June, 
            by = "SampleID") %>% 
  replace_na(list(FL_Range_Count = 0))
head(SAPM_Total_June_Final)
str(SAPM_Total_June_Final)

## July
SAPM_Plus_Count_July <- 
  SAPM_Data %>% 
  filter(OrganismCode == "SAPM", 
         ForkLength == 0, 
         Month == 7) %>%
  group_by(SampleID) %>% 
  summarise(Plus_Count = sum(CatchCount))

SAPM_FL_Count_July <- 
  SAPM_Data %>% 
  filter(OrganismCode == "SAPM", 
         ForkLength > 0, 
         Month == 7)%>%
  group_by(SampleID)%>% 
  summarise(FL_Count = sum(CatchCount))

SAPM_Total_July <- 
  full_join(SAPM_FL_Count_July, 
            SAPM_Plus_Count_July,  
            by = "SampleID") %>% 
  replace_na(list(FL_Count = 0, 
                  Plus_Count = 0)) %>% 
  mutate(Total_Count = FL_Count + Plus_Count)
head(SAPM_Total_July) 

# July age-0 cuttoff is 70 mm
SAPM_FL_Range_July <- 
  SAPM_Data %>% 
  filter(OrganismCode == "SAPM", 
         ForkLength > 24, ForkLength <71,
         Month == 7) %>%
  group_by(SampleID) %>% 
  summarise(FL_Range_Count = sum(CatchCount))
head(SAPM_FL_Range_July)

SAPM_Total_July_Final <- 
  left_join(SAPM_Total_July, 
            SAPM_FL_Range_July, 
            by = "SampleID") %>% 
  replace_na(list(FL_Range_Count = 0))
head(SAPM_Total_July_Final)
str(SAPM_Total_July_Final)

## Combining both months
SAPM_Final <- 
  bind_rows(SAPM_Total_June_Final, 
            SAPM_Total_July_Final)
head(SAPM_Final)
str(SAPM_Final)

# DF where I am calculating adjusted count
# Also standardizing Volume, Julian Date, and Julian Date^2 for model to est. abun.
SAPM_Index <- 
  full_join(subset(SAPM_Data, !duplicated(SampleID)), 
            SAPM_Final, 
            by = "SampleID") %>% 
  mutate(Adjusted_Count = Total_Count * (FL_Range_Count/FL_Count)) %>% 
  replace_na(list(CatchCount = 0, 
                  FL_Count = 0, 
                  Plus_Count = 0,
                  Total_Count = 0, 
                  FL_Range_Count = 0, 
                  Adjusted_Count = 0)) %>% 
  filter(Month %in% c(6:7)) %>% 
  mutate(CPUE = Adjusted_Count/SeineVolume,
         Volume_Z = scale(SeineVolume),
         Julian_Z = scale(Julian_Date),
         Julian_Sq_Z = scale(Julian_Date^2),
         Region = as.factor(case_when(subarea %in% c(1:5) ~ "Delta",
                            subarea %in% c(6:8) ~ "Sacramento",
                            subarea %in% c(9:10) ~ "San_Joaquin")))
head(SAPM_Index, 20) 

# Outputting data
write.csv(SAPM_Index, "Data/SAPM_Index.csv", row.names = FALSE)

# SPLT --------------------------------------------------------------------

# Based on length freq histograms, age 0 fish make up the majority of the catch during both months

# May cuttoff for age-0 fish is ~ 50 mm
# June cuttoff for age-0 fish is ~ 60 mm

## May
SPLT_Plus_Count_May <- 
  SPLT_Data %>% 
  filter(OrganismCode == "SPLT", 
         ForkLength == 0, 
         Month == 5) %>%
  group_by(SampleID) %>% 
  summarise(Plus_Count = sum(CatchCount))

SPLT_FL_Count_May <- 
  SPLT_Data %>% 
  filter(OrganismCode == "SPLT", 
         ForkLength > 0,
         Month == 5) %>%
  group_by(SampleID) %>% 
  summarise(FL_Count = sum(CatchCount))

SPLT_Total_May <- 
  full_join(SPLT_FL_Count_May, 
            SPLT_Plus_Count_May,  
            by = "SampleID") %>% 
  replace_na(list(FL_Count = 0, 
                  Plus_Count = 0)) %>% 
  mutate(Total_Count = FL_Count + Plus_Count)
head(SPLT_Total_May)

# Age-0 cutoff in May is 50 mm
SPLT_FL_Range_May <- 
  SPLT_Data %>% 
  filter(OrganismCode == "SPLT", 
         ForkLength > 24, ForkLength <51, 
         Month == 5) %>%
  group_by(SampleID) %>% 
  summarise(FL_Range_Count = sum(CatchCount))

SPLT_Total_May_Final <- 
  left_join(SPLT_Total_May, 
            SPLT_FL_Range_May, 
            by = "SampleID") %>% 
  replace_na(list(FL_Range_Count = 0))
head(SPLT_Total_May_Final)

## June
SPLT_Plus_Count_June <- 
  SPLT_Data %>% 
  filter(OrganismCode == "SPLT", 
         ForkLength == 0, 
         Month == 6)%>%
  group_by(SampleID)%>% 
  summarise(Plus_Count = sum(CatchCount))

SPLT_FL_Count_June <- 
  SPLT_Data %>% 
  filter(OrganismCode == "SPLT", 
         ForkLength > 0, 
         Month == 6)%>%
  group_by(SampleID)%>% 
  summarise(FL_Count = sum(CatchCount))

SPLT_Total_June <- 
  full_join(SPLT_FL_Count_June, 
            SPLT_Plus_Count_June,  
            by = "SampleID") %>% 
  replace_na(list(FL_Count = 0,
                  Plus_Count = 0)) %>% 
  mutate(Total_Count = FL_Count + Plus_Count)

SPLT_FL_Range_June <- 
  SPLT_Data %>% 
  filter(OrganismCode == "SPLT", 
         ForkLength > 24, 
         ForkLength <61, 
         Month == 6)%>%
  group_by(SampleID)%>% 
  summarise(FL_Range_Count = sum(CatchCount))

SPLT_Total_June_Final <- 
  left_join(SPLT_Total_June, 
            SPLT_FL_Range_June, 
            by = "SampleID") %>% 
  replace_na(list(FL_Range_Count = 0))
head(SPLT_Total_June_Final)


## Combining all months
SPLT_Final <- 
  bind_rows(SPLT_Total_May_Final, 
            SPLT_Total_June_Final)
head(SPLT_Final)
str(SPLT_Final)

# DF where I am calculating adjusted count
# Also standardizing Volume, Julian Date, and Julian Date^2 for model to est. abun.
SPLT_Index <- 
  full_join(subset(SPLT_Data, !duplicated(SampleID)), 
            SPLT_Final, 
            by = "SampleID") %>% 
  mutate(Adjusted_Count = Total_Count * (FL_Range_Count/FL_Count)) %>% 
  replace_na(list(CatchCount = 0,
                  FL_Count = 0, 
                  Plus_Count = 0,
                  Total_Count = 0, 
                  FL_Range_Count = 0, 
                  Adjusted_Count = 0)) %>% 
  filter(Month %in% c(5:6)) %>% 
  mutate(CPUE = Adjusted_Count/SeineVolume,
         Volume_Z = scale(SeineVolume),
         Julian_Z = scale(Julian_Date),
         Julian_Sq_Z = scale(Julian_Date^2),
         Region = case_when(subarea %in% c(1:5) ~ "Delta",
                            subarea %in% c(6:8) ~ "Sacramento",
                            subarea %in% c(9:10) ~ "San_Joaquin"))
head(SPLT_Index, 20) 

# Outputting data
write.csv(SPLT_Index, "Data/SPLT_Index.csv", row.names = FALSE)

# SASU --------------------------------------------------------------------
# Going to use months 5 and 6 because they have the highest catches

# Based on length freq histograms, age 0 fish make up the majority of the catch during both months

# May cuttoff for age-0 fish is ~ 50 mm
# June cuttoff for age-0 fish is ~ 65 mm

## May
SASU_Plus_Count_May <- 
  SASU_Data %>% 
  filter(OrganismCode == "SASU",
         ForkLength == 0, 
         Month == 5) %>%
  group_by(SampleID) %>% 
  summarise(Plus_Count = sum(CatchCount))

SASU_FL_Count_May <- 
  SASU_Data %>% 
  filter(OrganismCode == "SASU", 
         ForkLength > 0, 
         Month == 5) %>%
  group_by(SampleID) %>% 
  summarise(FL_Count = sum(CatchCount))

SASU_Total_May <- 
  full_join(SASU_FL_Count_May, 
            SASU_Plus_Count_May,   
            by = "SampleID") %>% 
  replace_na(list(FL_Count = 0,
                  Plus_Count = 0)) %>% 
  mutate(Total_Count = FL_Count + Plus_Count)

SASU_FL_Range_May <- 
  SASU_Data %>% 
  filter(OrganismCode == "SASU", 
         ForkLength >= 25, 
         ForkLength < 51, 
         Month == 5) %>%
  group_by(SampleID) %>% 
  summarise(FL_Range_Count = sum(CatchCount))

SASU_Total_May_Final <- 
  left_join(SASU_Total_May, 
            SASU_FL_Range_May, 
            by = "SampleID") %>% 
  replace_na(list(FL_Range_Count = 0))
head(SASU_Total_May_Final)

## June
SASU_Plus_Count_June <- 
  SASU_Data %>% 
  filter(OrganismCode == "SASU",
         ForkLength == 0, 
         Month == 6)%>%
  group_by(SampleID)%>% 
  summarise(Plus_Count = sum(CatchCount))

SASU_FL_Count_June <- 
  SASU_Data %>% 
  filter(OrganismCode == "SASU", 
         ForkLength > 0, 
         Month == 6) %>%
  group_by(SampleID)%>% 
  summarise(FL_Count = sum(CatchCount))

SASU_Total_June <- 
  full_join(SASU_FL_Count_June, 
            SASU_Plus_Count_June,  
            by = "SampleID") %>% 
  replace_na(list(FL_Count = 0,
                  Plus_Count = 0)) %>% 
  mutate(Total_Count = FL_Count + Plus_Count)
head(SASU_Total_June)

SASU_FL_Range_June <- 
  SASU_Data %>% 
  filter(OrganismCode == "SASU",
         ForkLength >= 25, 
         ForkLength < 66, 
         Month == 6)%>%
  group_by(SampleID)%>% 
  summarise(FL_Range_Count = sum(CatchCount))

SASU_Total_June_Final <- 
  left_join(SASU_Total_June, 
            SASU_FL_Range_June, 
            by = "SampleID") %>% 
  replace_na(list(FL_Range_Count = 0))
head(SASU_Total_June_Final)

## December
# Don't plan on doing anything with this data in the MS but am calculating just in case
SASU_Plus_Count_December <- 
  SASU_Data %>% 
  filter(OrganismCode == "SASU", 
         ForkLength == 0, 
         Month == 12) %>%
  group_by(SampleID)%>% 
  summarise(Plus_Count = sum(CatchCount))

SASU_FL_Count_December <- 
  SASU_Data %>% 
  filter(OrganismCode == "SASU", 
         ForkLength > 0, 
         Month == 12) %>%
  group_by(SampleID)%>% 
  summarise(FL_Count = sum(CatchCount))

SASU_Total_December <- 
  full_join(SASU_FL_Count_December, 
            SASU_Plus_Count_December,  
            by = "SampleID") %>% 
  replace_na(list(FL_Count = 0,
                  Plus_Count = 0)) %>% 
  mutate(Total_Count = FL_Count + Plus_Count)
head(SASU_Total_December)

SASU_FL_Range_December <- 
  SASU_Data %>% 
  filter(OrganismCode == "SASU", 
         ForkLength >= 25, ForkLength <66,
         Month == 12)%>%
  group_by(SampleID)%>% 
  summarise(FL_Range_Count = sum(CatchCount))

SASU_Total_December_Final <- 
  left_join(SASU_Total_December, 
            SASU_FL_Range_December, 
            by = "SampleID") %>% 
  replace_na(list(FL_Range_Count = 0))
head(SASU_Total_December_Final)

## Combining all months
SASU_Final <- 
  bind_rows(SASU_Total_May_Final, 
            SASU_Total_June_Final, 
            SASU_Total_December_Final)
head(SASU_Final)
str(SASU_Final)

# DF where I am calculating adjusted count
# Also standardizing Volume, Julian Date, and Julian Date^2 for model to est. abun.
SASU_Index <- 
  full_join(subset(SASU_Data, !duplicated(SampleID)), 
            SASU_Final, 
            by = "SampleID") %>% 
  mutate(Adjusted_Count = Total_Count * (FL_Range_Count/FL_Count)) %>% 
  replace_na(list(CatchCount = 0, 
                  FL_Count = 0, 
                  Plus_Count = 0,
                  Total_Count = 0, 
                  FL_Range_Count = 0, 
                  Adjusted_Count = 0)) %>% 
  filter(Month %in% c(5:6, 12)) %>% 
  mutate(CPUE = Adjusted_Count/SeineVolume,
         Volume_Z = scale(SeineVolume),
         Julian_Z = scale(Julian_Date),
         Julian_Sq_Z = scale(Julian_Date^2),
         Region = case_when(subarea %in% c(1:5) ~ "Delta",
                            subarea %in% c(6:8) ~ "Sacramento",
                            subarea %in% c(9:10) ~ "San_Joaquin"))
head(SASU_Index, 20) 

# Outputting data
write.csv(SASU_Index, "Data/SASU_Index.csv", row.names = FALSE)

# ***Indicies ----------------------------------------------------------------

## Age-0 Indicies: Using plus count proportioned data to create age-0 abundance indices

# SAPM --------------------------------------------------------------------

# Calculating mean June and July CPUE for each station 
# We don't sample subarea 8 anymore so I am going to remove it

##### Subarea 8 is no longer beind sampled...removing subarea 8 to see if it influences results of Sac Models
SAPM_Index_Final <-
  SAPM_Index %>%
  filter(subarea != 8) %>%
  group_by(StationCode, Year, Month, Region) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Month, Region) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Region) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  spread(Region, CPUE)


SAPM_Index_Final$Delta_Index = SAPM_Index_Final$Delta
SAPM_Index_Final$Sacramento_Index = SAPM_Index_Final$Sacramento
SAPM_Index_Final$San_Joaquin_Index = SAPM_Index_Final$San_Joaquin
SAPM_Index_Final$Watershed_Index = rowSums(SAPM_Index_Final[,2:4])
head(SAPM_Index_Final)

write.csv(SAPM_Index_Final, "Output/SAPM_Index_Final.csv", row.names = FALSE)

# SPLT --------------------------------------------------------------------

# Subarea 8 is no longer beind sampled...removing subarea 8 
SPLT_Index_Final <-
  SPLT_Index %>%
  filter(subarea != 8) %>%
  group_by(StationCode, Year, Month, Region) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Month, Region) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Region) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  spread(Region, CPUE)


SPLT_Index_Final$Delta_Index = SPLT_Index_Final$Delta
SPLT_Index_Final$Sacramento_Index = SPLT_Index_Final$Sacramento
SPLT_Index_Final$San_Joaquin_Index = SPLT_Index_Final$San_Joaquin
SPLT_Index_Final$Watershed_Index = rowSums(SPLT_Index_Final[,2:4])
head(SPLT_Index_Final)

write.csv(SPLT_Index_Final, "Output/SPLT_Index_Final.csv", row.names = FALSE)


# SASU --------------------------------------------------------------------

# Subarea 8 is no longer beind sampled...removing subarea 8 
SASU_Index_Final <-
  SASU_Index %>%
  filter(subarea != 8,
         Month != 12) %>% # Removing December
  group_by(StationCode, Year, Month, Region) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Month, Region) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  group_by(Year, Region) %>% 
  summarise(CPUE = mean(CPUE)) %>% 
  spread(Region, CPUE)


SASU_Index_Final$Delta_Index = SASU_Index_Final$Delta
SASU_Index_Final$Sacramento_Index = SASU_Index_Final$Sacramento
SASU_Index_Final$San_Joaquin_Index = SASU_Index_Final$San_Joaquin
SASU_Index_Final$Watershed_Index = rowSums(SASU_Index_Final[,2:4])
head(SASU_Index_Final)

write.csv(SASU_Index_Final, "Output/SASU_Index_Final.csv", row.names = FALSE)


# ***Center of Dist --------------------------------------------------------------

## Centroid (i.e., center of distribution)
 
# Calculated using the months used to make the indices
# Formula taken from Sommer et al. 2011
# CD = sum(RiverKilo * CPUE)/sum(CPUE)
# Going to be used as response variable in model with flow/timing/temp as covariates (below)

# Removed subarea 8 because we haven't sampled it since 2013


# SAPM --------------------------------------------------------------------

head(SAPM_Index)
head(Site_RM)

SAPM_RM <- 
  SAPM_Index %>% 
  left_join(Site_RM, 
            by = "StationCode") %>% 
  filter(CPUE != 0) # Removing records where CPUE was 0. This will prevent NAs in SJ when there were years w/ 0 catch (OW 0 CPUE shouldn't matter)
head(SAPM_RM)

# Sacramento River
SAPM_RM_Sac <-
  SAPM_RM %>% 
  filter(grepl("SR", StationCode),
         subarea != "8") # Selecting stations that begin w/ SR
head(SAPM_RM_Sac)

# SJ River
SAPM_RM_SJ <-
  SAPM_RM %>% 
  filter(grepl("SJ", StationCode)) # Selecting stations that begin w/ SJ
head(SAPM_RM_SJ)

SAPM_CDist_SAC <- 
  SAPM_RM_Sac %>% 
  filter(Year %in% c(1995:2019)) %>% 
  group_by(Year) %>% 
  summarise(CDist_SAC = sum(RiverKilo * CPUE)/sum(CPUE))
head(SAPM_CDist_SAC)

SAPM_Month_CDist_SAC <- 
  SAPM_RM_Sac %>% 
  filter(Year %in% c(1995:2019)) %>% 
  group_by(Year, 
           Month) %>% 
  summarise(CDist_SAC = sum(RiverKilo * CPUE)/sum(CPUE))
head(SAPM_Month_CDist_SAC)

SAPM_CDist_SJ <- 
  SAPM_RM_SJ %>% 
  filter(Year %in% c(1995:2019)) %>% 
  group_by(Year) %>% 
  summarise(CDist_SJ = sum(RiverKilo * CPUE)/sum(CPUE))
head(SAPM_CDist_SJ)

SAPM_CDist <- 
  SAPM_CDist_SAC %>% 
  left_join(SAPM_CDist_SJ,
            by = "Year")
head(SAPM_CDist)

# SPLT --------------------------------------------------------------------

head(SPLT_Index)
head(Site_RM)

SPLT_RM <- 
  left_join(SPLT_Index, 
            Site_RM, 
            by = "StationCode") %>% 
  filter(CPUE != 0) # Removing records where CPUE was 0
head(SPLT_RM)

# Sacramento River
SPLT_RM_Sac <-
  SPLT_RM %>% 
  filter(grepl("SR", StationCode),
         subarea != "8") # Selecting stations that begin w/ SR
head(SPLT_RM_Sac)

# SJ River
SPLT_RM_SJ <-
  SPLT_RM %>% 
  filter(grepl("SJ", StationCode)) # Selecting stations that begin w/ SJ
head(SPLT_RM_SJ)

SPLT_CDist_SAC <- 
  SPLT_RM_Sac %>% 
  filter(Year %in% c(1995:2019)) %>% 
  group_by(Year) %>% 
  summarise(CDist_SAC = sum(RiverKilo * CPUE)/sum(CPUE))
head(SPLT_CDist_SAC)

SPLT_Month_CDist_SAC <- 
  SPLT_RM_Sac %>% 
  filter(Year %in% c(1995:2019)) %>% 
  group_by(Year, Month) %>% 
  summarise(CDist_SAC = sum(RiverKilo * CPUE)/sum(CPUE))
head(SPLT_Month_CDist_SAC)

SPLT_CDist_SJ <- 
  SPLT_RM_SJ %>% 
  filter(Year %in% c(1995:2019)) %>% 
  group_by(Year) %>% 
  summarise(CDist_SJ = sum(RiverKilo * CPUE)/sum(CPUE))
head(SPLT_CDist_SJ)

SPLT_CDist <- 
  SPLT_CDist_SAC %>% 
  left_join(SPLT_CDist_SJ, 
            by = "Year")
head(SPLT_CDist)

# SASU --------------------------------------------------------------------

head(SASU_Index)
head(Site_RM)

SASU_RM <- 
  left_join(SASU_Index, 
            Site_RM, 
            by = "StationCode") %>% 
  filter(CPUE != 0,
         Month != 12) # Removing records where CPUE was 0
head(SASU_RM)

# Sacramento River
SASU_RM_Sac <-
  SASU_RM %>% 
  filter(grepl("SR", StationCode),
         subarea != "8") # Selecting stations that begin w/ SR
head(SASU_RM_Sac)

# SJ River
SASU_RM_SJ <-
  SASU_RM %>% 
  filter(grepl("SJ", StationCode)) # Selecting stations that begin w/ SJ
head(SASU_RM_SJ)

SASU_CDist_SAC <- 
  SASU_RM_Sac %>% 
  filter(Year %in% c(1995:2019)) %>% 
  group_by(Year) %>% 
  summarise(CDist_SAC = sum(RiverKilo * CPUE)/sum(CPUE))
head(SASU_CDist_SAC)

SASU_Month_CDist_SAC <- 
  SASU_RM_Sac %>% 
  filter(Year %in% c(1995:2019)) %>% 
  group_by(Year,
           Month) %>% 
  summarise(CDist_SAC = sum(RiverKilo * CPUE)/sum(CPUE))
head(SASU_Month_CDist_SAC)

SASU_CDist_SJ <- 
  SASU_RM_SJ %>% 
  filter(Year %in% c(1995:2019)) %>% 
  group_by(Year) %>% 
  summarise(CDist_SJ = sum(RiverKilo * CPUE)/sum(CPUE))
head(SASU_CDist_SJ)

SASU_CDist <- 
  SASU_CDist_SAC %>% 
  left_join(SASU_CDist_SJ, 
            by = "Year")
head(SASU_CDist)

# ***Covariates -----------------------------------------------------------

# i. Discharge ---------------------------------------------------------------

head(Discharge)

## Discharge data is taken DNR Day Flow data
# SJ = SJR
# Sac = SAC - not going to use YOLO because the sites are upstream of the bypass
# Delta = OUT 

# Creating julian day in discharge dataframe
JD_Dis <- as.POSIXlt(Discharge$Date, 
                    format = "%d-%b-%y")

Discharge$Julian_Date <- JD_Dis$yday + 1
# Julian data adds 1 to Jan 1st so it's not 0

# Variables were created below anticipating reviewers thinking about flow earlier that Jan

# Made a variable for water year w/ Oct as month 1
# Then did a julian date with Oct 1 as 1
# Did case when due to leap years

# This needs to be checked before it is used
Discharge_WY <- 
  Discharge %>% 
  mutate(Water_Year = as.factor(case_when(as.numeric(Mo) > 9 
                                          ~ as.numeric(Year) + 1994,
                                          as.numeric(Mo) < 10 
                                          ~ as.numeric(Year) + 1993)),
         Julian_Date_Water_Year = 
           case_when(Year %in% c(1996, 2000, 2004, 2008, 2012, 2016) 
                     & Julian_Date > 274 
                     ~ Julian_Date - 274,
                     !(Year %in% c(1996, 2000, 2004, 2008, 2012, 2016)) 
                     & Julian_Date > 273 
                     ~ Julian_Date - 273,
                     Year %in% c(1996, 2000, 2004, 2008, 2012, 2016) 
                     & Julian_Date < 274 
                     ~ Julian_Date + 92,
                     !(Year %in% c(1996, 2000, 2004, 2008, 2012, 2016)) 
                     & Julian_Date < 273 
                     ~ Julian_Date + 92))
                                       

# /// Mean Flow Spawning /// ----------------------------------------------

# Calculating mean discharge during spawning

# Calculate mean Delta flow below but probably won't use it because they don't spawn in the Delta


# SAPM --------------------------------------------------------------------

### Going to use mean discharge data for April and May (during peak spawning)

## San Joaquin
SJ_Mean_Discharge_SAPM <- 
  Discharge %>% 
  filter(Mo %in% c(4:5),
         Year %in% c(1995:2019)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SJ_Discharge = mean(SJR))
SJ_Mean_Discharge_SAPM

## Sacramento
SAC_Mean_Discharge_SAPM <- 
  Discharge %>% 
  filter(Mo %in% c(4:5),
         Year %in% c(1995:2019)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SAC_Discharge = mean(SAC))
SAC_Mean_Discharge_SAPM

## Delta
DELTA_Mean_Discharge_SAPM <- 
  Discharge %>% 
  filter(Mo %in% c(4:5),
         Year %in% c(1995:2019)) %>% 
  group_by(Year) %>% 
  summarise(Mean_DELTA_Discharge = mean(OUT))
DELTA_Mean_Discharge_SAPM

# Combining Data
SAPM_Mean_Disc <- 
  cbind(SJ_Mean_Discharge_SAPM, 
        SAC_Mean_Discharge_SAPM[,2], 
        DELTA_Mean_Discharge_SAPM[,2])
head(SAPM_Mean_Disc)

# SPLT --------------------------------------------------------------------

## SPLT
# May spawn from late feb through July but spawning after early may is unusual (Biology and Population Dynamics of Sacramento Splittail in the San Francisco Estuary: A Review)

# Going to use March and April

## San Joaquin
SJ_Mean_Discharge_SPLT <- 
  Discharge %>% 
  filter(Mo %in% c(3:4),
         Year %in% c(1995:2019)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SJ_Discharge = mean(SJR))
SJ_Mean_Discharge_SPLT

## Sacramento
SAC_Mean_Discharge_SPLT <- 
  Discharge %>% 
  filter(Mo %in% c(3:4),
         Year %in% c(1995:2019)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SAC_Discharge = mean(SAC))
SAC_Mean_Discharge_SPLT

## Delta
DELTA_Mean_Discharge_SPLT <- 
  Discharge %>% 
  filter(Mo %in% c(3:4),
         Year %in% c(1995:2019)) %>% 
  group_by(Year) %>% 
  summarise(Mean_DELTA_Discharge = mean(OUT))
DELTA_Mean_Discharge_SPLT

# Combining Data
SPLT_Mean_Disc <- 
  cbind(SJ_Mean_Discharge_SPLT, 
        SAC_Mean_Discharge_SPLT[,2], 
        DELTA_Mean_Discharge_SPLT[,2])
head(SPLT_Mean_Disc)

# SASU --------------------------------------------------------------------

# Peak spawning in March and April (Moyle)
# Going to look at how discharge in March and April influences abundance index
# Used May and June for index

# San Joaquin
SJ_Mean_Discharge_SASU <- 
  Discharge %>% 
  filter(Mo %in% c(3:4),
         Year %in% c(1995:2019)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SJ_Discharge = mean(SJR))
SJ_Mean_Discharge_SASU

## Sacramento
SAC_Mean_Discharge_SASU <- 
  Discharge %>% 
  filter(Mo %in% c(3:4),
         Year %in% c(1995:2019)) %>% 
  group_by(Year) %>% 
  summarise(Mean_SAC_Discharge = mean(SAC))
SAC_Mean_Discharge_SASU

## Delta
DELTA_Mean_Discharge_SASU <- 
  Discharge %>% 
  filter(Mo %in% c(3:4),
         Year %in% c(1995:2019)) %>% 
  group_by(Year) %>% 
  summarise(Mean_DELTA_Discharge = mean(OUT))
DELTA_Mean_Discharge_SASU

# Combining Data
SASU_Mean_Disc <- 
  cbind(SJ_Mean_Discharge_SASU, 
        SAC_Mean_Discharge_SASU[,2], 
        DELTA_Mean_Discharge_SASU[,2])
head(SASU_Mean_Disc)

# /// Timing of flows /// ------------------------------------------------------

## Centroid
# Looking at timing of flow by julian day in each region
# Formula taken from Sommer et al. 2011
# PF = sum(Julian day * flow)/sum(flow)
# Going to calculate timing of flow for Jan - May because peak spawning for all species should have started by May and high flows earlier may have allowed them to reach the spawning grounds

# Timing of flow by year 
# Going to use for all 3 species

# Also calc. timing by water year (Oct - May) in case reviewers want it

# SJ
Timing_Flow_SJR <- 
  Discharge %>% 
  filter(Mo %in% c(1:5),
         Year %in% c(1995:2019)) %>% 
  group_by(Year) %>% 
  summarise(SJ_Flow_Timing = sum(Julian_Date * SJR)/sum(SJR))
head(Timing_Flow_SJR)

Timing_Flow_Water_Year_SJR <- 
  Discharge_WY %>% 
  filter(Mo %in% c(10:12, 1:5),
         Water_Year %in% c(1995:2019)) %>% 
  group_by(Water_Year) %>% 
  summarise(SJ_Flow_Timing_WY = sum(Julian_Date_Water_Year * SJR)/sum(SJR))
head(Timing_Flow_Water_Year_SJR)

# Sac
Timing_Flow_SAC <- 
  Discharge %>% 
  filter(Mo %in% c(1:5),
         Year %in% c(1995:2019)) %>% 
  group_by(Year) %>% 
  summarise(SAC_Flow_Timing = sum(Julian_Date * SAC)/sum(SAC))
head(Timing_Flow_SAC)

Timing_Flow_Water_Year_SAC <- 
  Discharge_WY %>% 
  filter(Mo %in% c(10:12, 1:5),
         Water_Year %in% c(1995:2019)) %>% 
  group_by(Water_Year) %>% 
  summarise(SAC_Flow_Timing_WY = sum(Julian_Date_Water_Year * SAC)/sum(SAC))
head(Timing_Flow_Water_Year_SAC)

Timing_Flow_DELTA <- 
  Discharge %>% 
  filter(Mo %in% c(1:5),
         Year %in% c(1995:2019)) %>% 
  group_by(Year) %>% 
  summarise(DELTA_Flow_Timing = sum(Julian_Date * (OUT))/sum(OUT))
head(Timing_Flow_DELTA)

Timing_Flow_Water_Year_DELTA <- 
  Discharge_WY %>% 
  filter(Mo %in% c(10:12, 1:5),
         Water_Year %in% c(1995:2019)) %>% 
  group_by(Water_Year) %>% 
  summarise(DELTA_Flow_Timing_WY = sum(Julian_Date_Water_Year * (OUT))/sum(OUT))
head(Timing_Flow_Water_Year_DELTA)

# Combining data
Flow_Timing <- 
  cbind(Timing_Flow_SJR, 
        Timing_Flow_SAC[,2], 
        Timing_Flow_DELTA[,2])
head(Flow_Timing)

Flow_Timing_WY <- 
  cbind(Timing_Flow_Water_Year_SJR, 
        Timing_Flow_Water_Year_SAC[,2], 
        Timing_Flow_Water_Year_DELTA[,2])
head(Flow_Timing_WY)

# ii. Floodplain Inundation Duration ------------------------------------------

# Taken from Takata 2017
# Calculated as the number of days that mean Yolo Bypass flows met or exceeded 4000 cfs
# This variable was a good idea but is correlated with mean discharge
# Removed from analyses



# iii. Water Temperature -------------------------------------------------------
# Had code to get water temp from USGS stations in first version of this script
# There was bad spatial coverage so I decided to use temps from seine stations instead

# Going to use water temps from months that were used for indices because 
# 1) We know fish were there 
# 2) Temps during early life should control growth and ecosystem processes (which should influence survival)

# SAPM --------------------------------------------------------------------

head(SAPM_Index)

# San Joaquin
SAPM_SJ_WT <- 
  SAPM_Index %>% 
  filter(subarea %in% c(9:10), # Selecting subareas in SJ
         Month %in% c(6:7), # Months that were used to calculate index
         !(is.na(WaterTemperature))) %>% # Removing NAs water temp
  group_by(StationCode, 
           Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(WaterTemperature)) %>% 
  group_by(Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year) %>% 
  summarise(SJ_Mean_WT = mean(Mean_WT))
head(SAPM_SJ_WT)

# Sacramento
SAPM_SAC_WT <- 
  SAPM_Index %>% 
  filter(subarea %in% c(6:7), # SAC subareas (subarea 8 no longer sampled)
         Month %in% c(6:7), # Months that were used to calculate index
         !(is.na(WaterTemperature))) %>% # Removing NAs water temp
  group_by(StationCode, 
           Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(WaterTemperature)) %>% 
  group_by(Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year) %>% 
  summarise(SAC_Mean_WT = mean(Mean_WT))
head(SAPM_SAC_WT)

# Delta
SAPM_DELTA_WT <- 
  SAPM_Index %>% 
  filter(subarea %in% c(1:5), # DELTA subareas
         Month %in% c(6:7), # Months that were used to calculate index
         !(is.na(WaterTemperature))) %>% # Removing NAs water temp
  group_by(StationCode, 
           Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(WaterTemperature)) %>% 
  group_by(Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year) %>% 
  summarise(DELTA_Mean_WT = mean(Mean_WT))
head(SAPM_DELTA_WT)

SAPM_SAC_CDist_WT <- 
  SAPM_Index %>% 
  filter(grepl("SR", StationCode), # Stations on Sac River
         Month %in% c(6:7), # Months that were used to calculate index
         !(is.na(WaterTemperature))) %>% # Removing NAs water temp
  group_by(StationCode, 
           Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(WaterTemperature)) %>% 
  group_by(Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year) %>% 
  summarise(SAC_CDist_Mean_WT = mean(Mean_WT))
head(SAPM_SAC_CDist_WT)


SAPM_SJ_CDist_WT <- 
  SAPM_Index %>% 
  filter(grepl("SJ", StationCode), # Stations on SJ River
         Month %in% c(6:7), # Months that were used to calculate index
         !(is.na(WaterTemperature))) %>% # Removing NAs water temp
  group_by(StationCode, 
           Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(WaterTemperature)) %>% 
  group_by(Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year) %>% 
  summarise(SJ_CDist_Mean_WT = mean(Mean_WT))
head(SAPM_SJ_CDist_WT)

SAPM_WT <- 
  cbind(SAPM_SJ_WT, 
        SAPM_SAC_WT[,2], 
        SAPM_DELTA_WT[,2], 
        SAPM_SAC_CDist_WT[,2],
        SAPM_SJ_CDist_WT[,2])
head(SAPM_WT)

# SPLT --------------------------------------------------------------------

# Using may/june (same months used to calc index)

head(SPLT_Index)

# San Joaquin
SPLT_SJ_WT <- 
  SPLT_Index %>% 
  filter(subarea %in% c(9:10), # Selecting subareas in SJ
         Month %in% c(5:6), # Months that were used to calculate index
         !(is.na(WaterTemperature))) %>% # Removing NAs water temp
  group_by(StationCode, 
           Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(WaterTemperature)) %>% 
  group_by(Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year) %>% 
  summarise(SJ_Mean_WT = mean(Mean_WT))
head(SPLT_SJ_WT)

# Sacramento
SPLT_SAC_WT <- 
  SPLT_Index %>% 
  filter(subarea %in% c(6:7), # SAC subareas (subarea 8 no longer sampled)
         Month %in% c(5:6), # Months that were used to calculate index
         !(is.na(WaterTemperature))) %>% # Removing NAs water temp
  group_by(StationCode, 
           Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(WaterTemperature)) %>% 
  group_by(Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year) %>% 
  summarise(SAC_Mean_WT = mean(Mean_WT))
head(SPLT_SAC_WT)

# Delta
SPLT_DELTA_WT <- 
  SPLT_Index %>% 
  filter(subarea %in% c(1:5), # DELTA subareas
         Month %in% c(5:6), # Months that were used to calculate index
         !(is.na(WaterTemperature))) %>% # Removing NAs water temp
  group_by(StationCode, 
           Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(WaterTemperature)) %>% 
  group_by(Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year) %>% 
  summarise(DELTA_Mean_WT = mean(Mean_WT))
head(SPLT_DELTA_WT)

SPLT_SAC_CDist_WT <- 
  SPLT_Index %>% 
  filter(grepl("SR", StationCode), # Stations on Sac River
         Month %in% c(5:6), # Months that were used to calculate index
         !(is.na(WaterTemperature))) %>% # Removing NAs water temp
  group_by(StationCode, 
           Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(WaterTemperature)) %>% 
  group_by(Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year) %>% 
  summarise(SAC_CDist_Mean_WT = mean(Mean_WT))
head(SPLT_SAC_CDist_WT)

SPLT_SJ_CDist_WT <- 
  SPLT_Index %>% 
  filter(grepl("SJ", StationCode), # Stations on SJ River
         Month %in% c(6:7), # Months that were used to calculate index
         !(is.na(WaterTemperature))) %>% # Removing NAs water temp
  group_by(StationCode, 
           Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(WaterTemperature)) %>% 
  group_by(Year, 
           Month, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year, 
           Region) %>% 
  summarise(Mean_WT = mean(Mean_WT)) %>% 
  group_by(Year) %>% 
  summarise(SJ_CDist_Mean_WT = mean(Mean_WT))
head(SPLT_SJ_CDist_WT)

SPLT_WT <- 
  cbind(SPLT_SJ_WT, 
        SPLT_SAC_WT[,2], 
        SPLT_DELTA_WT[,2], 
        SPLT_SAC_CDist_WT[,2],
        SPLT_SJ_CDist_WT[,2])
head(SPLT_WT)

# SASU --------------------------------------------------------------------

# SASU index months (May/June) were the same as SPLT

SASU_WT <- 
  SPLT_WT
head(SASU_WT)

# iv. Final Covariate Data --------------------------------------------

# SAPM --------------------------------------------------------------------

SAPM_Covariates <- 
  cbind(SAPM_Mean_Disc, 
        Flow_Timing[,2:4], 
        SAPM_WT[,2:6])
head(SAPM_Covariates)

# Scaling covariates

SAPM_Covariates_Scaled <- 
  scale(SAPM_Covariates[,2:12])
head(SAPM_Covariates_Scaled)  

# SPLT --------------------------------------------------------------------

SPLT_Covariates <- 
  cbind(SPLT_Mean_Disc, 
        Flow_Timing[,2:4], 
        SPLT_WT[,2:6])
head(SPLT_Covariates)

# Scaling covariates

SPLT_Covariates_Scaled <- 
  scale(SPLT_Covariates[,2:12])
head(SPLT_Covariates_Scaled)

# SASU --------------------------------------------------------------------

SASU_Covariates <- 
  cbind(SASU_Mean_Disc, 
        Flow_Timing[,2:4], 
        SASU_WT[,2:6])
head(SASU_Covariates)

# Scaling covariates

SASU_Covariates_Scaled <- 
  scale(SASU_Covariates[,2:12])
head(SASU_Covariates_Scaled)

# ***Final Analyses ----------------------------------------------------------

# i. Summary Stats ---------------------------------------------------------------

# SAPM --------------------------------------------------------------------

head(SAPM_Index_Final)

SAPM_Index_Final_Long <- 
  pivot_longer(SAPM_Index_Final,
               cols = c(Delta_Index, 
                        Sacramento_Index, 
                        San_Joaquin_Index,
                        Watershed_Index),
               names_to = "Regions") %>% 
  select(Year, 
         Regions, 
         value)
head(SAPM_Index_Final_Long)

SAPM_Sum_Stats <- 
  SAPM_Index_Final_Long %>% 
  mutate(Species = "SAPM") %>% 
  group_by(Species, 
           Regions) %>% 
  summarise(Mean = mean(value), 
            Min = min(value), 
            Max = max(value), 
            SD = sd(value))
head(SAPM_Sum_Stats)

# SPLT --------------------------------------------------------------------

head(SPLT_Index_Final)

SPLT_Index_Final_Long <- 
  pivot_longer(SPLT_Index_Final,
               cols = c(Delta_Index, 
                        Sacramento_Index, 
                        San_Joaquin_Index,
                        Watershed_Index),
               names_to = "Regions") %>% 
  select(Year, 
         Regions, 
         value)
head(SPLT_Index_Final_Long)

SPLT_Sum_Stats <- 
  SPLT_Index_Final_Long %>% 
  mutate(Species = "SPLT") %>% 
  group_by(Species,
           Regions) %>% 
  summarise(Mean = mean(value), 
            Min = min(value), 
            Max = max(value), 
            SD = sd(value))
head(SPLT_Sum_Stats)

# SASU --------------------------------------------------------------------

head(SASU_Index_Final)

SASU_Index_Final_Long <- 
  pivot_longer(SASU_Index_Final,
               cols = c(Delta_Index, 
                        Sacramento_Index, 
                        San_Joaquin_Index,
                        Watershed_Index),
               names_to = "Regions") %>% 
  select(Year, 
         Regions, 
         value)
head(SASU_Index_Final_Long)

SASU_Sum_Stats <- 
  SASU_Index_Final_Long %>% 
  mutate(Species = "SASU") %>% 
  group_by(Species, 
           Regions) %>% 
  summarise(Mean = mean(value), 
            Min = min(value), 
            Max = max(value), 
            SD = sd(value))
head(SASU_Sum_Stats)


# ***Data Output*** -------------------------------------------------------------

Abun_Summaries <- 
  rbind(SAPM_Sum_Stats, 
        SPLT_Sum_Stats, 
        SASU_Sum_Stats)
head(Abun_Summaries)

write.csv(Abun_Summaries, "Tables/Abun_Summaries.csv", row.names = FALSE)

# ii. Trends ---------------------------------------------------------------

# Looking for trends using MannKendall Test.
# If there are trends going to run pettitt test to id a change point

# SAPM --------------------------------------------------------------------

head(SAPM_Index_Final)

# Delta Wide

acf(SAPM_Index_Final$Watershed_Index) # no autocorrelation

MannKendall(SAPM_Index_Final$Watershed_Index)
# tau = 0.147, 2-sided pvalue =0.31525
# No trend

# SJ
acf(SAPM_Index_Final$San_Joaquin_Index) # no autocorrelation

MannKendall(SAPM_Index_Final$San_Joaquin_Index)
# tau = 0.186, 2-sided pvalue = 0.24378
# No trend

# Proportion of SJ to DW index
MannKendall(SAPM_Index_Final$San_Joaquin_Index/SAPM_Index_Final$Watershed_Index)
# tau = 0.145, 2-sided pvalue = 0.36776

# Sac
acf(SAPM_Index_Final$Sacramento_Index) # no autocorrelation

MannKendall(SAPM_Index_Final$Sacramento_Index)
# tau = 0.173, 2-sided pvalue = 0.23361
# No trend

# Proportion of SAC to DW index
MannKendall(SAPM_Index_Final$Sacramento_Index/SAPM_Index_Final$Watershed_Index)
# tau = 0.16, 2-sided pvalue =0.27234

# Delta
acf(SAPM_Index_Final$Delta_Index) # no autocorrelation

MannKendall(SAPM_Index_Final$Delta_Index)
# tau = -0.147, 2-sided pvalue =0.31525
# No trend

# Proportion of Delta to DW index
MannKendall(SAPM_Index_Final$Delta_Index/SAPM_Index_Final$Watershed_Index)
# tau = -0.167, 2-sided pvalue =0.25246

# SPLT --------------------------------------------------------------------

head(SPLT_Index_Final)

# Delta Wide

acf(SPLT_Index_Final$Watershed_Index) # no autocorrelation

MannKendall(SPLT_Index_Final$Watershed_Index)
# tau = 0.1, 2-sided pvalue = 0.49822
# No trend

# SJ
acf(SPLT_Index_Final$San_Joaquin_Index) # no autocorrelation

MannKendall(SPLT_Index_Final$San_Joaquin_Index)
# tau = -0.0267, 2-sided pvalue =0.87014
# No trend

# Proportion of SJ to DW index
MannKendall(SPLT_Index_Final$San_Joaquin_Index/SPLT_Index_Final$Watershed_Index)
# tau = -0.107, 2-sided pvalue =0.46906

# Sac
acf(SPLT_Index_Final$Sacramento_Index) # no autocorrelation

MannKendall(SPLT_Index_Final$Sacramento_Index)
# tau = 0.413, 2-sided pvalue =0.0040703
# Positive trend

pettitt.test(SPLT_Index_Final$Sacramento_Index)
# U* = 92, p-value = 0.08786; no change point

# Proportion of SAC to DW index
MannKendall(SPLT_Index_Final$Sacramento_Index/SPLT_Index_Final$Watershed_Index)
# tau = 0.207, 2-sided pvalue =0.15426

# Delta
acf(SPLT_Index_Final$Delta_Index) # no autocorrelation

MannKendall(SPLT_Index_Final$Delta_Index)
# tau = 0.06, 2-sided pvalue =0.69134
# No trend

# Proportion of DELTA to DW index
MannKendall(SPLT_Index_Final$Delta_Index/SPLT_Index_Final$Watershed_Index)
# tau = -0.0533, 2-sided pvalue =0.7261

# SASU --------------------------------------------------------------------

head(SASU_Index_Final)

# Delta Wide

acf(SASU_Index_Final$Watershed_Index) # no autocorrelation

MannKendall(SASU_Index_Final$Watershed_Index)
# tau = 0.5, 2-sided pvalue =0.00050163
# Positive trend

pettitt.test(SASU_Index_Final$Watershed_Index)
# U* = 142, p-value = 0.001169; Sig
# Change point at time 12

# SJ
acf(SASU_Index_Final$San_Joaquin_Index) # no autocorrelation

MannKendall(SASU_Index_Final$San_Joaquin_Index)
# tau = 0.08, 2-sided pvalue =0.59115
# No trend

# Proportion of SJ to DW index
MannKendall(SASU_Index_Final$San_Joaquin_Index/SASU_Index_Final$Watershed_Index)
# tau = -0.24, 2-sided pvalue =0.097277

# Sac
acf(SASU_Index_Final$Sacramento_Index) # no autocorrelation

MannKendall(SASU_Index_Final$Sacramento_Index)
# tau = 0.447, 2-sided pvalue =0.0018951
# Positive trend

pettitt.test(SASU_Index_Final$Sacramento_Index)
# U* = 136, p-value = 0.002163
# Change point at time 12

# Proportion of SAC to DW index
MannKendall(SASU_Index_Final$Sacramento_Index/SASU_Index_Final$Watershed_Index)
# tau = 0.187, 2-sided pvalue =0.19896

# Delta
acf(SASU_Index_Final$Delta_Index) # no autocorrelation

MannKendall(SASU_Index_Final$Delta_Index)
# tau = 0.447, 2-sided pvalue =0.0018951
# Positive trend

pettitt.test(SASU_Index_Final$Delta_Index)
# U* = 118, p-value = 0.0117
# Change point at time 10

# Proportion of DELTA to DW index
MannKendall(SASU_Index_Final$Delta_Index/SASU_Index_Final$Watershed_Index)
# tau = 0.0467, 2-sided pvalue =0.76142

# iii. Regression Analyses -----------------------------------------------------

# ***** PCA ***** -------------------------------------------------------------

# Because IEP gets critisim for only using flow and water temp is correlated with flow, I am going to use PCA to get uncor. covariates. I'm not worried about overfitting so I will not be reducing the number of variables.

# Will be rotating PCs to get higher loadings

# SAPM --------------------------------------------------------------------

# Final index data
head(SAPM_Index_Final)

# Final scaled covariate data
head(SAPM_Covariates_Scaled)
cor(SAPM_Covariates_Scaled)

SAPM_Model_Data <- 
  as.data.frame(cbind(SAPM_Index_Final[,5:8], 
                      SAPM_Covariates_Scaled))
head(SAPM_Model_Data)

# a. PCA ------------------------------------------------------------------

head(SAPM_Model_Data)

# All covariate data has already been scaled

# Converting DF to Matrix for PCA

# Sac Data

SAPM_Model_Data_SAC <- 
  SAPM_Model_Data %>% 
  select(Mean_SAC_Discharge, SAC_Flow_Timing, SAC_Mean_WT)
head(SAPM_Model_Data_SAC)
cor(SAPM_Model_Data_SAC)

# Creating matrix for PCA
SAPM_Model_Data_SAC_Mat <- 
  SAPM_Model_Data_SAC %>% 
  as.matrix()
head(SAPM_Model_Data_SAC_Mat)

SAPM_SAC_PCA_No_Rot <- 
  psych::principal(SAPM_Model_Data_SAC_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SAPM_SAC_PCA_No_Rot

SAPM_SAC_PCA <- 
  psych::principal(SAPM_Model_Data_SAC_Mat, 
                   nfactors = 3, 
                   rotate = "varimax")
SAPM_SAC_PCA

# Loadings
SAPM_SAC_PCA_Loadings_1 <- 
  SAPM_SAC_PCA$loadings

SAPM_SAC_PCA_Loadings <- 
  round(SAPM_SAC_PCA_Loadings_1[,], 2) 
SAPM_SAC_PCA_Loadings

# Loadings no rotation
SAPM_SAC_PCA_Loadings_No_Rot_1 <- 
  SAPM_SAC_PCA_No_Rot$loadings

SAPM_SAC_PCA_Loadings_No_Rot <- 
  round(SAPM_SAC_PCA_Loadings_No_Rot_1[,], 2) 
SAPM_SAC_PCA_Loadings_No_Rot

# Scores
SAPM_SAC_PC_Scores <- 
  as.data.frame(SAPM_SAC_PCA$scores)

colnames(SAPM_SAC_PC_Scores) <- 
  c("SAC_RC3", 
    "SAC_RC2", 
    "SAC_RC1")
SAPM_SAC_PC_Scores

# SJ -> no score because we are not fitting a model for this region

# Delta
#  They don't spawn in the Delta so using sac + SJ flows
# Mean flows calc during months of peak spawning

SAPM_Model_Data_DELTA <- 
  SAPM_Model_Data %>% 
  select(Mean_DELTA_Discharge, DELTA_Flow_Timing, DELTA_Mean_WT)
head(SAPM_Model_Data_DELTA)
cor(SAPM_Model_Data_DELTA)

SAPM_Model_Data_DELTA_Mat <- 
  SAPM_Model_Data_DELTA %>% 
  as.matrix()
head(SAPM_Model_Data_DELTA_Mat)

SAPM_DELTA_PCA_No_Rot <- 
  psych::principal(SAPM_Model_Data_DELTA_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SAPM_DELTA_PCA_No_Rot

SAPM_DELTA_PCA <- 
  psych::principal(SAPM_Model_Data_DELTA_Mat, 
                   nfactors = 3, 
                   rotate = "varimax")
SAPM_DELTA_PCA

# Loadings
SAPM_DELTA_PCA_Loadings_1 <- 
  SAPM_DELTA_PCA$loadings

SAPM_DELTA_PCA_Loadings <- 
  round(SAPM_DELTA_PCA_Loadings_1[,], 2)
SAPM_DELTA_PCA_Loadings

SAPM_DELTA_PCA_Loadings_No_Rot_1 <- 
  SAPM_DELTA_PCA_No_Rot$loadings

SAPM_DELTA_PCA_Loadings_No_Rot <- 
  round(SAPM_DELTA_PCA_Loadings_No_Rot_1[,], 2)
SAPM_DELTA_PCA_Loadings_No_Rot


# Getting scores
SAPM_DELTA_PC_Scores <- 
  as.data.frame(SAPM_DELTA_PCA$scores)
colnames(SAPM_DELTA_PC_Scores) <- 
  c("DELTA_RC2", 
    "DELTA_RC3", 
    "DELTA_RC1")
SAPM_DELTA_PC_Scores

# b. CDist PCA ---------------------------------------------------------------

# CDist analysis focusing on Sac River
# CDist WT from all stations on the Sac River and not just Sac Region

# CDist Data
SAPM_Model_Data_CDist_Mat <- 
  SAPM_Model_Data %>% 
  select(Mean_SAC_Discharge, 
         SAC_Flow_Timing, 
         SAC_CDist_Mean_WT) %>% 
  as.matrix()
head(SAPM_Model_Data_CDist_Mat)
cor(SAPM_Model_Data_CDist_Mat)

# PCs
SAPM_CDist_PCA <- 
  psych::principal(SAPM_Model_Data_CDist_Mat, 
                   nfactors = 3, 
                   rotate = "varimax")
SAPM_CDist_PCA

SAPM_CDist_PCA_No_Rot <- 
  psych::principal(SAPM_Model_Data_CDist_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SAPM_CDist_PCA_No_Rot

# Loadings
SAPM_CDist_PCA_Loadings_1 <- 
  SAPM_CDist_PCA$loadings

SAPM_CDist_PCA_Loadings <- 
  round(SAPM_CDist_PCA_Loadings_1[,], 2)
SAPM_CDist_PCA_Loadings


SAPM_CDist_PCA_Loadings_No_Rot_1 <- 
  SAPM_CDist_PCA_No_Rot$loadings

SAPM_CDist_PCA_Loadings_No_Rot <- 
  round(SAPM_CDist_PCA_Loadings_No_Rot_1[,], 2)
SAPM_CDist_PCA_Loadings_No_Rot

# Scores
SAPM_CDist_PC_Scores <- 
  as.data.frame(SAPM_CDist_PCA$scores)
colnames(SAPM_CDist_PC_Scores) <- 
  c("CDist_RC3", 
    "CDist_RC2", 
    "CDist_RC1")
SAPM_CDist_PC_Scores

# c. Final PCA data -------------------------------------------------

SAPM_Model_Data_Final <- 
  SAPM_Model_Data %>% 
  select(Delta_Index, 
         Sacramento_Index, 
         Watershed_Index) %>% 
  bind_cols(SAPM_CDist[,2],
            SAPM_SAC_PC_Scores, 
            SAPM_DELTA_PC_Scores,
            SAPM_CDist_PC_Scores)
head(SAPM_Model_Data_Final)


# SPLT --------------------------------------------------------------------

# Final index data
head(SPLT_Index_Final)

# Final scaled covariate data
head(SPLT_Covariates_Scaled)
cor(SPLT_Covariates_Scaled)

SPLT_Model_Data <- 
  cbind(SPLT_Index_Final[,5:8], 
        SPLT_Covariates_Scaled)
head(SPLT_Model_Data)

# a. PCA ------------------------------------------------------------------

# All covariate data has already been scaled

# Sac Data


SPLT_Model_Data_SAC <- 
  SPLT_Model_Data %>% 
  select(Mean_SAC_Discharge, SAC_Flow_Timing, SAC_Mean_WT)
head(SPLT_Model_Data_SAC)
cor(SPLT_Model_Data_SAC)

# Creating matrix for PCA
SPLT_Model_Data_SAC_Mat <- 
  SPLT_Model_Data_SAC %>% 
  as.matrix()
head(SPLT_Model_Data_SAC_Mat)

# PCs
SPLT_SAC_PCA <- 
  psych::principal(SPLT_Model_Data_SAC_Mat, 
                   nfactors = 3, 
                   rotate = "varimax")
SPLT_SAC_PCA

SPLT_SAC_PCA_No_Rot <- 
  psych::principal(SPLT_Model_Data_SAC_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SPLT_SAC_PCA_No_Rot

# Loadings
SPLT_SAC_PCA_Loadings_1 <- 
  SPLT_SAC_PCA$loadings

SPLT_SAC_PCA_Loadings <- 
  round(SPLT_SAC_PCA_Loadings_1[,], 2)
SPLT_SAC_PCA_Loadings

SPLT_SAC_PCA_Loadings_No_Rot_1 <- 
  SPLT_SAC_PCA_No_Rot$loadings

SPLT_SAC_PCA_Loadings_No_Rot <- 
  round(SPLT_SAC_PCA_Loadings_No_Rot_1[,], 2)
SPLT_SAC_PCA_Loadings_No_Rot

# Scores
SPLT_SAC_PC_Scores <- 
  as.data.frame(SPLT_SAC_PCA$scores)
colnames(SPLT_SAC_PC_Scores) <- 
  c("SAC_RC3", 
    "SAC_RC2", 
    "SAC_RC1")
SPLT_SAC_PC_Scores

# SJ Data

SPLT_Model_Data_SJ <- 
  SPLT_Model_Data %>% 
  select(Mean_SJ_Discharge, SJ_Flow_Timing, SJ_Mean_WT)
head(SPLT_Model_Data_SJ)
cor(SPLT_Model_Data_SJ)

# Creating matrix for PCA
SPLT_Model_Data_SJ_Mat <- 
  SPLT_Model_Data_SJ %>% 
  as.matrix()
head(SPLT_Model_Data_SJ_Mat)

# PCs
SPLT_SJ_PCA <- 
  psych::principal(SPLT_Model_Data_SJ_Mat, 
                   nfactors = 3, 
                   rotate = "varimax")

SPLT_SJ_PCA_No_Rot <- 
  psych::principal(SPLT_Model_Data_SJ_Mat, 
                   nfactors = 3, 
                   rotate = "none")

# Loadings
SPLT_SJ_PCA_Loadings_1 <- 
  SPLT_SJ_PCA$loadings

SPLT_SJ_PCA_Loadings <- 
  round(SPLT_SJ_PCA_Loadings_1[,], 2)
SPLT_SJ_PCA_Loadings

SPLT_SJ_PCA_Loadings_No_Rot_1 <- 
  SPLT_SJ_PCA_No_Rot$loadings

SPLT_SJ_PCA_Loadings_No_Rot <- 
  round(SPLT_SJ_PCA_Loadings_No_Rot_1[,], 2)
SPLT_SJ_PCA_Loadings_No_Rot

# Scores
SPLT_SJ_PC_Scores <- 
  as.data.frame(SPLT_SJ_PCA$scores)
colnames(SPLT_SJ_PC_Scores) <- 
  c("SJ_RC3", 
    "SJ_RC2", 
    "SJ_RC1")
SPLT_SJ_PC_Scores

# Delta

SPLT_Model_Data_DELTA_Mat <- 
  SPLT_Model_Data %>% 
  select(Mean_DELTA_Discharge, 
         DELTA_Flow_Timing, 
         DELTA_Mean_WT) %>% 
  as.matrix()
head(SPLT_Model_Data_DELTA_Mat)
cor(SPLT_Model_Data_DELTA_Mat)

SPLT_DELTA_PCA <- 
  psych::principal(SPLT_Model_Data_DELTA_Mat, 
                   nfactors = 3, 
                   rotate = "varimax")
SPLT_DELTA_PCA


SPLT_DELTA_PCA_No_Rot <- 
  psych::principal(SPLT_Model_Data_DELTA_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SPLT_DELTA_PCA_No_Rot

# Loadings
SPLT_DELTA_PCA_Loadings_1 <- 
  SPLT_DELTA_PCA$loadings

SPLT_DELTA_PCA_Loadings <- 
  round(SPLT_DELTA_PCA_Loadings_1[,], 2)
SPLT_DELTA_PCA_Loadings

SPLT_DELTA_PCA_Loadings_No_Rot_1 <- 
  SPLT_DELTA_PCA_No_Rot$loadings

SPLT_DELTA_PCA_Loadings_No_Rot <- 
  round(SPLT_DELTA_PCA_Loadings_No_Rot_1[,], 2)
SPLT_DELTA_PCA_Loadings_No_Rot

# Getting scores
SPLT_DELTA_PC_Scores <- 
  as.data.frame(SPLT_DELTA_PCA$scores)
colnames(SPLT_DELTA_PC_Scores) <- 
  c("DELTA_RC2", 
    "DELTA_RC3", 
    "DELTA_RC1")
SPLT_DELTA_PC_Scores


# b. CDist PCA ---------------------------------------------------------------

# CDist analysis focusing on Sac River
# CDist WT from all stations on the Sac River and not just Sac Region

# SAC CDist Data
SPLT_Model_Data_SAC_CDist_Mat <- 
  SPLT_Model_Data %>% 
  select(Mean_SAC_Discharge, 
         SAC_Flow_Timing, 
         SAC_CDist_Mean_WT) %>% 
  as.matrix()
head(SPLT_Model_Data_SAC_CDist_Mat)
cor(SPLT_Model_Data_SAC_CDist_Mat)

# PCs
SPLT_SAC_CDist_PCA <- 
  psych::principal(SPLT_Model_Data_SAC_CDist_Mat, 
                   nfactors = 3, 
                   rotate = "varimax")
SPLT_SAC_CDist_PCA


SPLT_SAC_CDist_PCA_No_Rot <- 
  psych::principal(SPLT_Model_Data_SAC_CDist_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SPLT_SAC_CDist_PCA_No_Rot

# Loadings
SPLT_SAC_CDist_PCA_Loadings_1 <- 
  SPLT_SAC_CDist_PCA$loadings

SPLT_SAC_CDist_PCA_Loadings <- 
  round(SPLT_SAC_CDist_PCA_Loadings_1[,], 2)
SPLT_SAC_CDist_PCA_Loadings


SPLT_SAC_CDist_PCA_Loadings_No_Rot_1 <- 
  SPLT_SAC_CDist_PCA_No_Rot$loadings

SPLT_SAC_CDist_PCA_Loadings_No_Rot <- 
  round(SPLT_SAC_CDist_PCA_Loadings_No_Rot_1[,], 2)
SPLT_SAC_CDist_PCA_Loadings_No_Rot

# Scores
SPLT_SAC_CDist_PC_Scores <- 
  as.data.frame(SPLT_SAC_CDist_PCA$scores)
colnames(SPLT_SAC_CDist_PC_Scores) <- 
  c("SAC_CDist_RC2", 
    "SAC_CDist_RC3", 
    "SAC_CDist_RC1")
SPLT_SAC_CDist_PC_Scores


# SJ CDist Data
SPLT_Model_Data_SJ_CDist_Mat <- 
  SPLT_Model_Data %>% 
  select(Mean_SJ_Discharge, 
         SJ_Flow_Timing, 
         SJ_CDist_Mean_WT) %>% 
  as.matrix()
head(SPLT_Model_Data_SJ_CDist_Mat)
cor(SPLT_Model_Data_SJ_CDist_Mat)

# PCs
SPLT_SJ_CDist_PCA <- 
  psych::principal(SPLT_Model_Data_SJ_CDist_Mat, 
                   nfactors = 3, 
                   rotate = "varimax")
SPLT_SJ_CDist_PCA


SPLT_SJ_CDist_PCA_No_Rot <- 
  psych::principal(SPLT_Model_Data_SJ_CDist_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SPLT_SJ_CDist_PCA_No_Rot

# Loadings
SPLT_SJ_CDist_PCA_Loadings_1 <- 
  SPLT_SJ_CDist_PCA$loadings

SPLT_SJ_CDist_PCA_Loadings <- 
  round(SPLT_SJ_CDist_PCA_Loadings_1[,], 2)
SPLT_SJ_CDist_PCA_Loadings


SPLT_SJ_CDist_PCA_Loadings_No_Rot_1 <- 
  SPLT_SJ_CDist_PCA_No_Rot$loadings

SPLT_SJ_CDist_PCA_Loadings_No_Rot <- 
  round(SPLT_SJ_CDist_PCA_Loadings_No_Rot_1[,], 2)
SPLT_SJ_CDist_PCA_Loadings_No_Rot

# Scores
SPLT_SJ_CDist_PC_Scores <- 
  as.data.frame(SPLT_SJ_CDist_PCA$scores)
colnames(SPLT_SJ_CDist_PC_Scores) <- 
  c("SJ_CDist_RC2", 
    "SJ_CDist_RC3", 
    "SJ_CDist_RC1")
SPLT_SJ_CDist_PC_Scores

# c. Final PCA data -------------------------------------------------

SPLT_Model_Data_Final <- 
  SPLT_Model_Data %>% 
  select(Delta_Index, 
         Sacramento_Index, 
         San_Joaquin_Index,
         Watershed_Index) %>% 
  bind_cols(SPLT_CDist[,2:3],
            SPLT_SAC_PC_Scores, 
            SPLT_SJ_PC_Scores,
            SPLT_DELTA_PC_Scores,
            SPLT_SAC_CDist_PC_Scores,
            SPLT_SJ_CDist_PC_Scores)
head(SPLT_Model_Data_Final)

# SASU --------------------------------------------------------------------

# Final index data
head(SASU_Index_Final)

# Final scaled covariate data
head(SASU_Covariates_Scaled)
cor(SASU_Covariates_Scaled)

SASU_Model_Data <- 
  cbind(SASU_Index_Final[,5:8], 
        SASU_Covariates_Scaled)
head(SASU_Model_Data)

# a. PCA ------------------------------------------------------------------

# Converting DF to Matrix for PCA

# Sac Data


SASU_Model_Data_SAC <- 
  SASU_Model_Data %>% 
  select(Mean_SAC_Discharge, SAC_Flow_Timing, SAC_Mean_WT)
head(SASU_Model_Data_SAC)
cor(SASU_Model_Data_SAC)

# Creating matrix for PCA
SASU_Model_Data_SAC_Mat <- 
  SASU_Model_Data_SAC %>% 
  as.matrix()
head(SASU_Model_Data_SAC_Mat)

# PCs
SASU_SAC_PCA <- 
  psych::principal(SASU_Model_Data_SAC_Mat, 
                   nfactors = 3, 
                   rotate = "varimax")
SASU_SAC_PCA

SASU_SAC_PCA_No_Rot <- 
  psych::principal(SASU_Model_Data_SAC_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SASU_SAC_PCA_No_Rot

# Loadings
SASU_SAC_PCA_Loadings_1 <- 
  SASU_SAC_PCA$loadings

SASU_SAC_PCA_Loadings <- 
  round(SASU_SAC_PCA_Loadings_1[,], 2)
SASU_SAC_PCA_Loadings

SASU_SAC_PCA_Loadings_No_Rot_1 <- 
  SASU_SAC_PCA_No_Rot$loadings

SASU_SAC_PCA_Loadings_No_Rot <- 
  round(SASU_SAC_PCA_Loadings_No_Rot_1[,], 2)
SASU_SAC_PCA_Loadings_No_Rot

# Scores
SASU_SAC_PC_Scores <- 
  as.data.frame(SASU_SAC_PCA$scores)
colnames(SASU_SAC_PC_Scores) <- 
  c("SAC_RC3", 
    "SAC_RC2", 
    "SAC_RC1")
SASU_SAC_PC_Scores

# SJ Data

SASU_Model_Data_SJ <- 
  SASU_Model_Data %>% 
  select(Mean_SJ_Discharge, SJ_Flow_Timing, SJ_Mean_WT)
head(SASU_Model_Data_SJ)
cor(SASU_Model_Data_SJ)

# Creating matrix for PCA
SASU_Model_Data_SJ_Mat <- 
  SASU_Model_Data_SJ %>% 
  as.matrix()
head(SASU_Model_Data_SJ_Mat)

# PCs
SASU_SJ_PCA <- 
  psych::principal(SASU_Model_Data_SJ_Mat, 
                   nfactors = 3, 
                   rotate = "varimax")

SASU_SJ_PCA_No_Rot <- 
  psych::principal(SASU_Model_Data_SJ_Mat, 
                   nfactors = 3, 
                   rotate = "none")

# Loadings
SASU_SJ_PCA_Loadings_1 <- 
  SASU_SJ_PCA$loadings

SASU_SJ_PCA_Loadings <- 
  round(SASU_SJ_PCA_Loadings_1[,], 2)
SASU_SJ_PCA_Loadings

SASU_SJ_PCA_Loadings_No_Rot_1 <- 
  SASU_SJ_PCA_No_Rot$loadings

SASU_SJ_PCA_Loadings_No_Rot <- 
  round(SASU_SJ_PCA_Loadings_No_Rot_1[,], 2)
SASU_SJ_PCA_Loadings_No_Rot

# Scores
SASU_SJ_PC_Scores <- 
  as.data.frame(SASU_SJ_PCA$scores)
colnames(SASU_SJ_PC_Scores) <- 
  c("SJ_RC3", 
    "SJ_RC2", 
    "SJ_RC1")
SASU_SJ_PC_Scores

# Delta

SASU_Model_Data_DELTA_Mat <- 
  SASU_Model_Data %>% 
  select(Mean_DELTA_Discharge, 
         DELTA_Flow_Timing, 
         DELTA_Mean_WT) %>% 
  as.matrix()
head(SASU_Model_Data_DELTA_Mat)
cor(SASU_Model_Data_DELTA_Mat)

SASU_DELTA_PCA <- 
  psych::principal(SASU_Model_Data_DELTA_Mat, 
                   nfactors = 3, 
                   rotate = "varimax")
SASU_DELTA_PCA


SASU_DELTA_PCA_No_Rot <- 
  psych::principal(SASU_Model_Data_DELTA_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SASU_DELTA_PCA_No_Rot

# Loadings
SASU_DELTA_PCA_Loadings_1 <- 
  SASU_DELTA_PCA$loadings

SASU_DELTA_PCA_Loadings <- 
  round(SASU_DELTA_PCA_Loadings_1[,], 2)
SASU_DELTA_PCA_Loadings

SASU_DELTA_PCA_Loadings_No_Rot_1 <- 
  SASU_DELTA_PCA_No_Rot$loadings

SASU_DELTA_PCA_Loadings_No_Rot <- 
  round(SASU_DELTA_PCA_Loadings_No_Rot_1[,], 2)
SASU_DELTA_PCA_Loadings_No_Rot

# Getting scores
SASU_DELTA_PC_Scores <- 
  as.data.frame(SASU_DELTA_PCA$scores)
colnames(SASU_DELTA_PC_Scores) <- 
  c("DELTA_RC2", 
    "DELTA_RC3", 
    "DELTA_RC1")
SASU_DELTA_PC_Scores


# b. CDist PCA ---------------------------------------------------------------

# CDist analysis focusing on Sac River
# CDist WT from all stations on the Sac River and not just Sac Region

# SAC CDist Data
SASU_Model_Data_SAC_CDist_Mat <- 
  SASU_Model_Data %>% 
  select(Mean_SAC_Discharge, 
         SAC_Flow_Timing, 
         SAC_CDist_Mean_WT) %>% 
  as.matrix()
head(SASU_Model_Data_SAC_CDist_Mat)
cor(SASU_Model_Data_SAC_CDist_Mat)

# PCs
SASU_SAC_CDist_PCA <- 
  psych::principal(SASU_Model_Data_SAC_CDist_Mat, 
                   nfactors = 3, 
                   rotate = "varimax")
SASU_SAC_CDist_PCA


SASU_SAC_CDist_PCA_No_Rot <- 
  psych::principal(SASU_Model_Data_SAC_CDist_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SASU_SAC_CDist_PCA_No_Rot

# Loadings
SASU_SAC_CDist_PCA_Loadings_1 <- 
  SASU_SAC_CDist_PCA$loadings

SASU_SAC_CDist_PCA_Loadings <- 
  round(SASU_SAC_CDist_PCA_Loadings_1[,], 2)
SASU_SAC_CDist_PCA_Loadings


SASU_SAC_CDist_PCA_Loadings_No_Rot_1 <- 
  SASU_SAC_CDist_PCA_No_Rot$loadings

SASU_SAC_CDist_PCA_Loadings_No_Rot <- 
  round(SASU_SAC_CDist_PCA_Loadings_No_Rot_1[,], 2)
SASU_SAC_CDist_PCA_Loadings_No_Rot

# Scores
SASU_SAC_CDist_PC_Scores <- 
  as.data.frame(SASU_SAC_CDist_PCA$scores)
colnames(SASU_SAC_CDist_PC_Scores) <- 
  c("SAC_CDist_RC2", 
    "SAC_CDist_RC3", 
    "SAC_CDist_RC1")
SASU_SAC_CDist_PC_Scores


# SJ CDist Data
SASU_Model_Data_SJ_CDist_Mat <- 
  SASU_Model_Data %>% 
  select(Mean_SJ_Discharge, 
         SJ_Flow_Timing, 
         SJ_CDist_Mean_WT) %>% 
  as.matrix()
head(SASU_Model_Data_SJ_CDist_Mat)
cor(SASU_Model_Data_SJ_CDist_Mat)

# PCs
SASU_SJ_CDist_PCA <- 
  psych::principal(SASU_Model_Data_SJ_CDist_Mat, 
                   nfactors = 3, 
                   rotate = "varimax")
SASU_SJ_CDist_PCA


SASU_SJ_CDist_PCA_No_Rot <- 
  psych::principal(SASU_Model_Data_SJ_CDist_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SASU_SJ_CDist_PCA_No_Rot

# Loadings
SASU_SJ_CDist_PCA_Loadings_1 <- 
  SASU_SJ_CDist_PCA$loadings

SASU_SJ_CDist_PCA_Loadings <- 
  round(SASU_SJ_CDist_PCA_Loadings_1[,], 2)
SASU_SJ_CDist_PCA_Loadings


SASU_SJ_CDist_PCA_Loadings_No_Rot_1 <- 
  SASU_SJ_CDist_PCA_No_Rot$loadings

SASU_SJ_CDist_PCA_Loadings_No_Rot <- 
  round(SASU_SJ_CDist_PCA_Loadings_No_Rot_1[,], 2)
SASU_SJ_CDist_PCA_Loadings_No_Rot

# Scores
SASU_SJ_CDist_PC_Scores <- 
  as.data.frame(SASU_SJ_CDist_PCA$scores)
colnames(SASU_SJ_CDist_PC_Scores) <- 
  c("SJ_CDist_RC2", 
    "SJ_CDist_RC3", 
    "SJ_CDist_RC1")
SASU_SJ_CDist_PC_Scores

# c. Final PCA data -------------------------------------------------

SASU_Model_Data_Final <- 
  SASU_Model_Data %>% 
  select(Delta_Index, 
         Sacramento_Index, 
         San_Joaquin_Index,
         Watershed_Index) %>% 
  bind_cols(SASU_CDist[,2:3],
            SASU_SAC_PC_Scores, 
            SASU_SJ_PC_Scores,
            SASU_DELTA_PC_Scores,
            SASU_SAC_CDist_PC_Scores,
            SASU_SJ_CDist_PC_Scores)
head(SASU_Model_Data_Final)


# iv. Abundance Models --------------------------------------------------------

# All subset selection that examine the influence of Water temp, mean flow during spawning, and timing of flow on age-0 abundance

# SAPM --------------------------------------------------------------------

head(SAPM_Model_Data_Final)

# 1) Delta-Wide Models ----------------------------------------------------

pairs(SAPM_Model_Data_Final[,c(3,5:7)], pch = 16)
# Best relationship with RC2...but slightly non-linear

# Delta-Wide Index
SAPM_DW_Global_lm <- 
  lm(log(Watershed_Index) ~ SAC_RC1 + SAC_RC2 + SAC_RC3, 
     data = SAPM_Model_Data_Final, na.action = "na.fail")
summary(SAPM_DW_Global_lm)
# RC2 (timing of flow) is important

# Model assumptions
plot(SAPM_DW_Global_lm) # Assumptions look fine
acf(SAPM_SAC_Global_lm$residuals) # No Autocorreltion
vif(SAPM_SAC_Global_lm) # PCs are unclorrelated

## All subset selection
SAPM_DW_Global_Dredge <- 
  dredge(SAPM_DW_Global_lm,
         extra = list(
           "R^2", "Dredge_Function" = function(x) {
             s <- summary(x)
             c(adjRsq = s$adj.r.squared)
           }))
SAPM_DW_Global_Dredge
# Top models include Flow timing (RC2)

importance(SAPM_DW_Global_Dredge)

### Getting model output and weights
SAPM_DW_Dredge_Output <- 
  as.data.frame(cbind(round(SAPM_DW_Global_Dredge$`(Intercept)`, 2),
                      round(SAPM_DW_Global_Dredge$SAC_RC1, 2),
                      round(SAPM_DW_Global_Dredge$SAC_RC2, 2), 
                      round(SAPM_DW_Global_Dredge$SAC_RC3, 2),
                      SAPM_DW_Global_Dredge$df,
                      round(SAPM_DW_Global_Dredge$`R^2`, 2),
                      round(SAPM_DW_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SAPM_DW_Global_Dredge$logLik, 2), 
                      round(SAPM_DW_Global_Dredge$AICc, 2),
                      round(SAPM_DW_Global_Dredge$delta, 2), 
                      SAPM_DW_Global_Dredge$weight)) %>% 
  mutate(Species = "SAPM", Region = "DW")
colnames(SAPM_DW_Dredge_Output) = c("Intercept", "SAC_RC1", "SAC_RC2", "SAC_RC3", 
                                    "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                    "Delta", "Weights", "Species", "Region")
SAPM_DW_Dredge_Output

# Getting weights for output
SAPM_DW_RC2_Weights <- 
  SAPM_DW_Dredge_Output %>% 
  filter(!(is.na(SAC_RC2))) %>% # Selecting rows where SAC_RC2 is not NA
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", 
         Region = "DW", 
         Variable = "SAC_RC2")

SAPM_DW_RC3_Weights <- 
  SAPM_DW_Dredge_Output %>% 
  filter(!(is.na(SAC_RC3))) %>% # Selecting rows where SAC_RC3 is not NA
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", 
         Region = "DW", 
         Variable = "SAC_RC3")

SAPM_DW_Weights <- 
  SAPM_DW_Dredge_Output %>% 
  filter(!(is.na(SAC_RC1))) %>% # Selecting rows where SAC_RC1 is not NA
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", 
         Region = "DW", 
         Variable = "SAC_RC1") %>% 
  bind_rows(SAPM_DW_RC2_Weights, 
            SAPM_DW_RC3_Weights) # Binding summed weights of SAC_RC2, and SAC_RC3
SAPM_DW_Weights

# Checking weight output
importance(SAPM_DW_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set
SAPM_DW_MA <- model.avg(SAPM_DW_Global_Dredge, 
                       cumsum(weight) <= .95, 
                       rank = "AIC")
SAPM_DW_MA_Sum = summary(SAPM_DW_MA)
SAPM_DW_MA_Full_Out = SAPM_DW_MA_Sum$coefmat.full
SAPM_DW_MA_Full_Out

# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SAPM_DW_Dredge_Output_Final <- 
  SAPM_DW_Dredge_Output %>% 
  replace_na(list(SAC_RC1 = "", SAC_RC2 = "", SAC_RC3 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SAPM_DW_Dredge_Output_Final

write.csv(SAPM_DW_Dredge_Output_Final, 
          "Tables/SAPM_DW_Dredge_Abun_Output.csv", 
          row.names = FALSE)


# 2) SAC Models ----------------------------------------------------

pairs(SAPM_Model_Data_Final[,c(2,5:7)], pch = 16)
# Best relationship with RC2...but slightly non-linear

# Global model for all subset selection
SAPM_SAC_Global_lm <- 
  lm(log(Sac_River_Index) ~ SAC_RC3 + SAC_RC2 + SAC_RC1, 
     data = SAPM_Model_Data_Final, na.action = "na.fail")
summary(SAPM_SAC_Global_lm)

# Model assumptions
plot(SAPM_SAC_Global_lm) # Assumptions look fine
acf(SAPM_SAC_Global_lm$residuals) # No Autocorreltion
vif(SAPM_SAC_Global_lm) # PCs are uncor.

## All subset selection
SAPM_SAC_Global_Dredge <- 
  dredge(SAPM_SAC_Global_lm,
         extra = list(
           "R^2", "Dredge_Function" = function(x) {
             s <- summary(x)
             c(adjRsq = s$adj.r.squared)
           }))
SAPM_SAC_Global_Dredge
# Top models include Flow timing (RC2)

importance(SAPM_SAC_Global_Dredge)
# Flow timing is most important variable; weights = 0.85

### Getting model output and weights
SAPM_SAC_Dredge_Output <- 
  as.data.frame(cbind(round(SAPM_SAC_Global_Dredge$`(Intercept)`, 2), 
                      round(SAPM_SAC_Global_Dredge$SAC_RC1, 2),
                      round(SAPM_SAC_Global_Dredge$SAC_RC2, 2),
                      round(SAPM_SAC_Global_Dredge$SAC_RC3, 2), 
                      SAPM_SAC_Global_Dredge$df,
                      round(SAPM_SAC_Global_Dredge$`R^2`, 2),
                      round(SAPM_SAC_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SAPM_SAC_Global_Dredge$logLik, 2), 
                      round(SAPM_SAC_Global_Dredge$AICc, 2),
                      round(SAPM_SAC_Global_Dredge$delta, 2), 
                      SAPM_SAC_Global_Dredge$weight)) %>% 
  mutate(Species = "SAPM", Region = "SAC")
colnames(SAPM_SAC_Dredge_Output) = c("Intercept", "SAC_RC1", "SAC_RC2", "SAC_RC3", 
                                    "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                    "Delta", "Weights", "Species", "Region")
SAPM_SAC_Dredge_Output

# Model weights output
SAPM_SAC_RC2_Weights <- 
  SAPM_SAC_Dredge_Output %>% 
  filter(!(is.na(SAC_RC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", Region = "SAC", Variable = "SAC_RC2")

SAPM_SAC_RC3_Weights <- 
  SAPM_SAC_Dredge_Output %>% 
  filter(!(is.na(SAC_RC3))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", Region = "SAC", Variable = "SAC_RC3")

SAPM_SAC_Weights <- 
  SAPM_SAC_Dredge_Output %>% 
  filter(!(is.na(SAC_RC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", Region = "SAC", Variable = "SAC_RC1") %>% 
  bind_rows(SAPM_SAC_RC2_Weights, SAPM_SAC_RC3_Weights)
SAPM_SAC_Weights

# Checking weights output
importance(SAPM_SAC_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set
SAPM_SAC_MA <- model.avg(SAPM_SAC_Global_Dredge, 
                        cumsum(weight) <= .95, 
                        rank = "AIC")
SAPM_SAC_MA_Sum = summary(SAPM_SAC_MA)
SAPM_SAC_MA_Full_Out = SAPM_SAC_MA_Sum$coefmat.full
SAPM_SAC_MA_Full_Out

# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SAPM_SAC_Dredge_Output_Final <- 
  SAPM_SAC_Dredge_Output %>% 
  replace_na(list(SAC_RC1 = "", SAC_RC2 = "", SAC_RC3 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights))
SAPM_SAC_Dredge_Output_Final

write.csv(SAPM_SAC_Dredge_Output_Final, 
          "Tables/SAPM_SAC_Dredge_Abun_Output.csv", 
          row.names = FALSE)

# 3) Delta Models ----------------------------------------------------

pairs(SAPM_Model_Data_Final[,c(1,8:10)], pch = 16)
# Best relationship with RC2...but there's an outlier

# Global model for all subset selection
SAPM_Delta_Global_lm <- 
  lm(log(Delta_Index) ~ DELTA_RC3 + DELTA_RC2 + DELTA_RC1, 
     data = SAPM_Model_Data_Final, na.action = "na.fail")
summary(SAPM_Delta_Global_lm)
# RC3 and RC2 important and R2adj = 0.34
# Seems like there is an outlier though
# Stronger relationship with Sac variables R2adj = 0.49

# Model assumptions
plot(SAPM_Delta_Global_lm) # Assumptions look fine
acf(SAPM_Delta_Global_lm$residuals) # No Autocorreltion
vif(SAPM_Delta_Global_lm) # All < 2

## All subset selection
SAPM_DELTA_Global_Dredge <- 
  dredge(SAPM_Delta_Global_lm,
         extra = list(
           "R^2", "Dredge_Function" = function(x) {
             s <- summary(x)
             c(adjRsq = s$adj.r.squared)
           }))
SAPM_DELTA_Global_Dredge
# Top models include Water temp (RC3) and Flow timing (RC2)

importance(SAPM_DELTA_Global_Dredge)

### Getting model output and weights
SAPM_DELTA_Dredge_Output <- 
  as.data.frame(cbind(round(SAPM_DELTA_Global_Dredge$`(Intercept)`, 2),
                      round(SAPM_DELTA_Global_Dredge$DELTA_RC1, 2), 
                      round(SAPM_DELTA_Global_Dredge$DELTA_RC2, 2),
                      round(SAPM_DELTA_Global_Dredge$DELTA_RC3, 2),
                      SAPM_DELTA_Global_Dredge$df,
                      round(SAPM_DELTA_Global_Dredge$`R^2`, 2),
                      round(SAPM_DELTA_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SAPM_DELTA_Global_Dredge$logLik, 2), 
                      round(SAPM_DELTA_Global_Dredge$AICc, 2),
                      round(SAPM_DELTA_Global_Dredge$delta, 2), 
                      SAPM_DELTA_Global_Dredge$weight)) %>% 
  mutate(Species = "SAPM", Region = "DELTA")
colnames(SAPM_DELTA_Dredge_Output) = c("Intercept", "DELTA_RC1", "DELTA_RC2", 
                                       "DELTA_RC3", 
                                    "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                    "Delta", "Weights", "Species", "Region")
SAPM_DELTA_Dredge_Output

# Getting weights for output
SAPM_DELTA_RC2_Weights <- 
  SAPM_DELTA_Dredge_Output %>% 
  filter(!(is.na(DELTA_RC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", Region = "DELTA", Variable = "DELTA_RC2")

SAPM_DELTA_RC3_Weights <- 
  SAPM_DELTA_Dredge_Output %>% 
  filter(!(is.na(DELTA_RC3))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", Region = "DELTA", Variable = "DELTA_RC3")

SAPM_DELTA_Weights <- 
  SAPM_DELTA_Dredge_Output %>% 
  filter(!(is.na(DELTA_RC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", Region = "DELTA", Variable = "DELTA_RC1") %>% 
  bind_rows(SAPM_DELTA_RC2_Weights, SAPM_DELTA_RC3_Weights)
SAPM_DELTA_Weights

# Checking weights
importance(SAPM_DELTA_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set
SAPM_DELTA_MA <- model.avg(SAPM_DELTA_Global_Dredge, 
                          cumsum(weight) <= .95, 
                          rank = "AIC")
SAPM_DELTA_MA_Sum = summary(SAPM_DELTA_MA)
SAPM_DELTA_MA_Full_Out = SAPM_DELTA_MA_Sum$coefmat.full
SAPM_DELTA_MA_Full_Out

# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SAPM_DELTA_Dredge_Output_Final <- 
  SAPM_DELTA_Dredge_Output %>% 
  replace_na(list(DELTA_RC1 = "", DELTA_RC2 = "", DELTA_RC3 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SAPM_DELTA_Dredge_Output_Final

write.csv(SAPM_DELTA_Dredge_Output_Final, 
          "Tables/SAPM_DELTA_Dredge_Abun_Output.csv", 
          row.names = FALSE)


# SJR ---------------------------------------------------------------------

# SJ
# Zero inflated data. Not appropriate to use multiple regression

# * Weights Table Output --------------------------------------------------

# Weights table output

# Reorder loadings matrix
col.order <- c("RC1","RC2","RC3")


# i. Delta-Wide Models ----------------------------------------------------

# Loadings
SAPM_SAC_PCA_Loadings_Output_1 <- 
  as.data.frame(SAPM_SAC_PCA_Loadings[,col.order]) # Reordering columns
SAPM_SAC_PCA_Loadings_Output_1

SAPM_SAC_PCA_Loadings_Output_2 <- 
  SAPM_SAC_PCA_Loadings_Output_1 %>% 
  rownames_to_column() # Making row names into column
SAPM_SAC_PCA_Loadings_Output_2 

# Getting original variable (e.g., mean flow) w/ high loadings for each PC
SAPM_SAC_PCA_Loadings_Output <- 
  SAPM_SAC_PCA_Loadings_Output_2 %>% 
  pivot_longer(cols = c(RC1, RC2, RC3),
               names_to = "temp") %>% # Switching to long format
  arrange(desc(value)) %>% # Sorting by loadings to get top variable for each PC
  slice(1:3) %>% # Selecting the the top variable for each PC
  mutate(Original_Variable = rowname,
         Original_Var_Link = case_when(rowname == "SAC_Flow_Timing" ~ "Flow Timing",
                                       rowname == "SAC_Mean_WT" ~ "Water Temp.",
                                       rowname == "Mean_SAC_Discharge" ~ "Mean Discharge"),
         Variable = case_when(temp == "RC1" ~ "SAC_RC1",
                              temp == "RC2" ~ "SAC_RC2",
                              temp == "RC3" ~ "SAC_RC3")) %>% 
  select(Original_Var_Link, Original_Variable, Variable, value)
SAPM_SAC_PCA_Loadings_Output

# Weights output
SAPM_DW_Abun_Weights_Wide <- 
  pivot_wider(SAPM_DW_Weights, 
              names_from = Region,
              values_from = Weights_Sum) %>% 
  left_join(SAPM_SAC_PCA_Loadings_Output, by = "Variable") %>% 
  rename(PC = Variable,
         Loadings = value) %>% 
  select("Species",
         "Original_Var_Link",
         "Original_Variable",
         "PC",
         "DW",
         "Loadings")
SAPM_DW_Abun_Weights_Wide

# ii. SAC Models ----------------------------------------------------

# Loadings
# Still SAC PCA loadings (using from the previous section)
 
# Weights output
SAPM_SAC_Abun_Weights_Wide <- 
  pivot_wider(SAPM_SAC_Weights, 
              names_from = Region,
              values_from = Weights_Sum) %>% 
  left_join(SAPM_SAC_PCA_Loadings_Output, by = "Variable") %>% 
  rename(PC = Variable,
         Loadings = value) %>% 
  select("Species",
         "Original_Var_Link",
         "Original_Variable",
         "PC",
         "SAC",
         "Loadings")
SAPM_SAC_Abun_Weights_Wide


# iii. Delta Models ----------------------------------------------------

# Loadings
SAPM_DELTA_PCA_Loadings_Output_1 <- 
  as.data.frame(SAPM_DELTA_PCA_Loadings[,col.order]) # Reordering columns
SAPM_DELTA_PCA_Loadings_Output_1

SAPM_DELTA_PCA_Loadings_Output_2 <- 
  SAPM_DELTA_PCA_Loadings_Output_1 %>% 
  rownames_to_column() # Making row names thier own column
SAPM_DELTA_PCA_Loadings_Output_2 

# Getting original variable (e.g., mean flow) w/ high loadings for each PC
SAPM_DELTA_PCA_Loadings_Output <- 
  SAPM_DELTA_PCA_Loadings_Output_2 %>% 
  pivot_longer(cols = c(RC1, RC2, RC3),
               names_to = "temp") %>% # Switching to long format
  arrange(desc(value)) %>% # Sorting to get top variable for each PC
  slice(1:3) %>% # Selecting the the top variable for each PC
  mutate(Original_Variable = rowname,
         Original_Var_Link = case_when(rowname == "SAC_Flow_Timing" ~ "Flow Timing",
                                       rowname == "DELTA_Mean_WT" ~ "Water Temp.",
                                       rowname == "Mean_SAC_Discharge" ~ "Mean Discharge"),
         Variable = case_when(temp == "RC1" ~ "DELTA_RC1",
                              temp == "RC2" ~ "DELTA_RC2",
                              temp == "RC3" ~ "DELTA_RC3")) %>% 
  select(Original_Var_Link, Original_Variable, Variable, value)
SAPM_DELTA_PCA_Loadings_Output

# Weights output
SAPM_DELTA_Abun_Weights_Wide <- 
  pivot_wider(SAPM_DELTA_Weights, 
              names_from = Region,
              values_from = Weights_Sum) %>% 
  left_join(SAPM_DELTA_PCA_Loadings_Output, by = "Variable") %>% 
  rename(PC = Variable,
         Loadings = value) %>% 
  select("Species",
         "Original_Var_Link",
         "Original_Variable",
         "PC",
         "DELTA",
         "Loadings")
SAPM_DELTA_Abun_Weights_Wide

# iv. Combined Weights Table ----------------------------------------------

# With RCs listed 
# Because PC vary by region (e.g., SAC_RC1, SJ_RC1, etc) need to first new RC variables and then combine into a new dataframe
SAPM_DW_Weights_RC <- 
  SAPM_DW_Weights %>% 
  mutate(RC = case_when(grepl("RC1", Variable) ~ "RC1",
                        grepl("RC2", Variable) ~ "RC2",
                        grepl("RC3", Variable) ~ "RC3")) %>% 
  pivot_wider( names_from = Region,
               values_from = Weights_Sum)

SAPM_DELTA_Weights_RC <- 
  SAPM_DELTA_Weights %>% 
  mutate(RC = case_when(grepl("RC1", Variable) ~ "RC1",
                        grepl("RC2", Variable) ~ "RC2",
                        grepl("RC3", Variable) ~ "RC3")) %>% 
  pivot_wider( names_from = Region,
               values_from = Weights_Sum)

SAPM_SAC_Weights_RC <- 
  SAPM_SAC_Weights %>% 
  mutate(RC = case_when(grepl("RC1", Variable) ~ "RC1",
                        grepl("RC2", Variable) ~ "RC2",
                        grepl("RC3", Variable) ~ "RC3")) %>% 
  pivot_wider( names_from = Region,
               values_from = Weights_Sum)

SAPM_Abun_Weights_RC_Final <- 
  SAPM_DW_Weights_RC %>% 
  left_join(SAPM_DELTA_Weights_RC, by = c("Species", "RC")) %>%
  left_join(SAPM_SAC_Weights_RC, by = c("Species", "RC")) %>% 
  select("Species", "RC", 'DW', "DELTA", "SAC")
SAPM_Abun_Weights_RC_Final

# With input variables listed
SAPM_Abun_Weights_Final <- 
  SAPM_DW_Abun_Weights_Wide %>% 
  left_join(SAPM_DELTA_Abun_Weights_Wide, by = c("Species", "Original_Var_Link")) %>%
  left_join(SAPM_SAC_Abun_Weights_Wide, by = c("Species", "Original_Var_Link")) %>% 
  select("Species", "Original_Var_Link", 'DW', "DELTA", "SAC")
SAPM_Abun_Weights_Final


# v. Combined Loadings Table ----------------------------------------------

# Getting loadings for each region to output to a species specific loadings table

SAPM_DW_Loadings_Output <- 
  SAPM_SAC_PCA_Loadings_Output_2 %>% 
  mutate(Species = "SAPM",
         Region = "Delta-Wide Abundance",
         Covariates = case_when(rowname == "SAC_Flow_Timing" ~ "Sacramento Flow Timing",
                   rowname == "SAC_Mean_WT" ~ "Sacramento Water Temperature",
                   rowname == "Mean_SAC_Discharge" ~ "Mean Sacramento Discharge")) %>% 
  select("Species", "Region", "Covariates", "RC1", "RC2", "RC3")
SAPM_DW_Loadings_Output

SAPM_SAC_Loadings_Output <- 
  SAPM_SAC_PCA_Loadings_Output_2 %>% 
  mutate(Species = "SAPM",
         Region = "Sacramento Abundance",
         Covariates = case_when(rowname == "SAC_Flow_Timing" ~ "Sacramento Flow Timing",
                                rowname == "SAC_Mean_WT" ~ "Sacramento Water Temperature",
                                rowname == "Mean_SAC_Discharge" ~ "Mean Sacramento Discharge")) %>% 
  select("Species", "Region", "Covariates", "RC1", "RC2", "RC3")
SAPM_SAC_Loadings_Output

SAPM_DELTA_Loadings_Output <- 
  SAPM_DELTA_PCA_Loadings_Output_2 %>% 
  mutate(Species = "SAPM",
         Region = "Delta Abundance",
         Covariates = case_when(rowname == "SAC_Flow_Timing" ~ "Sacramento Flow Timing",
                                rowname == "DELTA_Mean_WT" ~ "Delta Water Temperature",
                                rowname == "Mean_SAC_Discharge" ~ "Mean Sacramento Discharge")) %>% 
  select("Species", "Region", "Covariates", "RC1", "RC2", "RC3")
SAPM_DELTA_Loadings_Output

SAPM_CDist_Loadings_Output <- 
  SAPM_CDist_PCA_Loadings_Output_2 %>% 
  mutate(Species = "SAPM",
         Region = "Sacramento Distribution",
         Covariates = case_when(rowname == "SAC_Flow_Timing" ~ "Sacramento Flow Timing",
                                rowname == "SAC_CDist_Mean_WT" ~ "Sacramento Water Temperature",
                                rowname == "Mean_SAC_Discharge" ~ "Mean Sacramento Discharge")) %>% 
  select("Species", "Region", "Covariates", "RC1", "RC2", "RC3")
SAPM_CDist_Loadings_Output

SAPM_Loadings_Final_Output <- 
  SAPM_DW_Loadings_Output %>% 
  bind_rows(SAPM_SAC_Loadings_Output, 
            SAPM_DELTA_Loadings_Output,
            SAPM_CDist_Loadings_Output)

write.csv(SAPM_Loadings_Final_Output, 
          "Tables/SAPM_Loadings_Final_Output.csv", 
          row.names = FALSE)


# vi. Combined Dredge -----------------------------------------------------

# Dredge output taken from i. Dredge Output above
SAPM_DW_Dredge_Output_Final
SAPM_SAC_Dredge_Output_Final
SAPM_DELTA_Dredge_Output_Final

# Loadings taken from * Weights Table Output above
SAPM_SAC_PCA_Loadings_Output
SAPM_DELTA_PCA_Loadings_Output

# Sac loadings (used for Delta-wide and Sac abundance models)
# Getting original variable w/ highest loadings for each PC
SAPM_SAC_RC1 <- 
  SAPM_SAC_PCA_Loadings_Output %>% 
  filter(Variable == "SAC_RC1") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SAPM_SAC_RC1

SAPM_SAC_RC2 <- 
  SAPM_SAC_PCA_Loadings_Output %>% 
  filter(Variable == "SAC_RC2") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SAPM_SAC_RC2

SAPM_SAC_RC3 <- 
  SAPM_SAC_PCA_Loadings_Output %>% 
  filter(Variable == "SAC_RC3") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SAPM_SAC_RC3

# Delta loadings
SAPM_DELTA_RC1 <- 
  SAPM_DELTA_PCA_Loadings_Output %>% 
  filter(Variable == "DELTA_RC1") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SAPM_DELTA_RC1

SAPM_DELTA_RC2 <- 
  SAPM_DELTA_PCA_Loadings_Output %>% 
  filter(Variable == "DELTA_RC2") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SAPM_DELTA_RC2

SAPM_DELTA_RC3 <- 
  SAPM_DELTA_PCA_Loadings_Output %>% 
  filter(Variable == "DELTA_RC3") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SAPM_DELTA_RC3

# Delta-Wide Models

# New object w/ PCs changed to original variables
SAPM_DW_Dredge_Original_Var_Final <- 
  SAPM_DW_Dredge_Output_Final 

# Renaming PC columns w/ original variable names
names(SAPM_DW_Dredge_Original_Var_Final)[names(SAPM_DW_Dredge_Original_Var_Final)=="SAC_RC1"] <- SAPM_SAC_RC1  

names(SAPM_DW_Dredge_Original_Var_Final)[names(SAPM_DW_Dredge_Original_Var_Final)=="SAC_RC2"] <- SAPM_SAC_RC2  

names(SAPM_DW_Dredge_Original_Var_Final)[names(SAPM_DW_Dredge_Original_Var_Final)=="SAC_RC3"] <- SAPM_SAC_RC3  

head(SAPM_DW_Dredge_Original_Var_Final)

# Sac Models
# New object w/ PCs changed to original variables
SAPM_SAC_Dredge_Original_Var_Final <- 
  SAPM_SAC_Dredge_Output_Final 

# Renaming PC columns w/ original variable names
names(SAPM_SAC_Dredge_Original_Var_Final)[names(SAPM_SAC_Dredge_Original_Var_Final)=="SAC_RC1"] <- SAPM_SAC_RC1  

names(SAPM_SAC_Dredge_Original_Var_Final)[names(SAPM_SAC_Dredge_Original_Var_Final)=="SAC_RC2"] <- SAPM_SAC_RC2  

names(SAPM_SAC_Dredge_Original_Var_Final)[names(SAPM_SAC_Dredge_Original_Var_Final)=="SAC_RC3"] <- SAPM_SAC_RC3  

head(SAPM_SAC_Dredge_Original_Var_Final)


# Delta Models
# New object w/ PCs changed to original variables
SAPM_DELTA_Dredge_Original_Var_Final <- 
  SAPM_DELTA_Dredge_Output_Final 

# Renaming PC columns w/ original variable names
names(SAPM_DELTA_Dredge_Original_Var_Final)[names(SAPM_DELTA_Dredge_Original_Var_Final)=="DELTA_RC1"] <- SAPM_DELTA_RC1  

names(SAPM_DELTA_Dredge_Original_Var_Final)[names(SAPM_DELTA_Dredge_Original_Var_Final)=="DELTA_RC2"] <- SAPM_DELTA_RC2  

names(SAPM_DELTA_Dredge_Original_Var_Final)[names(SAPM_DELTA_Dredge_Original_Var_Final)=="DELTA_RC3"] <- SAPM_DELTA_RC3  

head(SAPM_DELTA_Dredge_Original_Var_Final)

# Combining dredge output for all abundance models
SAPM_Dredge_Abun_Original_Var_Combined <- 
  SAPM_DW_Dredge_Original_Var_Final %>% 
  bind_rows(SAPM_DELTA_Dredge_Original_Var_Final,
            SAPM_SAC_Dredge_Original_Var_Final)
SAPM_Dredge_Abun_Original_Var_Combined

# Going to output to csv in the CDist section

# SPLT --------------------------------------------------------------------

# Final data for models
head(SPLT_Model_Data_Final)

# 1) Delta-Wide Models ----------------------------------------------------

pairs(SPLT_Model_Data_Final[,c(4,6:8)], pch = 16)

# Delta-Wide Index
SPLT_DW_Global_lm <- 
  lm(log(Delta_Wide_Index) ~ SAC_RC3 + SAC_RC2 + SAC_RC1, 
     data = SPLT_Model_Data_Final, na.action = "na.fail")
summary(SPLT_DW_Global_lm)

# Model assumptions
plot(SPLT_DW_Global_lm) # Assumptions look fine
acf(SPLT_DW_Global_lm$residuals) # No Autocorreltion

## All subset selection
SPLT_DW_Global_Dredge <- 
  dredge(SPLT_DW_Global_lm,
         extra = list(
           "R^2", "Dredge_Function" = function(x) {
             s <- summary(x)
             c(adjRsq = s$adj.r.squared)
           }))
SPLT_DW_Global_Dredge
# Top models include Flow timing

importance(SPLT_DW_Global_Dredge)
# All variables are important

### Getting model output and weights
SPLT_DW_Dredge_Output <- 
  as.data.frame(cbind(round(SPLT_DW_Global_Dredge$`(Intercept)`, 2),
                      round(SPLT_DW_Global_Dredge$SAC_RC1, 2),
                      round(SPLT_DW_Global_Dredge$SAC_RC2, 2), 
                      round(SPLT_DW_Global_Dredge$SAC_RC3, 2),
                      SPLT_DW_Global_Dredge$df,
                      round(SPLT_DW_Global_Dredge$`R^2`, 2),
                      round(SPLT_DW_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SPLT_DW_Global_Dredge$logLik, 2), 
                      round(SPLT_DW_Global_Dredge$AICc, 2),
                      round(SPLT_DW_Global_Dredge$delta, 2), 
                      SPLT_DW_Global_Dredge$weight)) %>% 
  mutate(Species = "SPLT", Region = "DW")
colnames(SPLT_DW_Dredge_Output) = c("Intercept", "SAC_RC1", "SAC_RC2", "SAC_RC3", 
                                    "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                    "Delta", "Weights", "Species", "Region")

# Getting weights for output
SPLT_DW_RC2_Weights <- 
  SPLT_DW_Dredge_Output %>% 
  filter(!(is.na(SAC_RC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "DW", Variable = "SAC_RC2")

SPLT_DW_RC3_Weights <- 
  SPLT_DW_Dredge_Output %>% 
  filter(!(is.na(SAC_RC3))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "DW", Variable = "SAC_RC3")

SPLT_DW_Weights <- 
  SPLT_DW_Dredge_Output %>% 
  filter(!(is.na(SAC_RC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "DW", Variable = "SAC_RC1") %>% 
  bind_rows(SPLT_DW_RC2_Weights, SPLT_DW_RC3_Weights)
SPLT_DW_Weights

# Checking weight output
importance(SPLT_DW_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set
SPLT_DW_MA <- model.avg(SPLT_DW_Global_Dredge, 
                       cumsum(weight) <= .95, 
                       rank = "AIC")
SPLT_DW_MA_Sum = summary(SPLT_DW_MA)
SPLT_DW_MA_Full_Out = SPLT_DW_MA_Sum$coefmat.full
SPLT_DW_MA_Full_Out

# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SPLT_DW_Dredge_Output_Final <- 
  SPLT_DW_Dredge_Output %>% 
  replace_na(list(SAC_RC1 = "", SAC_RC2 = "", SAC_RC3 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SPLT_DW_Dredge_Output_Final

write.csv(SPLT_DW_Dredge_Output_Final, 
          "Tables/SPLT_DW_Dredge_Abun_Output.csv", 
          row.names = FALSE)

head(SPLT_DW_Dredge_Output_Final)

# 2) SJ Models ----------------------------------------------------

pairs(SPLT_Model_Data_Final[,c(3,9:11)], pch = 16)

# Global model for all subset selection
SPLT_SJ_Global_lm <- 
  lm(log(San_Joaquin_River_Index) ~ SJ_RC1 + SJ_RC2 + SJ_RC3, 
     data = SPLT_Model_Data_Final, na.action = "na.fail")
summary(SPLT_SJ_Global_lm)

# Model assumptions
plot(SPLT_SJ_Global_lm) # Assumptions look fine
acf(SPLT_SJ_Global_lm$residuals) # Slight Autocorreltion at lag 3

## All subset selection
SPLT_SJ_Global_Dredge <- 
  dredge(SPLT_SJ_Global_lm,
         extra = list(
           "R^2", "Dredge_Function" = function(x) {
             s <- summary(x)
             c(adjRsq = s$adj.r.squared)
           }))
SPLT_SJ_Global_Dredge

importance(SPLT_SJ_Global_Dredge)
# All 3 variables have high weights

### Getting model output and weights
SPLT_SJ_Dredge_Output <- 
  as.data.frame(cbind(round(SPLT_SJ_Global_Dredge$`(Intercept)`, 2),
                      round(SPLT_SJ_Global_Dredge$SJ_RC1, 2),
                      round(SPLT_SJ_Global_Dredge$SJ_RC2, 2),
                      round(SPLT_SJ_Global_Dredge$SJ_RC3, 2),
                      SPLT_SJ_Global_Dredge$df,
                      round(SPLT_SJ_Global_Dredge$`R^2`, 2),
                      round(SPLT_SJ_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SPLT_SJ_Global_Dredge$logLik, 2), 
                      round(SPLT_SJ_Global_Dredge$AICc, 2),
                      round(SPLT_SJ_Global_Dredge$delta, 2), 
                      SPLT_SJ_Global_Dredge$weight)) %>% 
  mutate(Species = "SPLT", Region = "SJ")
colnames(SPLT_SJ_Dredge_Output) = c("Intercept", "SJ_RC1", "SJ_RC2", "SJ_RC3", 
                                    "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                    "Delta", "Weights", "Species", "Region")
SPLT_SJ_Dredge_Output

# Getting weights output
SPLT_SJ_RC2_Weights <- 
  SPLT_SJ_Dredge_Output %>% 
  filter(!(is.na(SJ_RC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "SJ", Variable = "SJ_RC2")

SPLT_SJ_RC3_Weights <- 
  SPLT_SJ_Dredge_Output %>% 
  filter(!(is.na(SJ_RC3))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "SJ", Variable = "SJ_RC3")

SPLT_SJ_Weights <- 
  SPLT_SJ_Dredge_Output %>% 
  filter(!(is.na(SJ_RC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "SJ", Variable = "SJ_RC1") %>% 
  bind_rows(SPLT_SJ_RC2_Weights, SPLT_SJ_RC3_Weights)
SPLT_SJ_Weights

# Checking weights output
importance(SPLT_SJ_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set
SPLT_SJ_MA <- model.avg(SPLT_SJ_Global_Dredge, 
                       cumsum(weight) <= .99, # 99 OW only one model and doesn't work
                       rank = "AIC")
SPLT_SJ_MA_Sum = summary(SPLT_SJ_MA)
SPLT_SJ_MA_Full_Out = SPLT_SJ_MA_Sum$coefmat.full
SPLT_SJ_MA_Full_Out


# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SPLT_SJ_Dredge_Output_Final <- 
  SPLT_SJ_Dredge_Output %>% 
  replace_na(list(SJ_RC1 = "", SJ_RC2 = "", SJ_RC3 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SPLT_SJ_Dredge_Output_Final

write.csv(SPLT_SJ_Dredge_Output_Final, 
          "Tables/SPLT_SJ_Dredge_Abun_Output.csv", 
          row.names = FALSE)

# 3) SAC Models ----------------------------------------------------

pairs(SPLT_Model_Data_Final[,c(2,6:8)], pch = 16)

# Global model for all subset selection
SPLT_SAC_Global_lm <- 
  lm(log(Sac_River_Index) ~ SAC_RC3 + SAC_RC2 + SAC_RC1, 
     data = SPLT_Model_Data_Final, 
     na.action = "na.fail")
summary(SPLT_SAC_Global_lm)

# Model assumptions
plot(SPLT_SAC_Global_lm) # Assumptions look fine (normality is questionable)
acf(SPLT_SAC_Global_lm$residuals) # No Autocorreltion

## All subset selection
SPLT_SAC_Global_Dredge <- 
  dredge(SPLT_SAC_Global_lm,
         extra = list(
           "R^2", "Dredge_Function" = function(x) {
             s <- summary(x)
             c(adjRsq = s$adj.r.squared)
           }))
SPLT_SAC_Global_Dredge

importance(SPLT_SAC_Global_Dredge)
# No important variables (RC1 at 0.52)

### Getting model output and weights
SPLT_SAC_Dredge_Output <- 
  as.data.frame(cbind(round(SPLT_SAC_Global_Dredge$`(Intercept)`, 2),
                      round(SPLT_SAC_Global_Dredge$SAC_RC1, 2),
                      round(SPLT_SAC_Global_Dredge$SAC_RC2, 2),
                      round(SPLT_SAC_Global_Dredge$SAC_RC3, 2),
                      SPLT_SAC_Global_Dredge$df,
                      round(SPLT_SAC_Global_Dredge$`R^2`, 2),
                      round(SPLT_SAC_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SPLT_SAC_Global_Dredge$logLik, 2), 
                      round(SPLT_SAC_Global_Dredge$AICc, 2),
                      round(SPLT_SAC_Global_Dredge$delta, 2), 
                      SPLT_SAC_Global_Dredge$weight)) %>% 
  mutate(Species = "SPLT", Region = "SAC")
colnames(SPLT_SAC_Dredge_Output) = c("Intercept", "SAC_RC1", "SAC_RC2", "SAC_RC3", 
                                    "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                    "Delta", "Weights", "Species", "Region")
SPLT_SAC_Dredge_Output

# Getting weights output
SPLT_SAC_RC2_Weights <- 
  SPLT_SAC_Dredge_Output %>% 
  filter(!(is.na(SAC_RC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "SAC", Variable = "SAC_RC2")

SPLT_SAC_RC3_Weights <- 
  SPLT_SAC_Dredge_Output %>% 
  filter(!(is.na(SAC_RC3))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "SAC", Variable = "SAC_RC3")

SPLT_SAC_Weights <- 
  SPLT_SAC_Dredge_Output %>% 
  filter(!(is.na(SAC_RC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "SAC", Variable = "SAC_RC1") %>% 
  bind_rows(SPLT_SAC_RC2_Weights, SPLT_SAC_RC3_Weights)
SPLT_SAC_Weights

# Checking weights output
importance(SPLT_SAC_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set
SPLT_SAC_MA <- model.avg(SPLT_SAC_Global_Dredge, 
                       cumsum(weight) <= .95, 
                       rank = "AIC")
SPLT_SAC_MA_Sum = summary(SPLT_SAC_MA)
SPLT_SAC_MA_Full_Out = SPLT_SAC_MA_Sum$coefmat.full
SPLT_SAC_MA_Full_Out

# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SPLT_SAC_Dredge_Output_Final <- 
  SPLT_SAC_Dredge_Output %>% 
  replace_na(list(SAC_RC1 = "", SAC_RC2 = "", SAC_RC3 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SPLT_SAC_Dredge_Output_Final

write.csv(SPLT_SAC_Dredge_Output_Final, 
          "Tables/SPLT_SAC_Dredge_Abun_Output.csv", 
          row.names = FALSE)

# 4) Delta Models ----------------------------------------------------

pairs(SPLT_Model_Data_Final[,c(1,12:14)], pch = 16)

# Global model for all subset selection
SPLT_Delta_Global_lm <- 
  lm(log(Delta_Index) ~ DELTA_RC3 + DELTA_RC2 + DELTA_RC1, 
     data = SPLT_Model_Data_Final, na.action = "na.fail")
summary(SPLT_Delta_Global_lm)

# Model assumptions
plot(SPLT_Delta_Global_lm) # Assumptions look fine
acf(SPLT_Delta_Global_lm$residuals) # No Autocorreltion

## All subset selection
SPLT_DELTA_Global_Dredge <- 
  dredge(SPLT_Delta_Global_lm,
         extra = list(
           "R^2", "Dredge_Function" = function(x) {
             s <- summary(x)
             c(adjRsq = s$adj.r.squared)
           }))
SPLT_DELTA_Global_Dredge

importance(SPLT_DELTA_Global_Dredge)

### Getting model output and weights
SPLT_DELTA_Dredge_Output <- 
  as.data.frame(cbind(round(SPLT_DELTA_Global_Dredge$`(Intercept)`, 2),
                      round(SPLT_DELTA_Global_Dredge$DELTA_RC1, 2),
                      round(SPLT_DELTA_Global_Dredge$DELTA_RC2, 2), 
                      round(SPLT_DELTA_Global_Dredge$DELTA_RC3, 2),
                      SPLT_DELTA_Global_Dredge$df,
                      round(SPLT_DELTA_Global_Dredge$`R^2`, 2),
                      round(SPLT_DELTA_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SPLT_DELTA_Global_Dredge$logLik, 2), 
                      round(SPLT_DELTA_Global_Dredge$AICc, 2),
                      round(SPLT_DELTA_Global_Dredge$delta, 2), 
                      SPLT_DELTA_Global_Dredge$weight)) %>% 
  mutate(Species = "SPLT", Region = "DELTA")
colnames(SPLT_DELTA_Dredge_Output) = c("Intercept", "DELTA_RC1", "DELTA_RC2", "DELTA_RC3", 
                                    "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                    "Delta", "Weights", "Species", "Region")
SPLT_DELTA_Dredge_Output

# Getting weights output
SPLT_DELTA_RC2_Weights <- 
  SPLT_DELTA_Dredge_Output %>% 
  filter(!(is.na(DELTA_RC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "DELTA", Variable = "DELTA_RC2")

SPLT_DELTA_RC3_Weights <- 
  SPLT_DELTA_Dredge_Output %>% 
  filter(!(is.na(DELTA_RC3))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "DELTA", Variable = "DELTA_RC3")

SPLT_DELTA_Weights <- 
  SPLT_DELTA_Dredge_Output %>% 
  filter(!(is.na(DELTA_RC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "DELTA", Variable = "DELTA_RC1") %>% 
  bind_rows(SPLT_DELTA_RC2_Weights, SPLT_DELTA_RC3_Weights)
SPLT_DELTA_Weights

# Checking weights output
importance(SPLT_DELTA_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set
SPLT_DELTA_MA <- model.avg(SPLT_DELTA_Global_Dredge, 
                       cumsum(weight) <= .95, 
                       rank = "AIC")
SPLT_DELTA_MA_Sum = summary(SPLT_DELTA_MA)
SPLT_DELTA_MA_Full_Out = SPLT_DELTA_MA_Sum$coefmat.full
SPLT_DELTA_MA_Full_Out

# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SPLT_DELTA_Dredge_Output_Final <- 
  SPLT_DELTA_Dredge_Output %>% 
  replace_na(list(DELTA_RC1 = "", DELTA_RC2 = "", DELTA_RC3 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SPLT_DELTA_Dredge_Output_Final

write.csv(SPLT_DELTA_Dredge_Output_Final, 
          "Tables/SPLT_DELTA_Dredge_Abun_Output.csv", 
          row.names = FALSE)

# * Weights Table Output --------------------------------------------------

# Weights table output

# Reorder loadings matrix
col.order <- c("RC1","RC2","RC3")


# i. Delta-Wide Models ----------------------------------------------------

# Loadings
SPLT_SAC_PCA_Loadings_Output_1 <- 
  as.data.frame(SPLT_SAC_PCA_Loadings[,col.order]) # Reordering columns
SPLT_SAC_PCA_Loadings_Output_1

SPLT_SAC_PCA_Loadings_Output_2 <- 
  SPLT_SAC_PCA_Loadings_Output_1 %>% 
  rownames_to_column() # Making row names thier own column
SPLT_SAC_PCA_Loadings_Output_2 

# Getting original variable (e.g., mean flow) w/ high loadings for each PC
SPLT_SAC_PCA_Loadings_Output <- 
  SPLT_SAC_PCA_Loadings_Output_2 %>% 
  pivot_longer(cols = c(RC1, RC2, RC3),
               names_to = "temp") %>% # Switching to long format
  arrange(desc(value)) %>% # Sorting to get top variable for each PC
  slice(1:3) %>% # Selecting the the top variable for each PC
  mutate(Original_Variable = rowname,
         Original_Var_Link = case_when(rowname == "SAC_Flow_Timing" ~ "Flow Timing",
                                       rowname == "SAC_Mean_WT" ~ "Water Temp.",
                                       rowname == "Mean_SAC_Discharge" ~ "Mean Discharge"),
         Variable = case_when(temp == "RC1" ~ "SAC_RC1",
                              temp == "RC2" ~ "SAC_RC2",
                              temp == "RC3" ~ "SAC_RC3")) %>% 
  select(Original_Var_Link, Original_Variable, Variable, value)
SPLT_SAC_PCA_Loadings_Output

# Weights output
SPLT_DW_Abun_Weights_Wide <- 
  pivot_wider(SPLT_DW_Weights, 
              names_from = Region,
              values_from = Weights_Sum) %>% 
  left_join(SPLT_SAC_PCA_Loadings_Output, by = "Variable") %>% 
  rename(PC = Variable,
         Loadings = value) %>% 
  select("Species",
         "Original_Var_Link",
         "Original_Variable",
         "PC",
         "DW",
         "Loadings")
SPLT_DW_Abun_Weights_Wide

# ii. SAC Models ----------------------------------------------------

# Loadings
# Still SAC PCA loadings (using from the previous section)

# Weights output
SPLT_SAC_Abun_Weights_Wide <- 
  pivot_wider(SPLT_SAC_Weights, 
              names_from = Region,
              values_from = Weights_Sum) %>% 
  left_join(SPLT_SAC_PCA_Loadings_Output, by = "Variable") %>% 
  rename(PC = Variable,
         Loadings = value) %>% 
  select("Species",
         "Original_Var_Link",
         "Original_Variable",
         "PC",
         "SAC",
         "Loadings")
SPLT_SAC_Abun_Weights_Wide

# iii. SJ Models ----------------------------------------------------

# Loadings
SPLT_SJ_PCA_Loadings_Output_1 <- 
  as.data.frame(SPLT_SJ_PCA_Loadings[,col.order]) # Reordering columns
SPLT_SJ_PCA_Loadings_Output_1

SPLT_SJ_PCA_Loadings_Output_2 <- 
  SPLT_SJ_PCA_Loadings_Output_1 %>% 
  rownames_to_column() # Making row names thier own column
SPLT_SJ_PCA_Loadings_Output_2 

# Getting original variable (e.g., mean flow) w/ high loadings for each PC
SPLT_SJ_PCA_Loadings_Output <- 
  SPLT_SJ_PCA_Loadings_Output_2 %>% 
  pivot_longer(cols = c(RC1, RC2, RC3),
               names_to = "temp") %>% # Switching to long format
  arrange(desc(value)) %>% # Sorting to get top variable for each PC
  slice(1:3) %>% # Selecting the the top variable for each PC
  mutate(Original_Variable = rowname,
         Original_Var_Link = case_when(rowname == "SJ_Flow_Timing" ~ "Flow Timing",
                                       rowname == "SJ_Mean_WT" ~ "Water Temp.",
                                       rowname == "Mean_SJ_Discharge" ~ "Mean Discharge"),
         Variable = case_when(temp == "RC1" ~ "SJ_RC1",
                              temp == "RC2" ~ "SJ_RC2",
                              temp == "RC3" ~ "SJ_RC3")) %>% 
  select(Original_Var_Link, Original_Variable, Variable, value)
SPLT_SJ_PCA_Loadings_Output

# Weights output
SPLT_SJ_Abun_Weights_Wide <- 
  pivot_wider(SPLT_SJ_Weights, 
              names_from = Region,
              values_from = Weights_Sum) %>% 
  left_join(SPLT_SJ_PCA_Loadings_Output, by = "Variable") %>% 
  rename(PC = Variable,
         Loadings = value) %>% 
  select("Species",
         "Original_Var_Link",
         "Original_Variable",
         "PC",
         "SJ",
         "Loadings")
SPLT_SJ_Abun_Weights_Wide

# iv. Delta Models ----------------------------------------------------

# Loadings
SPLT_DELTA_PCA_Loadings_Output_1 <- 
  as.data.frame(SPLT_DELTA_PCA_Loadings[,col.order]) # Reordering columns
SPLT_DELTA_PCA_Loadings_Output_1

SPLT_DELTA_PCA_Loadings_Output_2 <- 
  SPLT_DELTA_PCA_Loadings_Output_1 %>% 
  rownames_to_column() # Making row names thier own column
SPLT_DELTA_PCA_Loadings_Output_2 

# Getting original variable (e.g., mean flow) w/ high loadings for each PC
SPLT_DELTA_PCA_Loadings_Output <- 
  SPLT_DELTA_PCA_Loadings_Output_2 %>% 
  pivot_longer(cols = c(RC1, RC2, RC3),
               names_to = "temp") %>% # Switching to long format
  arrange(desc(value)) %>% # Sorting to get top variable for each PC
  slice(1:3) %>% # Selecting the the top variable for each PC
  mutate(Original_Variable = rowname,
         Original_Var_Link = case_when(rowname == "SAC_Flow_Timing" ~ "Flow Timing",
                                       rowname == "DELTA_Mean_WT" ~ "Water Temp.",
                                       rowname == "Mean_SAC_Discharge" ~ "Mean Discharge"),
         Variable = case_when(temp == "RC1" ~ "DELTA_RC1",
                              temp == "RC2" ~ "DELTA_RC2",
                              temp == "RC3" ~ "DELTA_RC3")) %>% 
  select(Original_Var_Link, Original_Variable, Variable, value)
SPLT_DELTA_PCA_Loadings_Output

# Weights output
SPLT_DELTA_Abun_Weights_Wide <- 
  pivot_wider(SPLT_DELTA_Weights, 
              names_from = Region,
              values_from = Weights_Sum) %>% 
  left_join(SPLT_DELTA_PCA_Loadings_Output, by = "Variable") %>% 
  rename(PC = Variable,
         Loadings = value) %>% 
  select("Species",
         "Original_Var_Link",
         "Original_Variable",
         "PC",
         "DELTA",
         "Loadings")
SPLT_DELTA_Abun_Weights_Wide

# v. Combined Weights Table ----------------------------------------------

# With RCs listed 
# Because PC vary by region (e.g., SAC_RC1, SJ_RC1, etc) need to first new RC variables and then combine into a new dataframe
SPLT_DW_Weights_RC <- 
  SPLT_DW_Weights %>% 
  mutate(RC = case_when(grepl("RC1", Variable) ~ "RC1",
                        grepl("RC2", Variable) ~ "RC2",
                        grepl("RC3", Variable) ~ "RC3")) %>% 
  pivot_wider( names_from = Region,
               values_from = Weights_Sum)

SPLT_DELTA_Weights_RC <- 
  SPLT_DELTA_Weights %>% 
  mutate(RC = case_when(grepl("RC1", Variable) ~ "RC1",
                        grepl("RC2", Variable) ~ "RC2",
                        grepl("RC3", Variable) ~ "RC3")) %>% 
  pivot_wider( names_from = Region,
               values_from = Weights_Sum)

SPLT_SAC_Weights_RC <- 
  SPLT_SAC_Weights %>% 
  mutate(RC = case_when(grepl("RC1", Variable) ~ "RC1",
                        grepl("RC2", Variable) ~ "RC2",
                        grepl("RC3", Variable) ~ "RC3")) %>% 
  pivot_wider( names_from = Region,
               values_from = Weights_Sum)

SPLT_SJ_Weights_RC <- 
  SPLT_SJ_Weights %>% 
  mutate(RC = case_when(grepl("RC1", Variable) ~ "RC1",
                        grepl("RC2", Variable) ~ "RC2",
                        grepl("RC3", Variable) ~ "RC3")) %>% 
  pivot_wider( names_from = Region,
               values_from = Weights_Sum)

SPLT_Abun_Weights_RC_Final <- 
  SPLT_DW_Weights_RC %>% 
  left_join(SPLT_DELTA_Weights_RC, by = c("Species", "RC")) %>%
  left_join(SPLT_SAC_Weights_RC, by = c("Species", "RC")) %>% 
  left_join(SPLT_SJ_Weights_RC, by = c("Species", "RC")) %>%
  select("Species", "RC", 'DW', "DELTA", "SAC", "SJ")
SPLT_Abun_Weights_RC_Final

# W/ original input variables
SPLT_Abun_Weights_Final <- 
  SPLT_DW_Abun_Weights_Wide %>% 
  left_join(SPLT_DELTA_Abun_Weights_Wide, by = c("Species", "Original_Var_Link")) %>%
  left_join(SPLT_SAC_Abun_Weights_Wide, by = c("Species", "Original_Var_Link")) %>% 
  left_join(SPLT_SJ_Abun_Weights_Wide, by = c("Species", "Original_Var_Link")) %>%
  select("Species", "Original_Var_Link", 'DW', "DELTA", "SAC", "SJ")
SPLT_Abun_Weights_Final

# vi. Combined Loadings Table ----------------------------------------------

# Getting loadings for each region to output to a species specific loadings table

SPLT_DW_Loadings_Output <- 
  SPLT_SAC_PCA_Loadings_Output_2 %>% 
  mutate(Species = "SPLT",
         Region = "Delta-Wide Abundance",
         Covariates = case_when(rowname == "SAC_Flow_Timing" ~ "Sacramento Flow Timing",
                                rowname == "SAC_Mean_WT" ~ "Sacramento Water Temperature",
                                rowname == "Mean_SAC_Discharge" ~ "Mean Sacramento Discharge")) %>% 
  select("Species", "Region", "Covariates", "RC1", "RC2", "RC3")
SPLT_DW_Loadings_Output

SPLT_SAC_Loadings_Output <- 
  SPLT_SAC_PCA_Loadings_Output_2 %>% 
  mutate(Species = "SPLT",
         Region = "Sacramento Abundance",
         Covariates = case_when(rowname == "SAC_Flow_Timing" ~ "Sacramento Flow Timing",
                                rowname == "SAC_Mean_WT" ~ "Sacramento Water Temperature",
                                rowname == "Mean_SAC_Discharge" ~ "Mean Sacramento Discharge")) %>% 
  select("Species", "Region", "Covariates", "RC1", "RC2", "RC3")
SPLT_SAC_Loadings_Output

SPLT_DELTA_Loadings_Output <- 
  SPLT_DELTA_PCA_Loadings_Output_2 %>% 
  mutate(Species = "SPLT",
         Region = "Delta Abundance",
         Covariates = case_when(rowname == "SAC_Flow_Timing" ~ "Sacramento Flow Timing",
                                rowname == "DELTA_Mean_WT" ~ "Delta Water Temperature",
                                rowname == "Mean_SAC_Discharge" ~ "Mean Sacramento Discharge")) %>% 
  select("Species", "Region", "Covariates", "RC1", "RC2", "RC3")
SPLT_DELTA_Loadings_Output

SPLT_SJ_Loadings_Output <- 
  SPLT_SJ_PCA_Loadings_Output_2 %>% 
  mutate(Species = "SPLT",
         Region = "San Joaquin Abundance",
         Covariates = case_when(rowname == "SJ_Flow_Timing" ~ "San Joaquin Flow Timing",
                                rowname == "SJ_Mean_WT" ~ "San Joaquin Water Temperature",
                                rowname == "Mean_SJ_Discharge" ~ "Mean San Joaquin Discharge")) %>% 
  select("Species", "Region", "Covariates", "RC1", "RC2", "RC3")
SPLT_SJ_Loadings_Output

SPLT_CDist_Loadings_Output <- 
  SPLT_CDist_PCA_Loadings_Output_2 %>% 
  mutate(Species = "SPLT",
         Region = "Sacramento Distribution",
         Covariates = case_when(rowname == "SAC_Flow_Timing" ~ "Sacramento Flow Timing",
                                rowname == "SAC_CDist_Mean_WT" ~ "Sacramento Water Temperature",
                                rowname == "Mean_SAC_Discharge" ~ "Mean Sacramento Discharge")) %>% 
  select("Species", "Region", "Covariates", "RC1", "RC2", "RC3")
SPLT_CDist_Loadings_Output

SPLT_Loadings_Final_Output <- 
  SPLT_DW_Loadings_Output %>% 
  bind_rows(SPLT_SAC_Loadings_Output, 
            SPLT_DELTA_Loadings_Output,
            SPLT_SJ_Loadings_Output,
            SPLT_CDist_Loadings_Output)


write.csv(SPLT_Loadings_Final_Output, 
          "Tables/SPLT_Loadings_Final_Output.csv", 
          row.names = FALSE)


# vii. Combined Dredge -----------------------------------------------------

# Dredge output taken from i. Dredge Output above
SPLT_DW_Dredge_Output_Final
SPLT_SAC_Dredge_Output_Final
SPLT_DELTA_Dredge_Output_Final
SPLT_SJ_Dredge_Output_Final

# Loadings taken from * Weights Table Output above
SPLT_SAC_PCA_Loadings_Output
SPLT_DELTA_PCA_Loadings_Output
SPLT_SJ_PCA_Loadings_Output

# Sac loadings (used for Delta-wide and Sac abundance models)
# Getting original variable w/ highest loadings for each PC
SPLT_SAC_RC1 <- 
  SPLT_SAC_PCA_Loadings_Output %>% 
  filter(Variable == "SAC_RC1") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SPLT_SAC_RC1

SPLT_SAC_RC2 <- 
  SPLT_SAC_PCA_Loadings_Output %>% 
  filter(Variable == "SAC_RC2") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SPLT_SAC_RC2

SPLT_SAC_RC3 <- 
  SPLT_SAC_PCA_Loadings_Output %>% 
  filter(Variable == "SAC_RC3") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SPLT_SAC_RC3

# Delta loadings
SPLT_DELTA_RC1 <- 
  SPLT_DELTA_PCA_Loadings_Output %>% 
  filter(Variable == "DELTA_RC1") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SPLT_DELTA_RC1

SPLT_DELTA_RC2 <- 
  SPLT_DELTA_PCA_Loadings_Output %>% 
  filter(Variable == "DELTA_RC2") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SPLT_DELTA_RC2

SPLT_DELTA_RC3 <- 
  SPLT_DELTA_PCA_Loadings_Output %>% 
  filter(Variable == "DELTA_RC3") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SPLT_DELTA_RC3

# SJ loadings
SPLT_SJ_RC1 <- 
  SPLT_SJ_PCA_Loadings_Output %>% 
  filter(Variable == "SJ_RC1") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SPLT_SJ_RC1

SPLT_SJ_RC2 <- 
  SPLT_SJ_PCA_Loadings_Output %>% 
  filter(Variable == "SJ_RC2") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SPLT_SJ_RC2

SPLT_SJ_RC3 <- 
  SPLT_SJ_PCA_Loadings_Output %>% 
  filter(Variable == "SJ_RC3") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SPLT_SJ_RC3


# Delta-Wide Models

# New object w/ PCs changed to original variables
SPLT_DW_Dredge_Original_Var_Final <- 
  SPLT_DW_Dredge_Output_Final 

# Renaming PC columns w/ original variable names
names(SPLT_DW_Dredge_Original_Var_Final)[names(SPLT_DW_Dredge_Original_Var_Final)=="SAC_RC1"] <- SPLT_SAC_RC1  

names(SPLT_DW_Dredge_Original_Var_Final)[names(SPLT_DW_Dredge_Original_Var_Final)=="SAC_RC2"] <- SPLT_SAC_RC2  

names(SPLT_DW_Dredge_Original_Var_Final)[names(SPLT_DW_Dredge_Original_Var_Final)=="SAC_RC3"] <- SPLT_SAC_RC3  

head(SPLT_DW_Dredge_Original_Var_Final)

# Sac Models
# New object w/ PCs changed to original variables
SPLT_SAC_Dredge_Original_Var_Final <- 
  SPLT_SAC_Dredge_Output_Final 

# Renaming PC columns w/ original variable names
names(SPLT_SAC_Dredge_Original_Var_Final)[names(SPLT_SAC_Dredge_Original_Var_Final)=="SAC_RC1"] <- SPLT_SAC_RC1  

names(SPLT_SAC_Dredge_Original_Var_Final)[names(SPLT_SAC_Dredge_Original_Var_Final)=="SAC_RC2"] <- SPLT_SAC_RC2  

names(SPLT_SAC_Dredge_Original_Var_Final)[names(SPLT_SAC_Dredge_Original_Var_Final)=="SAC_RC3"] <- SPLT_SAC_RC3  

head(SPLT_SAC_Dredge_Original_Var_Final)


# Delta Models
# New object w/ PCs changed to original variables
SPLT_DELTA_Dredge_Original_Var_Final <- 
  SPLT_DELTA_Dredge_Output_Final 

# Renaming PC columns w/ original variable names
names(SPLT_DELTA_Dredge_Original_Var_Final)[names(SPLT_DELTA_Dredge_Original_Var_Final)=="DELTA_RC1"] <- SPLT_DELTA_RC1  

names(SPLT_DELTA_Dredge_Original_Var_Final)[names(SPLT_DELTA_Dredge_Original_Var_Final)=="DELTA_RC2"] <- SPLT_DELTA_RC2  

names(SPLT_DELTA_Dredge_Original_Var_Final)[names(SPLT_DELTA_Dredge_Original_Var_Final)=="DELTA_RC3"] <- SPLT_DELTA_RC3  

head(SPLT_DELTA_Dredge_Original_Var_Final)

# SJ Models
# New object w/ PCs changed to original variables
SPLT_SJ_Dredge_Original_Var_Final <- 
  SPLT_SJ_Dredge_Output_Final 

# Renaming PC columns w/ original variable names
names(SPLT_SJ_Dredge_Original_Var_Final)[names(SPLT_SJ_Dredge_Original_Var_Final)=="SJ_RC1"] <- SPLT_SJ_RC1  

names(SPLT_SJ_Dredge_Original_Var_Final)[names(SPLT_SJ_Dredge_Original_Var_Final)=="SJ_RC2"] <- SPLT_SJ_RC2  

names(SPLT_SJ_Dredge_Original_Var_Final)[names(SPLT_SJ_Dredge_Original_Var_Final)=="SJ_RC3"] <- SPLT_SJ_RC3  

head(SPLT_SJ_Dredge_Original_Var_Final)

# Combining dredge output for all abundance models
SPLT_Dredge_Abun_Original_Var_Combined <- 
  SPLT_DW_Dredge_Original_Var_Final %>% 
  bind_rows(SPLT_DELTA_Dredge_Original_Var_Final,
            SPLT_SAC_Dredge_Original_Var_Final,
            SPLT_SJ_Dredge_Original_Var_Final)
SPLT_Dredge_Abun_Original_Var_Combined

# Going to output to csv in the CDist section

# SASU --------------------------------------------------------------------

# Final data
head(SASU_Model_Data_Final)

# 1) Delta-Wide Models ----------------------------------------------------

pairs(SASU_Model_Data_Final[,c(4,6:8)], pch = 16)

SASU_DW_Global_lm <- 
  lm(log(Delta_Wide_Index) ~ SAC_RC3 + SAC_RC2 + SAC_RC1, 
     data = SASU_Model_Data_Final, na.action = "na.fail")
summary(SASU_DW_Global_lm)

# Model assumptions
plot(SASU_DW_Global_lm) # Assumptions look fine
acf(SASU_SAC_Global_lm$residuals) # No Autocorreltion

## All subset selection
SASU_DW_Global_Dredge <- 
  dredge(SASU_DW_Global_lm,
         extra = list(
           "R^2", "Dredge_Function" = function(x) {
             s <- summary(x)
             c(adjRsq = s$adj.r.squared)
           }))
SASU_DW_Global_Dredge

importance(SASU_DW_Global_Dredge)
# Mean flow (RC 1) is most important variable; weights = 0.80

### Getting model output and weights
SASU_DW_Dredge_Output <- 
  as.data.frame(cbind(round(SASU_DW_Global_Dredge$`(Intercept)`, 2),
                      round(SASU_DW_Global_Dredge$SAC_RC1, 2),
                      round(SASU_DW_Global_Dredge$SAC_RC2, 2),
                      round(SASU_DW_Global_Dredge$SAC_RC3, 2),
                      SASU_DW_Global_Dredge$df,
                      round(SASU_DW_Global_Dredge$`R^2`, 2),
                      round(SASU_DW_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SASU_DW_Global_Dredge$logLik, 2), 
                      round(SASU_DW_Global_Dredge$AICc, 2),
                      round(SASU_DW_Global_Dredge$delta, 2), 
                      SASU_DW_Global_Dredge$weight)) %>% 
  mutate(Species = "SASU", Region = "DW")
colnames(SASU_DW_Dredge_Output) = c("Intercept", "SAC_RC1", "SAC_RC2", "SAC_RC3", 
                                    "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                    "Delta", "Weights", "Species", "Region")
SASU_DW_Dredge_Output

# Getting weights output
SASU_DW_RC2_Weights <- 
  SASU_DW_Dredge_Output %>% 
  filter(!(is.na(SAC_RC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "DW", Variable = "SAC_RC2")

SASU_DW_RC3_Weights <- 
  SASU_DW_Dredge_Output %>% 
  filter(!(is.na(SAC_RC3))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "DW", Variable = "SAC_RC3")

SASU_DW_Weights <- 
  SASU_DW_Dredge_Output %>% 
  filter(!(is.na(SAC_RC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "DW", Variable = "SAC_RC1") %>% 
  bind_rows(SASU_DW_RC2_Weights, SASU_DW_RC3_Weights)
SASU_DW_Weights

# Checking weights output
importance(SASU_DW_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set
SASU_DW_MA <- model.avg(SASU_DW_Global_Dredge, 
                       cumsum(weight) <= .95, 
                       rank = "AIC")
SASU_DW_MA_Sum = summary(SASU_DW_MA)
SASU_DW_MA_Full_Out = SASU_DW_MA_Sum$coefmat.full
SASU_DW_MA_Full_Out

# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SASU_DW_Dredge_Output_Final <- 
  SASU_DW_Dredge_Output %>% 
  replace_na(list(SAC_RC1 = "", SAC_RC2 = "", SAC_RC3 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SASU_DW_Dredge_Output_Final

write.csv(SASU_DW_Dredge_Output_Final, 
          "Tables/SASU_DW_Dredge_Abun_Output.csv", 
          row.names = FALSE)

# 2) SJ Models ----------------------------------------------------

pairs(SASU_Model_Data_Final[,c(3,9:11)], pch = 16)

# Global model for all subset selection
SASU_SJ_Global_lm <- 
  lm(log(San_Joaquin_River_Index) ~ SJ_RC3 + SJ_RC2 + SJ_RC1, 
     data = SASU_Model_Data_Final, na.action = "na.fail")
summary(SASU_SJ_Global_lm)

# Model assumptions
plot(SASU_SJ_Global_lm) # Normality is questionable 
acf(SASU_SJ_Global_lm$residuals) # No Autocorreltion
vif(SASU_SJ_Global_lm) # All < 2

## All subset selection
SASU_SJ_Global_Dredge <- 
  dredge(SASU_SJ_Global_lm,
         extra = list(
           "R^2", "Dredge_Function" = function(x) {
             s <- summary(x)
             c(adjRsq = s$adj.r.squared)
           }))
SASU_SJ_Global_Dredge
# Top models include water temp (RC 1)

importance(SASU_SJ_Global_Dredge)
# Water temp (RC 1) is most important variable; weights = 0.90

### Getting model output and weights
SASU_SJ_Dredge_Output <- 
  as.data.frame(cbind(round(SASU_SJ_Global_Dredge$`(Intercept)`, 2), 
                      round(SASU_SJ_Global_Dredge$SJ_RC1, 2),
                      round(SASU_SJ_Global_Dredge$SJ_RC2, 2),
                      round(SASU_SJ_Global_Dredge$SJ_RC3, 2),
                      SASU_SJ_Global_Dredge$df,
                      round(SASU_SJ_Global_Dredge$`R^2`, 2),
                      round(SASU_SJ_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SASU_SJ_Global_Dredge$logLik, 2), 
                      round(SASU_SJ_Global_Dredge$AICc, 2),
                      round(SASU_SJ_Global_Dredge$delta, 2), 
                      SASU_SJ_Global_Dredge$weight)) %>% 
  mutate(Species = "SASU", Region = "SJ")
colnames(SASU_SJ_Dredge_Output) = c("Intercept", "SJ_RC1", "SJ_RC2", "SJ_RC3", 
                                    "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                    "Delta", "Weights", "Species", "Region")
SASU_SJ_Dredge_Output

# Getting weights output
SASU_SJ_RC2_Weights <- 
  SASU_SJ_Dredge_Output %>% 
  filter(!(is.na(SJ_RC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "SJ", Variable = "SJ_RC2")

SASU_SJ_RC3_Weights <- 
  SASU_SJ_Dredge_Output %>% 
  filter(!(is.na(SJ_RC3))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "SJ", Variable = "SJ_RC3")

SASU_SJ_Weights <- 
  SASU_SJ_Dredge_Output %>% 
  filter(!(is.na(SJ_RC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "SJ", Variable = "SJ_RC1") %>% 
  bind_rows(SASU_SJ_RC2_Weights, SASU_SJ_RC3_Weights)
SASU_SJ_Weights

# Checking weights output
importance(SASU_SJ_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set
SASU_SJ_MA = model.avg(SASU_SJ_Global_Dredge, 
                       cumsum(weight) <= .95, 
                       rank = "AIC")
SASU_SJ_MA_Sum = summary(SASU_SJ_MA)
SASU_SJ_MA_Full_Out = SASU_SJ_MA_Sum$coefmat.full
SASU_SJ_MA_Full_Out

# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SASU_SJ_Dredge_Output_Final <- 
  SASU_SJ_Dredge_Output %>% 
  replace_na(list(SJ_RC1 = "", SJ_RC2 = "", SJ_RC3 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SASU_SJ_Dredge_Output_Final

write.csv(SASU_SJ_Dredge_Output_Final, 
          "Tables/SASU_SJ_Dredge_Abun_Output.csv", 
          row.names = FALSE)

# 3) SAC Models ----------------------------------------------------

pairs(SASU_Model_Data_Final[,c(2,6:8)], pch = 16)

# Global model for all subset selection
SASU_SAC_Global_lm <- 
  lm(log(Sac_River_Index) ~ SAC_RC3 + SAC_RC2 + SAC_RC1, 
     data = SASU_Model_Data_Final, na.action = "na.fail")
summary(SASU_SAC_Global_lm)

# Model assumptions
plot(SASU_SAC_Global_lm) # Assumptions look fine
acf(SASU_SAC_Global_lm$residuals) # No Autocorreltion
vif(SASU_SAC_Global_lm) # All < 2

## All subset selection
SASU_SAC_Global_Dredge <- 
  dredge(SASU_SAC_Global_lm,
         extra = list(
           "R^2", "Dredge_Function" = function(x) {
             s <- summary(x)
             c(adjRsq = s$adj.r.squared)
           }))
SASU_SAC_Global_Dredge
# Top models include discharge (RC 1) and water temp (RC 3)

importance(SASU_SAC_Global_Dredge)

### Getting model output and weights
SASU_SAC_Dredge_Output <- 
  as.data.frame(cbind(round(SASU_SAC_Global_Dredge$`(Intercept)`, 2),
                      round(SASU_SAC_Global_Dredge$SAC_RC1, 2),
                      round(SASU_SAC_Global_Dredge$SAC_RC2, 2),
                      round(SASU_SAC_Global_Dredge$SAC_RC3, 2),
                      SASU_SAC_Global_Dredge$df,
                      round(SASU_SAC_Global_Dredge$`R^2`, 2),
                      round(SASU_SAC_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SASU_SAC_Global_Dredge$logLik, 2), 
                      round(SASU_SAC_Global_Dredge$AICc, 2),
                      round(SASU_SAC_Global_Dredge$delta, 2), 
                      SASU_SAC_Global_Dredge$weight)) %>% 
  mutate(Species = "SASU", Region = "SAC")
colnames(SASU_SAC_Dredge_Output) = c("Intercept", "SAC_RC1", "SAC_RC2", "SAC_RC3", 
                                    "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                    "Delta", "Weights", "Species", "Region")
SASU_SAC_Dredge_Output

# Getting weights output
SASU_SAC_RC2_Weights <- 
  SASU_SAC_Dredge_Output %>% 
  filter(!(is.na(SAC_RC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "SAC", Variable = "SAC_RC2")

SASU_SAC_RC3_Weights <- 
  SASU_SAC_Dredge_Output %>% 
  filter(!(is.na(SAC_RC3))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "SAC", Variable = "SAC_RC3")

SASU_SAC_Weights <- 
  SASU_SAC_Dredge_Output %>% 
  filter(!(is.na(SAC_RC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "SAC", Variable = "SAC_RC1") %>% 
  bind_rows(SASU_SAC_RC2_Weights, SASU_SAC_RC3_Weights)
SASU_SAC_Weights

# Checking weights output
importance(SASU_SAC_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set
SASU_SAC_MA <- model.avg(SASU_SAC_Global_Dredge, 
                       cumsum(weight) <= .95, 
                       rank = "AIC")
SASU_SAC_MA_Sum = summary(SASU_SAC_MA)
SASU_SAC_MA_Full_Out = SASU_SAC_MA_Sum$coefmat.full
SASU_SAC_MA_Full_Out

# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SASU_SAC_Dredge_Output_Final <- 
  SASU_SAC_Dredge_Output %>% 
  replace_na(list(SAC_RC1 = "", SAC_RC2 = "", SAC_RC3 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SASU_SAC_Dredge_Output_Final

write.csv(SASU_SAC_Dredge_Output_Final, 
          "Tables/SASU_SAC_Dredge_Abun_Output.csv", 
          row.names = FALSE)

# 4) Delta Models ----------------------------------------------------

pairs(SASU_Model_Data_Final[,c(1,12:14)], pch = 16)

# Global model for all subset selection
SASU_Delta_Global_lm <- 
  lm(log(Delta_Index) ~ DELTA_RC3 + DELTA_RC2 + DELTA_RC1, 
     data = SASU_Model_Data_Final, na.action = "na.fail")
summary(SASU_Delta_Global_lm)

# Model assumptions
plot(SASU_Delta_Global_lm) # Assumptions look fine
acf(SASU_Delta_Global_lm$residuals) # No Autocorreltion
vif(SASU_Delta_Global_lm) # All < 2

## All subset selection
SASU_DELTA_Global_Dredge <- 
  dredge(SASU_Delta_Global_lm,
         extra = list(
           "R^2", "Dredge_Function" = function(x) {
             s <- summary(x)
             c(adjRsq = s$adj.r.squared)
           }))
SASU_DELTA_Global_Dredge
# Top models include Flow timing (RC 2)

importance(SASU_DELTA_Global_Dredge)
# Flow timing (RC 2) weights = 0.90

### Getting model output and weights
SASU_DELTA_Dredge_Output <- 
  as.data.frame(cbind(round(SASU_DELTA_Global_Dredge$`(Intercept)`, 2),
                      round(SASU_DELTA_Global_Dredge$DELTA_RC1, 2),
                      round(SASU_DELTA_Global_Dredge$DELTA_RC2, 2),
                      round(SASU_DELTA_Global_Dredge$DELTA_RC3, 2),
                      SASU_DELTA_Global_Dredge$df,
                      round(SASU_DELTA_Global_Dredge$`R^2`, 2),
                      round(SASU_DELTA_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SASU_DELTA_Global_Dredge$logLik, 2), 
                      round(SASU_DELTA_Global_Dredge$AICc, 2),
                      round(SASU_DELTA_Global_Dredge$delta, 2), 
                      SASU_DELTA_Global_Dredge$weight)) %>% 
  mutate(Species = "SASU", Region = "DELTA")
colnames(SASU_DELTA_Dredge_Output) = c("Intercept", "DELTA_RC1", "DELTA_RC2",
                                       "DELTA_RC3", "DF", "R^2", "R^2_Adj",
                                       "logLik","AICc", "Delta", "Weights",
                                       "Species", "Region")
SASU_DELTA_Dredge_Output

# Getting weights output
SASU_DELTA_RC2_Weights <- 
  SASU_DELTA_Dredge_Output %>% 
  filter(!(is.na(DELTA_RC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "DELTA", Variable = "DELTA_RC2")

SASU_DELTA_RC3_Weights <- 
  SASU_DELTA_Dredge_Output %>% 
  filter(!(is.na(DELTA_RC3))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "DELTA", Variable = "DELTA_RC3")

SASU_DELTA_Weights <- 
  SASU_DELTA_Dredge_Output %>% 
  filter(!(is.na(DELTA_RC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "DELTA", Variable = "DELTA_RC1") %>% 
  bind_rows(SASU_DELTA_RC2_Weights, SASU_DELTA_RC3_Weights)
SASU_DELTA_Weights

# Checking weights output
importance(SASU_DELTA_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set
SASU_DELTA_MA <- model.avg(SASU_DELTA_Global_Dredge, 
                       cumsum(weight) <= .95, 
                       rank = "AIC")
SASU_DELTA_MA_Sum = summary(SASU_DELTA_MA)
SASU_DELTA_MA_Full_Out = SASU_DELTA_MA_Sum$coefmat.full
SASU_DELTA_MA_Full_Out

# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SASU_DELTA_Dredge_Output_Final <- 
  SASU_DELTA_Dredge_Output %>% 
  replace_na(list(DELTA_RC1 = "", DELTA_RC2 = "", DELTA_RC3 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SASU_DELTA_Dredge_Output_Final

write.csv(SASU_DELTA_Dredge_Output_Final, 
          "Tables/SASU_DELTA_Dredge_Abun_Output.csv", 
          row.names = FALSE)

# * Weights Table Output --------------------------------------------------

# Weights table output

# Reorder loadings matrix
col.order <- c("RC1","RC2","RC3")


# i. Delta-Wide Models ----------------------------------------------------

# Loadings
SASU_SAC_PCA_Loadings_Output_1 <- 
  as.data.frame(SASU_SAC_PCA_Loadings[,col.order]) # Reordering columns
SASU_SAC_PCA_Loadings_Output_1

SASU_SAC_PCA_Loadings_Output_2 <- 
  SASU_SAC_PCA_Loadings_Output_1 %>% 
  rownames_to_column() # Making row names thier own column
SASU_SAC_PCA_Loadings_Output_2 

# Getting original variable (e.g., mean flow) w/ high loadings for each PC
SASU_SAC_PCA_Loadings_Output <- 
  SASU_SAC_PCA_Loadings_Output_2 %>% 
  pivot_longer(cols = c(RC1, RC2, RC3),
               names_to = "temp") %>% # Switching to long format
  arrange(desc(value)) %>% # Sorting to get top variable for each PC
  slice(1:3) %>% # Selecting the the top variable for each PC
  mutate(Original_Variable = rowname,
         Original_Var_Link = case_when(rowname == "SAC_Flow_Timing" ~ "Flow Timing",
                                       rowname == "SAC_Mean_WT" ~ "Water Temp.",
                                       rowname == "Mean_SAC_Discharge" ~ "Mean Discharge"),
         Variable = case_when(temp == "RC1" ~ "SAC_RC1",
                              temp == "RC2" ~ "SAC_RC2",
                              temp == "RC3" ~ "SAC_RC3")) %>% 
  select(Original_Var_Link, Original_Variable, Variable, value)
SASU_SAC_PCA_Loadings_Output

# Weights output
SASU_DW_Abun_Weights_Wide <- 
  pivot_wider(SASU_DW_Weights, 
              names_from = Region,
              values_from = Weights_Sum) %>% 
  left_join(SASU_SAC_PCA_Loadings_Output, by = "Variable") %>% 
  rename(PC = Variable,
         Loadings = value) %>% 
  select("Species",
         "Original_Var_Link",
         "Original_Variable",
         "PC",
         "DW",
         "Loadings")
SASU_DW_Abun_Weights_Wide

# ii. SAC Models ----------------------------------------------------

# Loadings
# Still SAC PCA loadings (using from the previous section)

# Weights output
SASU_SAC_Abun_Weights_Wide <- 
  pivot_wider(SASU_SAC_Weights, 
              names_from = Region,
              values_from = Weights_Sum) %>% 
  left_join(SASU_SAC_PCA_Loadings_Output, by = "Variable") %>% 
  rename(PC = Variable,
         Loadings = value) %>% 
  select("Species",
         "Original_Var_Link",
         "Original_Variable",
         "PC",
         "SAC",
         "Loadings")
SASU_SAC_Abun_Weights_Wide

# iii. SJ Models ----------------------------------------------------

# Loadings
SASU_SJ_PCA_Loadings_Output_1 <- 
  as.data.frame(SASU_SJ_PCA_Loadings[,col.order]) # Reordering columns
SASU_SJ_PCA_Loadings_Output_1

SASU_SJ_PCA_Loadings_Output_2 <- 
  SASU_SJ_PCA_Loadings_Output_1 %>% 
  rownames_to_column() # Making row names thier own column
SASU_SJ_PCA_Loadings_Output_2 

# Getting original variable (e.g., mean flow) w/ high loadings for each PC
SASU_SJ_PCA_Loadings_Output <- 
  SASU_SJ_PCA_Loadings_Output_2 %>% 
  pivot_longer(cols = c(RC1, RC2, RC3),
               names_to = "temp") %>% # Switching to long format
  arrange(desc(value)) %>% # Sorting to get top variable for each PC
  slice(1:3) %>% # Selecting the the top variable for each PC
  mutate(Original_Variable = rowname,
         Original_Var_Link = case_when(rowname == "SJ_Flow_Timing" ~ "Flow Timing",
                                       rowname == "SJ_Mean_WT" ~ "Water Temp.",
                                       rowname == "Mean_SJ_Discharge" ~ "Mean Discharge"),
         Variable = case_when(temp == "RC1" ~ "SJ_RC1",
                              temp == "RC2" ~ "SJ_RC2",
                              temp == "RC3" ~ "SJ_RC3")) %>% 
  select(Original_Var_Link, Original_Variable, Variable, value)
SASU_SJ_PCA_Loadings_Output

# Weights output
SASU_SJ_Abun_Weights_Wide <- 
  pivot_wider(SASU_SJ_Weights, 
              names_from = Region,
              values_from = Weights_Sum) %>% 
  left_join(SASU_SJ_PCA_Loadings_Output, by = "Variable") %>% 
  rename(PC = Variable,
         Loadings = value) %>% 
  select("Species",
         "Original_Var_Link",
         "Original_Variable",
         "PC",
         "SJ",
         "Loadings")
SASU_SJ_Abun_Weights_Wide

# iv. Delta Models ----------------------------------------------------

# Loadings
SASU_DELTA_PCA_Loadings_Output_1 <- 
  as.data.frame(SASU_DELTA_PCA_Loadings[,col.order]) # Reordering columns
SASU_DELTA_PCA_Loadings_Output_1

SASU_DELTA_PCA_Loadings_Output_2 <- 
  SASU_DELTA_PCA_Loadings_Output_1 %>% 
  rownames_to_column() # Making row names thier own column
SASU_DELTA_PCA_Loadings_Output_2 

# Getting original variable (e.g., mean flow) w/ high loadings for each PC
SASU_DELTA_PCA_Loadings_Output <- 
  SASU_DELTA_PCA_Loadings_Output_2 %>% 
  pivot_longer(cols = c(RC1, RC2, RC3),
               names_to = "temp") %>% # Switching to long format
  arrange(desc(value)) %>% # Sorting to get top variable for each PC
  slice(1:3) %>% # Selecting the the top variable for each PC
  mutate(Original_Variable = rowname,
         Original_Var_Link = case_when(rowname == "SAC_Flow_Timing" ~ "Flow Timing",
                                       rowname == "DELTA_Mean_WT" ~ "Water Temp.",
                                       rowname == "Mean_SAC_Discharge" ~ "Mean Discharge"),
         Variable = case_when(temp == "RC1" ~ "DELTA_RC1",
                              temp == "RC2" ~ "DELTA_RC2",
                              temp == "RC3" ~ "DELTA_RC3")) %>% 
  select(Original_Var_Link, Original_Variable, Variable, value)
SASU_DELTA_PCA_Loadings_Output

# Weights output
SASU_DELTA_Abun_Weights_Wide <- 
  pivot_wider(SASU_DELTA_Weights, 
              names_from = Region,
              values_from = Weights_Sum) %>% 
  left_join(SASU_DELTA_PCA_Loadings_Output, by = "Variable") %>% 
  rename(PC = Variable,
         Loadings = value) %>% 
  select("Species",
         "Original_Var_Link",
         "Original_Variable",
         "PC",
         "DELTA",
         "Loadings")
SASU_DELTA_Abun_Weights_Wide

# v. Combined Weights Table ----------------------------------------------

# With RCs listed 
# Because PC vary by region (e.g., SAC_RC1, SJ_RC1, etc) need to first new RC variables and then combine into a new dataframe
SASU_DW_Weights_RC <- 
  SASU_DW_Weights %>% 
  mutate(RC = case_when(grepl("RC1", Variable) ~ "RC1",
                        grepl("RC2", Variable) ~ "RC2",
                        grepl("RC3", Variable) ~ "RC3")) %>% 
  pivot_wider( names_from = Region,
               values_from = Weights_Sum)

SASU_DELTA_Weights_RC <- 
  SASU_DELTA_Weights %>% 
  mutate(RC = case_when(grepl("RC1", Variable) ~ "RC1",
                        grepl("RC2", Variable) ~ "RC2",
                        grepl("RC3", Variable) ~ "RC3")) %>% 
  pivot_wider( names_from = Region,
               values_from = Weights_Sum)

SASU_SAC_Weights_RC <- 
  SASU_SAC_Weights %>% 
  mutate(RC = case_when(grepl("RC1", Variable) ~ "RC1",
                        grepl("RC2", Variable) ~ "RC2",
                        grepl("RC3", Variable) ~ "RC3")) %>% 
  pivot_wider( names_from = Region,
               values_from = Weights_Sum)

SASU_SJ_Weights_RC <- 
  SASU_SJ_Weights %>% 
  mutate(RC = case_when(grepl("RC1", Variable) ~ "RC1",
                        grepl("RC2", Variable) ~ "RC2",
                        grepl("RC3", Variable) ~ "RC3")) %>% 
  pivot_wider( names_from = Region,
               values_from = Weights_Sum)

SASU_Abun_Weights_RC_Final <- 
  SASU_DW_Weights_RC %>% 
  left_join(SASU_DELTA_Weights_RC, by = c("Species", "RC")) %>%
  left_join(SASU_SAC_Weights_RC, by = c("Species", "RC")) %>% 
  left_join(SASU_SJ_Weights_RC, by = c("Species", "RC")) %>%
  select("Species", "RC", 'DW', "DELTA", "SAC", "SJ")
SASU_Abun_Weights_RC_Final


# W/ original input variables listed
SASU_Abun_Weights_Final <- 
  SASU_DW_Abun_Weights_Wide %>% 
  left_join(SASU_DELTA_Abun_Weights_Wide, by = c("Species", "Original_Var_Link")) %>%
  left_join(SASU_SAC_Abun_Weights_Wide, by = c("Species", "Original_Var_Link")) %>% 
  left_join(SASU_SJ_Abun_Weights_Wide, by = c("Species", "Original_Var_Link")) %>%
  select("Species", "Original_Var_Link", 'DW', "DELTA", "SAC", "SJ")
SASU_Abun_Weights_Final

# vi. Combined Loadings Table ----------------------------------------------

# Getting loadings for each region to output to a species specific loadings table

SASU_DW_Loadings_Output <- 
  SASU_SAC_PCA_Loadings_Output_2 %>% 
  mutate(Species = "SASU",
         Region = "Delta-Wide Abundance",
         Covariates = case_when(rowname == "SAC_Flow_Timing" ~ "Sacramento Flow Timing",
                                rowname == "SAC_Mean_WT" ~ "Sacramento Water Temperature",
                                rowname == "Mean_SAC_Discharge" ~ "Mean Sacramento Discharge")) %>% 
  select("Species", "Region", "Covariates", "RC1", "RC2", "RC3")
SASU_DW_Loadings_Output

SASU_SAC_Loadings_Output <- 
  SASU_SAC_PCA_Loadings_Output_2 %>% 
  mutate(Species = "SASU",
         Region = "Sacramento Abundance",
         Covariates = case_when(rowname == "SAC_Flow_Timing" ~ "Sacramento Flow Timing",
                                rowname == "SAC_Mean_WT" ~ "Sacramento Water Temperature",
                                rowname == "Mean_SAC_Discharge" ~ "Mean Sacramento Discharge")) %>% 
  select("Species", "Region", "Covariates", "RC1", "RC2", "RC3")
SASU_SAC_Loadings_Output

SASU_DELTA_Loadings_Output <- 
  SASU_DELTA_PCA_Loadings_Output_2 %>% 
  mutate(Species = "SASU",
         Region = "Delta Abundance",
         Covariates = case_when(rowname == "SAC_Flow_Timing" ~ "Sacramento Flow Timing",
                                rowname == "DELTA_Mean_WT" ~ "Delta Water Temperature",
                                rowname == "Mean_SAC_Discharge" ~ "Mean Sacramento Discharge")) %>% 
  select("Species", "Region", "Covariates", "RC1", "RC2", "RC3")
SASU_DELTA_Loadings_Output

SASU_SJ_Loadings_Output <- 
  SASU_SJ_PCA_Loadings_Output_2 %>% 
  mutate(Species = "SASU",
         Region = "San Joaquin Abundance",
         Covariates = case_when(rowname == "SJ_Flow_Timing" ~ "San Joaquin Flow Timing",
                                rowname == "SJ_Mean_WT" ~ "San Joaquin Water Temperature",
                                rowname == "Mean_SJ_Discharge" ~ "Mean San Joaquin Discharge")) %>% 
  select("Species", "Region", "Covariates", "RC1", "RC2", "RC3")
SASU_SJ_Loadings_Output

SASU_CDist_Loadings_Output <- 
  SASU_CDist_PCA_Loadings_Output_2 %>% 
  mutate(Species = "SASU",
         Region = "Sacramento Distribution",
         Covariates = case_when(rowname == "SAC_Flow_Timing" ~ "Sacramento Flow Timing",
                                rowname == "SAC_CDist_Mean_WT" ~ "Sacramento Water Temperature",
                                rowname == "Mean_SAC_Discharge" ~ "Mean Sacramento Discharge")) %>% 
  select("Species", "Region", "Covariates", "RC1", "RC2", "RC3")
SASU_CDist_Loadings_Output

SASU_Loadings_Final_Output <- 
  SASU_DW_Loadings_Output %>% 
  bind_rows(SASU_SAC_Loadings_Output, 
            SASU_DELTA_Loadings_Output,
            SASU_SJ_Loadings_Output,
            SASU_CDist_Loadings_Output)


write.csv(SASU_Loadings_Final_Output, 
          "Tables/SASU_Loadings_Final_Output.csv", 
          row.names = FALSE)

# vii. Combined Dredge -----------------------------------------------------

# Dredge output taken from i. Dredge Output above
SASU_DW_Dredge_Output_Final
SASU_SAC_Dredge_Output_Final
SASU_DELTA_Dredge_Output_Final
SASU_SJ_Dredge_Output_Final

# Loadings taken from * Weights Table Output above
SASU_SAC_PCA_Loadings_Output
SASU_DELTA_PCA_Loadings_Output
SASU_SJ_PCA_Loadings_Output

# Sac loadings (used for Delta-wide and Sac abundance models)
# Getting original variable w/ highest loadings for each PC
SASU_SAC_RC1 <- 
  SASU_SAC_PCA_Loadings_Output %>% 
  filter(Variable == "SAC_RC1") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SASU_SAC_RC1

SASU_SAC_RC2 <- 
  SASU_SAC_PCA_Loadings_Output %>% 
  filter(Variable == "SAC_RC2") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SASU_SAC_RC2

SASU_SAC_RC3 <- 
  SASU_SAC_PCA_Loadings_Output %>% 
  filter(Variable == "SAC_RC3") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SASU_SAC_RC3

# Delta loadings
SASU_DELTA_RC1 <- 
  SASU_DELTA_PCA_Loadings_Output %>% 
  filter(Variable == "DELTA_RC1") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SASU_DELTA_RC1

SASU_DELTA_RC2 <- 
  SASU_DELTA_PCA_Loadings_Output %>% 
  filter(Variable == "DELTA_RC2") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SASU_DELTA_RC2

SASU_DELTA_RC3 <- 
  SASU_DELTA_PCA_Loadings_Output %>% 
  filter(Variable == "DELTA_RC3") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SASU_DELTA_RC3

# SJ loadings
SASU_SJ_RC1 <- 
  SASU_SJ_PCA_Loadings_Output %>% 
  filter(Variable == "SJ_RC1") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SASU_SJ_RC1

SASU_SJ_RC2 <- 
  SASU_SJ_PCA_Loadings_Output %>% 
  filter(Variable == "SJ_RC2") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SASU_SJ_RC2

SASU_SJ_RC3 <- 
  SASU_SJ_PCA_Loadings_Output %>% 
  filter(Variable == "SJ_RC3") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SASU_SJ_RC3


# Delta-Wide Models

# New object w/ PCs changed to original variables
SASU_DW_Dredge_Original_Var_Final <- 
  SASU_DW_Dredge_Output_Final 

# Renaming PC columns w/ original variable names
names(SASU_DW_Dredge_Original_Var_Final)[names(SASU_DW_Dredge_Original_Var_Final)=="SAC_RC1"] <- SASU_SAC_RC1  

names(SASU_DW_Dredge_Original_Var_Final)[names(SASU_DW_Dredge_Original_Var_Final)=="SAC_RC2"] <- SASU_SAC_RC2  

names(SASU_DW_Dredge_Original_Var_Final)[names(SASU_DW_Dredge_Original_Var_Final)=="SAC_RC3"] <- SASU_SAC_RC3  

head(SASU_DW_Dredge_Original_Var_Final)

# Sac Models
# New object w/ PCs changed to original variables
SASU_SAC_Dredge_Original_Var_Final <- 
  SASU_SAC_Dredge_Output_Final 

# Renaming PC columns w/ original variable names
names(SASU_SAC_Dredge_Original_Var_Final)[names(SASU_SAC_Dredge_Original_Var_Final)=="SAC_RC1"] <- SASU_SAC_RC1  

names(SASU_SAC_Dredge_Original_Var_Final)[names(SASU_SAC_Dredge_Original_Var_Final)=="SAC_RC2"] <- SASU_SAC_RC2  

names(SASU_SAC_Dredge_Original_Var_Final)[names(SASU_SAC_Dredge_Original_Var_Final)=="SAC_RC3"] <- SASU_SAC_RC3  

head(SASU_SAC_Dredge_Original_Var_Final)


# Delta Models
# New object w/ PCs changed to original variables
SASU_DELTA_Dredge_Original_Var_Final <- 
  SASU_DELTA_Dredge_Output_Final 

# Renaming PC columns w/ original variable names
names(SASU_DELTA_Dredge_Original_Var_Final)[names(SASU_DELTA_Dredge_Original_Var_Final)=="DELTA_RC1"] <- SASU_DELTA_RC1  

names(SASU_DELTA_Dredge_Original_Var_Final)[names(SASU_DELTA_Dredge_Original_Var_Final)=="DELTA_RC2"] <- SASU_DELTA_RC2  

names(SASU_DELTA_Dredge_Original_Var_Final)[names(SASU_DELTA_Dredge_Original_Var_Final)=="DELTA_RC3"] <- SASU_DELTA_RC3  

head(SASU_DELTA_Dredge_Original_Var_Final)

# SJ Models
# New object w/ PCs changed to original variables
SASU_SJ_Dredge_Original_Var_Final <- 
  SASU_SJ_Dredge_Output_Final 

# Renaming PC columns w/ original variable names
names(SASU_SJ_Dredge_Original_Var_Final)[names(SASU_SJ_Dredge_Original_Var_Final)=="SJ_RC1"] <- SASU_SJ_RC1  

names(SASU_SJ_Dredge_Original_Var_Final)[names(SASU_SJ_Dredge_Original_Var_Final)=="SJ_RC2"] <- SASU_SJ_RC2  

names(SASU_SJ_Dredge_Original_Var_Final)[names(SASU_SJ_Dredge_Original_Var_Final)=="SJ_RC3"] <- SASU_SJ_RC3  

head(SASU_SJ_Dredge_Original_Var_Final)

# Combining dredge output for all abundance models
SASU_Dredge_Abun_Original_Var_Combined <- 
  SASU_DW_Dredge_Original_Var_Final %>% 
  bind_rows(SASU_DELTA_Dredge_Original_Var_Final,
            SASU_SAC_Dredge_Original_Var_Final,
            SASU_SJ_Dredge_Original_Var_Final)
SASU_Dredge_Abun_Original_Var_Combined

# Going to output to csv in the CDist section

# ***Species Combined Weights Output***----------------------------------------------

# W/ original variables
All_Sp_Abun_Weights_Output <- 
  bind_rows(SAPM_Abun_Weights_Final,
            SPLT_Abun_Weights_Final, 
            SASU_Abun_Weights_Final) %>% 
  replace_na(list(SJ = "-")) %>% 
  rename(Covariates = Original_Var_Link,
         'Delta-Wide Abundance' = DW,
         'Delta Abundance' = DELTA,
         'Sacramento Abundance' = SAC,
         'San Joaquin Abundance' = SJ)
All_Sp_Abun_Weights_Output

write.csv(All_Sp_Abun_Weights_Output, 
          "Tables/Abundance_Models_Weights.csv", 
          row.names = FALSE)

# W/ original variables
All_Sp_Abun_Weights_RC_Output <- 
  bind_rows(SAPM_Abun_Weights_RC_Final,
            SPLT_Abun_Weights_RC_Final, 
            SASU_Abun_Weights_RC_Final) %>% 
  replace_na(list(SJ = "-")) %>% 
  rename('Delta-Wide Abundance' = DW,
         'Delta Abundance' = DELTA,
         'Sacramento Abundance' = SAC,
         'San Joaquin Abundance' = SJ)
All_Sp_Abun_Weights_RC_Output

# v. Center of Distribution ---------------------------------------------

# Look at center of distribution (what Ted did) based on river KM

### Vary months used for the distribution (later then what was used for the indices) and maybe look at different months of flow too

# When considering months of flow, it probably doesn't make sense to look at flows before spawning...we are using center of dist that was calculated after those flows. It probably doesn't make sense to look at flows during spawning either unless some of the earlier hatched fish are washed DS

# CDist Summary Stats -------------------------------------------------------------

# General plots looking at change in Dist on Sac over the time series. What's apparent is that there's greater variability in the dist of SPLT, which might provide enough contrast to detect relationships with covariates. Also we might be missing a lot of larger part of SAPM and SASU pops.
plot(CDist_SAC ~ as.numeric(Year), SAPM_CDist, pch = 16)
plot(CDist_SAC ~ as.numeric(Year), SPLT_CDist, pch = 16)
plot(CDist_SAC ~ as.numeric(Year), SASU_CDist, pch = 16)

# Species
SAPM_Sum_Dist_Stats <- 
  SAPM_CDist %>% 
  mutate(Species = "SAPM") %>% 
  group_by(Species) %>% 
  summarise(Mean = mean(CDist_SAC), Min = min(CDist_SAC), Max = max(CDist_SAC), SD = sd(CDist_SAC))
head(SAPM_Sum_Dist_Stats)

SPLT_Sum_Dist_Stats <- 
  SPLT_CDist %>% 
  mutate(Species = "SPLT") %>% 
  group_by(Species) %>% 
  summarise(Mean = mean(CDist_SAC), Min = min(CDist_SAC), Max = max(CDist_SAC), SD = sd(CDist_SAC))
head(SPLT_Sum_Dist_Stats)

SASU_Sum_Dist_Stats <- 
  SASU_CDist %>% 
  mutate(Species = "SASU") %>% 
  group_by(Species) %>% 
  summarise(Mean = mean(CDist_SAC), Min = min(CDist_SAC), Max = max(CDist_SAC), SD = sd(CDist_SAC))
head(SASU_Sum_Dist_Stats)


# ***Data Output*** ------------------------------------------------------------

Dist_Summaries <- 
  rbind(SAPM_Sum_Dist_Stats,
        SPLT_Sum_Dist_Stats,
        SASU_Sum_Dist_Stats)

write.csv(Dist_Summaries, "Tables/Dist_Summaries.csv", row.names = FALSE)


# SAPM --------------------------------------------------------------------

head(SAPM_Model_Data_Final) # Distribution data

# Visualizing relationships between dist and covariates 
pairs(SAPM_Model_Data_Final[,c(4,11:13)], pch = 16)

SAPM_SAC_Dist_Global_lm <- 
  lm(log(CDist_SAC) ~ CDist_RC1 + CDist_RC2 + CDist_RC3, 
     data = SAPM_Model_Data_Final, 
     na.action = "na.fail")
summary(SAPM_SAC_Dist_Global_lm)

# Model assumptions
plot(SAPM_SAC_Dist_Global_lm) # Normality is questionable
acf(SAPM_SAC_Dist_Global_lm$residuals) # Autocorreltion is fine
vif(SAPM_SAC_Dist_Global_lm) # All < 2

## All subset selection
SAPM_SAC_Dist_Global_Dredge <- 
  dredge(SAPM_SAC_Dist_Global_lm,
         extra = list("R^2", "Dredge_Function" = function(x) {
           s <- summary(x)
           c(adjRsq = s$adj.r.squared)
         }))
SAPM_SAC_Dist_Global_Dredge
# Top model is null model

importance(SAPM_SAC_Dist_Global_Dredge)
# All weights < 0.45

### Getting model output and weights
SAPM_CDist_Dredge_Output <- 
  as.data.frame(cbind(round(SAPM_SAC_Dist_Global_Dredge$`(Intercept)`, 2), 
                      round(SAPM_SAC_Dist_Global_Dredge$CDist_RC1, 2), 
                      round(SAPM_SAC_Dist_Global_Dredge$CDist_RC2, 2), 
                      round(SAPM_SAC_Dist_Global_Dredge$CDist_RC3, 2),
                      SAPM_SAC_Dist_Global_Dredge$df,
                      round(SAPM_SAC_Dist_Global_Dredge$`R^2`, 2),
                      round(SAPM_SAC_Dist_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SAPM_SAC_Dist_Global_Dredge$logLik, 2), 
                      round(SAPM_SAC_Dist_Global_Dredge$AICc, 2),
                      round(SAPM_SAC_Dist_Global_Dredge$delta, 2), 
                      SAPM_SAC_Dist_Global_Dredge$weight)) %>% 
  mutate(Species = "SAPM")
colnames(SAPM_CDist_Dredge_Output) = c("Intercept", "CDist_RC1", "CDist_RC2",
                                       "CDist_RC3", "DF", "R^2", "R^2_Adj",
                                       "logLik","AICc", "Delta", "Weights",
                                       "Species")
SAPM_CDist_Dredge_Output

SAPM_CDist_RC2_Weights <- 
  SAPM_CDist_Dredge_Output %>% 
  filter(!(is.na(CDist_RC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", Region = "CDist", Variable = "CDist_RC2")

SAPM_CDist_RC3_Weights <- 
  SAPM_CDist_Dredge_Output %>% 
  filter(!(is.na(CDist_RC3))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", Region = "CDist", Variable = "CDist_RC3")

SAPM_CDist_Weights <- 
  SAPM_CDist_Dredge_Output %>% 
  filter(!(is.na(CDist_RC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", Region = "CDist", Variable = "CDist_RC1") %>% 
  bind_rows(SAPM_CDist_RC2_Weights, SAPM_CDist_RC3_Weights)
SAPM_CDist_Weights

# Checking weights DF
importance(SAPM_SAC_Dist_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set
SAPM_CDist_MA = model.avg(SAPM_SAC_Dist_Global_Dredge,
                          cumsum(weight) <= .95, 
                          rank = "AIC")
SAPM_CDist_MA_Sum = summary(SAPM_CDist_MA)
SAPM_CDist_MA_Full_Out = SAPM_CDist_MA_Sum$coefmat.full
SAPM_CDist_MA_Full_Out

# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SAPM_CDist_Dredge_Output_Final <- 
  SAPM_CDist_Dredge_Output %>% 
  replace_na(list(CDist_RC1 = "", CDist_RC2 = "", CDist_RC3 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SAPM_CDist_Dredge_Output_Final

write.csv(SAPM_CDist_Dredge_Output_Final, 
          "Tables/SAPM_CDist_Dredge_Abun_Output.csv", 
          row.names = FALSE)

# ii. Weights Table Output --------------------------------------------------

# Weights table output

# Reorder loadings matrix
col.order <- c("RC1","RC2","RC3")

# Loadings
SAPM_CDist_PCA_Loadings_Output_1 <- 
  as.data.frame(SAPM_CDist_PCA_Loadings[,col.order]) # Reordering columns
SAPM_CDist_PCA_Loadings_Output_1

SAPM_CDist_PCA_Loadings_Output_2 <- 
  SAPM_CDist_PCA_Loadings_Output_1 %>% 
  rownames_to_column() # Making row names thier own column
SAPM_CDist_PCA_Loadings_Output_2 

# Getting original variable (e.g., mean flow) w/ high loadings for each PC
SAPM_CDist_PCA_Loadings_Output <- 
  SAPM_CDist_PCA_Loadings_Output_2 %>% 
  pivot_longer(cols = c(RC1, RC2, RC3),
               names_to = "temp") %>% # Switching to long format
  arrange(desc(value)) %>% # Sorting to get top variable for each PC
  slice(1:3) %>% # Selecting the the top variable for each PC
  mutate(Original_Variable = rowname,
         Original_Var_Link = case_when(rowname == "SAC_Flow_Timing" ~ "Flow Timing",
                                       rowname == "SAC_CDist_Mean_WT" ~ "Water Temp.",
                                       rowname == "Mean_SAC_Discharge" ~ "Mean Discharge"),
         Variable = case_when(temp == "RC1" ~ "CDist_RC1",
                              temp == "RC2" ~ "CDist_RC2",
                              temp == "RC3" ~ "CDist_RC3")) %>% 
  select(Original_Var_Link, Original_Variable, Variable, value)
SAPM_CDist_PCA_Loadings_Output

SAPM_CDist_Abun_Weights_Wide <- 
  pivot_wider(SAPM_CDist_Weights, 
              names_from = Region,
              values_from = Weights_Sum) %>% 
  left_join(SAPM_CDist_PCA_Loadings_Output, by = "Variable") %>% 
  rename(PC = Variable,
         Loadings = value) %>% 
  select("Species",
         "Original_Var_Link",
         "Original_Variable",
         "PC",
         "CDist",
         "Loadings")
SAPM_CDist_Abun_Weights_Wide

# Final CDist Weights Table for original input variables 
SAPM_CDist_Weights_Final <- 
  SAPM_CDist_Abun_Weights_Wide %>% 
  select("Species", "Original_Var_Link", "CDist")
SAPM_CDist_Weights_Final

# Weights output w/ RCs
SAPM_CDist_Weights_RC <- 
  SAPM_CDist_Abun_Weights_Wide %>% 
  mutate(RC = case_when(grepl("RC1", PC) ~ "RC1",
                        grepl("RC2", PC) ~ "RC2",
                        grepl("RC3", PC) ~ "RC3")) %>% 
  select("Species", "RC", "CDist")
SAPM_CDist_Weights_RC


# iii. Combined Dredge -----------------------------------------------------

# Dredge output taken from i. Dredge Output above
SAPM_CDist_Dredge_Output_Final

# Loadings taken from ii. Weights Table Output above
SAPM_CDist_PCA_Loadings_Output

# CDist loadings
# Getting original variable w/ highest loadings for each PC
SAPM_CDist_RC1 <- 
  SAPM_CDist_PCA_Loadings_Output %>% 
  filter(Variable == "CDist_RC1") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SAPM_CDist_RC1

SAPM_CDist_RC2 <- 
  SAPM_CDist_PCA_Loadings_Output %>% 
  filter(Variable == "CDist_RC2") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SAPM_CDist_RC2

SAPM_CDist_RC3 <- 
  SAPM_CDist_PCA_Loadings_Output %>% 
  filter(Variable == "CDist_RC3") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SAPM_CDist_RC3

# CDist Models
# New object w/ PCs changed to original variables
SAPM_CDist_Dredge_Original_Var_Final <- 
  SAPM_CDist_Dredge_Output_Final %>% 
  mutate(Region = "CDist")

# Renaming PC columns w/ original variable names
names(SAPM_CDist_Dredge_Original_Var_Final)[names(SAPM_CDist_Dredge_Original_Var_Final)=="CDist_RC1"] <- SAPM_CDist_RC1  

names(SAPM_CDist_Dredge_Original_Var_Final)[names(SAPM_CDist_Dredge_Original_Var_Final)=="CDist_RC2"] <- SAPM_CDist_RC2  

names(SAPM_CDist_Dredge_Original_Var_Final)[names(SAPM_CDist_Dredge_Original_Var_Final)=="CDist_RC3"] <- SAPM_CDist_RC3  

head(SAPM_CDist_Dredge_Original_Var_Final)

# Combining abundance dredge output w/ CDist output
SAPM_Dredge_Output_Final <- 
  SAPM_Dredge_Abun_Original_Var_Combined %>% 
  bind_rows(SAPM_CDist_Dredge_Original_Var_Final)
SAPM_Dredge_Output_Final

write.csv(SAPM_Dredge_Output_Final, 
          "Tables/SAPM_Dredge_Output_Final.csv", 
          row.names = FALSE)

# SPLT --------------------------------------------------------------------

head(SPLT_Model_Data_Final)

# Visualizing relationships between dist and covariates 
pairs(SPLT_Model_Data_Final[,c(5,15:17)], pch = 16)

SPLT_SAC_Dist_Global_lm <- 
  lm(log(CDist_SAC) ~ CDist_RC1 + CDist_RC2 + CDist_RC3, 
     data = SPLT_Model_Data_Final, 
     na.action = "na.fail")
summary(SPLT_SAC_Dist_Global_lm)

# Model assumptions
plot(SPLT_SAC_Dist_Global_lm) # Normality is questionable
acf(SPLT_SAC_Dist_Global_lm$residuals) # Autocorreltion is fine
vif(SPLT_SAC_Dist_Global_lm) # All < 2

## All subset selection
SPLT_SAC_Dist_Global_Dredge <- 
  dredge(SPLT_SAC_Dist_Global_lm,
         extra = list("R^2", "Dredge_Function" = function(x) {
           s <- summary(x)
           c(adjRsq = s$adj.r.squared)
                                       }))
SPLT_SAC_Dist_Global_Dredge
# Top model includes discharge

importance(SPLT_SAC_Dist_Global_Dredge)

### Getting model output and weights
SPLT_CDist_Dredge_Output <- 
  as.data.frame(cbind(round(SPLT_SAC_Dist_Global_Dredge$`(Intercept)`, 2),
                      round(SPLT_SAC_Dist_Global_Dredge$CDist_RC1, 2), 
                      round(SPLT_SAC_Dist_Global_Dredge$CDist_RC2, 3),
                      round(SPLT_SAC_Dist_Global_Dredge$CDist_RC3, 2),
                      SPLT_SAC_Dist_Global_Dredge$df,
                      round(SPLT_SAC_Dist_Global_Dredge$`R^2`, 2),
                      round(SPLT_SAC_Dist_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SPLT_SAC_Dist_Global_Dredge$logLik, 2), 
                      round(SPLT_SAC_Dist_Global_Dredge$AICc, 2),
                      round(SPLT_SAC_Dist_Global_Dredge$delta, 2), 
                      SPLT_SAC_Dist_Global_Dredge$weight)) %>% 
  mutate(Species = "SPLT")
colnames(SPLT_CDist_Dredge_Output) = c("Intercept", "CDist_RC1", "CDist_RC2",
                                       "CDist_RC3", "DF", "R^2", "R^2_Adj",
                                       "logLik","AICc", "Delta", "Weights",
                                       "Species")
SPLT_CDist_Dredge_Output

SPLT_CDist_RC2_Weights <- 
  SPLT_CDist_Dredge_Output %>% 
  filter(!(is.na(CDist_RC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "CDist", Variable = "CDist_RC2")

SPLT_CDist_RC3_Weights <- 
  SPLT_CDist_Dredge_Output %>% 
  filter(!(is.na(CDist_RC3))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "CDist", Variable = "CDist_RC3")

SPLT_CDist_Weights <- 
  SPLT_CDist_Dredge_Output %>% 
  filter(!(is.na(CDist_RC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "CDist", Variable = "CDist_RC1") %>% 
  bind_rows(SPLT_CDist_RC2_Weights, SPLT_CDist_RC3_Weights)
SPLT_CDist_Weights

# Checking weights DF
importance(SPLT_SAC_Dist_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set
SPLT_CDist_MA = model.avg(SPLT_SAC_Dist_Global_Dredge,
                              cumsum(weight) <= .95, 
                              rank = "AIC")
SPLT_CDist_MA_Sum = summary(SPLT_CDist_MA)
SPLT_CDist_MA_Full_Out = SPLT_CDist_MA_Sum$coefmat.full
SPLT_CDist_MA_Full_Out


# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SPLT_CDist_Dredge_Output_Final <- 
  SPLT_CDist_Dredge_Output %>% 
  replace_na(list(CDist_RC1 = "", CDist_RC2 = "", CDist_RC3 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SPLT_CDist_Dredge_Output_Final

write.csv(SPLT_CDist_Dredge_Output_Final, 
          "Tables/SPLT_CDist_Dredge_Abun_Output.csv", 
          row.names = FALSE)


# ii. Weights Table Output --------------------------------------------------

# Weights table output

# Reorder loadings matrix
col.order <- c("RC1","RC2","RC3")

# Loadings
SPLT_CDist_PCA_Loadings_Output_1 <- 
  as.data.frame(SPLT_CDist_PCA_Loadings[,col.order]) # Reordering columns
SPLT_CDist_PCA_Loadings_Output_1

SPLT_CDist_PCA_Loadings_Output_2 <- 
  SPLT_CDist_PCA_Loadings_Output_1 %>% 
  rownames_to_column() # Making row names thier own column
SPLT_CDist_PCA_Loadings_Output_2 

# Getting original variable (e.g., mean flow) w/ high loadings for each PC
SPLT_CDist_PCA_Loadings_Output <- 
  SPLT_CDist_PCA_Loadings_Output_2 %>% 
  pivot_longer(cols = c(RC1, RC2, RC3),
               names_to = "temp") %>% # Switching to long format
  arrange(desc(value)) %>% # Sorting to get top variable for each PC
  slice(1:3) %>% # Selecting the the top variable for each PC
  mutate(Original_Variable = rowname,
         Original_Var_Link = case_when(rowname == "SAC_Flow_Timing" ~ "Flow Timing",
                                       rowname == "SAC_CDist_Mean_WT" ~ "Water Temp.",
                                       rowname == "Mean_SAC_Discharge" ~ "Mean Discharge"),
         Variable = case_when(temp == "RC1" ~ "CDist_RC1",
                              temp == "RC2" ~ "CDist_RC2",
                              temp == "RC3" ~ "CDist_RC3")) %>% 
  select(Original_Var_Link, Original_Variable, Variable, value)
SPLT_CDist_PCA_Loadings_Output

# Weights output
SPLT_CDist_Abun_Weights_Wide <- 
  pivot_wider(SPLT_CDist_Weights, 
              names_from = Region,
              values_from = Weights_Sum) %>% 
  left_join(SPLT_CDist_PCA_Loadings_Output, by = "Variable") %>% 
  rename(PC = Variable,
         Loadings = value) %>% 
  select("Species",
         "Original_Var_Link",
         "Original_Variable",
         "PC",
         "CDist",
         "Loadings")
SPLT_CDist_Abun_Weights_Wide

# Final CDist Weights Table
SPLT_CDist_Weights_Final <- 
  SPLT_CDist_Abun_Weights_Wide %>% 
  select("Species", "Original_Var_Link", "CDist")
SPLT_CDist_Weights_Final

# Weights output w/ RCs
SPLT_CDist_Weights_RC <- 
  SPLT_CDist_Abun_Weights_Wide %>% 
  mutate(RC = case_when(grepl("RC1", PC) ~ "RC1",
                        grepl("RC2", PC) ~ "RC2",
                        grepl("RC3", PC) ~ "RC3")) %>% 
  select("Species", "RC", "CDist")
SPLT_CDist_Weights_RC


# iii. Combined Dredge -----------------------------------------------------

# Dredge output taken from i. Dredge Output above
SPLT_CDist_Dredge_Output_Final

# Loadings taken from ii. Weights Table Output above
SPLT_CDist_PCA_Loadings_Output

# CDist loadings
# Getting original variable w/ highest loadings for each PC
SPLT_CDist_RC1 <- 
  SPLT_CDist_PCA_Loadings_Output %>% 
  filter(Variable == "CDist_RC1") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SPLT_CDist_RC1

SPLT_CDist_RC2 <- 
  SPLT_CDist_PCA_Loadings_Output %>% 
  filter(Variable == "CDist_RC2") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SPLT_CDist_RC2

SPLT_CDist_RC3 <- 
  SPLT_CDist_PCA_Loadings_Output %>% 
  filter(Variable == "CDist_RC3") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SPLT_CDist_RC3

# CDist Models
# New object w/ PCs changed to original variables
SPLT_CDist_Dredge_Original_Var_Final <- 
  SPLT_CDist_Dredge_Output_Final %>% 
  mutate(Region = "CDist")

# Renaming PC columns w/ original variable names
names(SPLT_CDist_Dredge_Original_Var_Final)[names(SPLT_CDist_Dredge_Original_Var_Final)=="CDist_RC1"] <- SPLT_CDist_RC1  

names(SPLT_CDist_Dredge_Original_Var_Final)[names(SPLT_CDist_Dredge_Original_Var_Final)=="CDist_RC2"] <- SPLT_CDist_RC2  

names(SPLT_CDist_Dredge_Original_Var_Final)[names(SPLT_CDist_Dredge_Original_Var_Final)=="CDist_RC3"] <- SPLT_CDist_RC3  

head(SPLT_CDist_Dredge_Original_Var_Final)

# Combining abundance dredge output w/ CDist output
SPLT_Dredge_Output_Final <- 
  SPLT_Dredge_Abun_Original_Var_Combined %>% 
  bind_rows(SPLT_CDist_Dredge_Original_Var_Final)
SPLT_Dredge_Output_Final

write.csv(SPLT_Dredge_Output_Final, 
          "Tables/SPLT_Dredge_Output_Final.csv", 
          row.names = FALSE)

# SASU --------------------------------------------------------------------

head(SASU_Model_Data_Final) # Distribution data

# Visualizing relationships between dist and covariates 
pairs(SASU_Model_Data_Final[,c(5,15:17)], pch = 16)

SASU_SAC_Dist_Global_lm <- 
  lm(log(CDist_SAC) ~ CDist_RC1 + CDist_RC2 + CDist_RC3, 
     data = SASU_Model_Data_Final, 
     na.action = "na.fail")
summary(SASU_SAC_Dist_Global_lm)

# Model assumptions
plot(SASU_SAC_Dist_Global_lm) # Normality is questionable
acf(SASU_SAC_Dist_Global_lm$residuals) # Autocorreltion is fine
vif(SASU_SAC_Dist_Global_lm) # All < 2

## All subset selection
SASU_SAC_Dist_Global_Dredge <- 
  dredge(SASU_SAC_Dist_Global_lm,
         extra = list("R^2", "Dredge_Function" = function(x) {
           s <- summary(x)
           c(adjRsq = s$adj.r.squared)
         }))
SASU_SAC_Dist_Global_Dredge
# Top model is null model

importance(SASU_SAC_Dist_Global_Dredge)
# All variables < 0.46

### Getting model output and weights
SASU_CDist_Dredge_Output <- 
  as.data.frame(cbind(round(SASU_SAC_Dist_Global_Dredge$`(Intercept)`, 2),
                      round(SASU_SAC_Dist_Global_Dredge$CDist_RC1, 2), 
                      round(SASU_SAC_Dist_Global_Dredge$CDist_RC2, 2),
                      round(SASU_SAC_Dist_Global_Dredge$CDist_RC3, 2),
                      SASU_SAC_Dist_Global_Dredge$df,
                      round(SASU_SAC_Dist_Global_Dredge$`R^2`, 2),
                      round(SASU_SAC_Dist_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SASU_SAC_Dist_Global_Dredge$logLik, 2), 
                      round(SASU_SAC_Dist_Global_Dredge$AICc, 2),
                      round(SASU_SAC_Dist_Global_Dredge$delta, 2), 
                      SASU_SAC_Dist_Global_Dredge$weight)) %>% 
  mutate(Species = "SASU")
colnames(SASU_CDist_Dredge_Output) = c("Intercept", "CDist_RC1", "CDist_RC2",
                                       "CDist_RC3", "DF", "R^2", "R^2_Adj",
                                       "logLik","AICc", "Delta", "Weights",
                                       "Species")
SASU_CDist_Dredge_Output

SASU_CDist_RC2_Weights <- 
  SASU_CDist_Dredge_Output %>% 
  filter(!(is.na(CDist_RC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "CDist", Variable = "CDist_RC2")

SASU_CDist_RC3_Weights <- 
  SASU_CDist_Dredge_Output %>% 
  filter(!(is.na(CDist_RC3))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "CDist", Variable = "CDist_RC3")

SASU_CDist_Weights <- 
  SASU_CDist_Dredge_Output %>% 
  filter(!(is.na(CDist_RC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "CDist", Variable = "CDist_RC1") %>% 
  bind_rows(SASU_CDist_RC2_Weights, SASU_CDist_RC3_Weights)
SASU_CDist_Weights

# Checking weights DF
importance(SASU_SAC_Dist_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set
SASU_CDist_MA = model.avg(SASU_SAC_Dist_Global_Dredge,
                          cumsum(weight) <= .95, 
                          rank = "AIC")
SASU_CDist_MA_Sum = summary(SASU_CDist_MA)
SASU_CDist_MA_Full_Out = SASU_CDist_MA_Sum$coefmat.full
SASU_CDist_MA_Full_Out

# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SASU_CDist_Dredge_Output_Final <- 
  SASU_CDist_Dredge_Output %>% 
  replace_na(list(CDist_RC1 = "", CDist_RC2 = "", CDist_RC3 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SASU_CDist_Dredge_Output_Final

write.csv(SASU_CDist_Dredge_Output_Final, 
          "Tables/SASU_CDist_Dredge_Abun_Output.csv", 
          row.names = FALSE)

# ii. Weights Table Output --------------------------------------------------

# Weights table output

# Reorder loadings matrix
col.order <- c("RC1","RC2","RC3")

# Loadings
SASU_CDist_PCA_Loadings_Output_1 <- 
  as.data.frame(SASU_CDist_PCA_Loadings[,col.order]) # Reordering columns
SASU_CDist_PCA_Loadings_Output_1

SASU_CDist_PCA_Loadings_Output_2 <- 
  SASU_CDist_PCA_Loadings_Output_1 %>% 
  rownames_to_column() # Making row names thier own column
SASU_CDist_PCA_Loadings_Output_2 

# Getting original variable (e.g., mean flow) w/ high loadings for each PC
SASU_CDist_PCA_Loadings_Output <- 
  SASU_CDist_PCA_Loadings_Output_2 %>% 
  pivot_longer(cols = c(RC1, RC2, RC3),
               names_to = "temp") %>% # Switching to long format
  arrange(desc(value)) %>% # Sorting to get top variable for each PC
  slice(1:3) %>% # Selecting the the top variable for each PC
  mutate(Original_Variable = rowname,
         Original_Var_Link = case_when(rowname == "SAC_Flow_Timing" ~ "Flow Timing",
                                       rowname == "SAC_CDist_Mean_WT" ~ "Water Temp.",
                                       rowname == "Mean_SAC_Discharge" ~ "Mean Discharge"),
         Variable = case_when(temp == "RC1" ~ "CDist_RC1",
                              temp == "RC2" ~ "CDist_RC2",
                              temp == "RC3" ~ "CDist_RC3")) %>% 
  select(Original_Var_Link, Original_Variable, Variable, value)
SASU_CDist_PCA_Loadings_Output

# Weights output
SASU_CDist_Abun_Weights_Wide <- 
  pivot_wider(SASU_CDist_Weights, 
              names_from = Region,
              values_from = Weights_Sum) %>% 
  left_join(SASU_CDist_PCA_Loadings_Output, by = "Variable") %>% 
  rename(PC = Variable,
         Loadings = value) %>% 
  select("Species",
         "Original_Var_Link",
         "Original_Variable",
         "PC",
         "CDist",
         "Loadings")
SASU_CDist_Abun_Weights_Wide

# Final CDist Weights Table
SASU_CDist_Weights_Final <- 
  SASU_CDist_Abun_Weights_Wide %>% 
  select("Species", "Original_Var_Link", "CDist")
SASU_CDist_Weights_Final

# Weights output w/ RCs
SASU_CDist_Weights_RC <- 
  SASU_CDist_Abun_Weights_Wide %>% 
  mutate(RC = case_when(grepl("RC1", PC) ~ "RC1",
                        grepl("RC2", PC) ~ "RC2",
                        grepl("RC3", PC) ~ "RC3")) %>% 
  select("Species", "RC", "CDist")
SASU_CDist_Weights_RC

# iii. Combined Dredge -----------------------------------------------------

# Dredge output taken from i. Dredge Output above
SASU_CDist_Dredge_Output_Final

# Loadings taken from ii. Weights Table Output above
SASU_CDist_PCA_Loadings_Output

# CDist loadings
# Getting original variable w/ highest loadings for each PC
SASU_CDist_RC1 <- 
  SASU_CDist_PCA_Loadings_Output %>% 
  filter(Variable == "CDist_RC1") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SASU_CDist_RC1

SASU_CDist_RC2 <- 
  SASU_CDist_PCA_Loadings_Output %>% 
  filter(Variable == "CDist_RC2") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SASU_CDist_RC2

SASU_CDist_RC3 <- 
  SASU_CDist_PCA_Loadings_Output %>% 
  filter(Variable == "CDist_RC3") %>% 
  select(Original_Var_Link) %>% 
  as.data.frame()
SASU_CDist_RC3

# CDist Models
# New object w/ PCs changed to original variables
SASU_CDist_Dredge_Original_Var_Final <- 
  SASU_CDist_Dredge_Output_Final %>% 
  mutate(Region = "CDist")

# Renaming PC columns w/ original variable names
names(SASU_CDist_Dredge_Original_Var_Final)[names(SASU_CDist_Dredge_Original_Var_Final)=="CDist_RC1"] <- SASU_CDist_RC1  

names(SASU_CDist_Dredge_Original_Var_Final)[names(SASU_CDist_Dredge_Original_Var_Final)=="CDist_RC2"] <- SASU_CDist_RC2  

names(SASU_CDist_Dredge_Original_Var_Final)[names(SASU_CDist_Dredge_Original_Var_Final)=="CDist_RC3"] <- SASU_CDist_RC3  

head(SASU_CDist_Dredge_Original_Var_Final)

# Combining abundance dredge output w/ CDist output
SASU_Dredge_Output_Final <- 
  SASU_Dredge_Abun_Original_Var_Combined %>% 
  bind_rows(SASU_CDist_Dredge_Original_Var_Final)
SASU_Dredge_Output_Final

write.csv(SASU_Dredge_Output_Final, 
          "Tables/SASU_Dredge_Output_Final.csv", 
          row.names = FALSE)

# ***Species Combined Weights Output***----------------------------------------------

# Original variables listed
All_Sp_CDist_Weights_Output <- 
  bind_rows(SAPM_CDist_Weights_Final,
            SPLT_CDist_Weights_Final, 
            SASU_CDist_Weights_Final) %>% 
  rename("Covariates" = "Original_Var_Link",
         "Center of Distribution" = "CDist")
All_Sp_CDist_Weights_Output

write.csv(All_Sp_CDist_Weights_Output, 
          "Tables/CDistdance_Models_Weights.csv", 
          row.names = FALSE)

# Combining CDist w/ Abun weights to create one single table

All_Sp_All_Weights_Output <- 
  All_Sp_Abun_Weights_Output %>% 
  left_join(All_Sp_CDist_Weights_Output,  by = c("Covariates", "Species"))
All_Sp_All_Weights_Output

# Final table that includes weights for all models (Abundance + Distribution)
write.csv(All_Sp_All_Weights_Output, 
          "Tables/All_Sp_All_Weights_Output.csv", 
          row.names = FALSE)

# Output with RCs listed
SASU_CDist_Weights_RC

SASU_Abun_Weights_RC_Final

All_Sp_CDist_Weights_RC_Output <- 
  bind_rows(SAPM_CDist_Weights_RC,
            SPLT_CDist_Weights_RC, 
            SASU_CDist_Weights_RC) %>% 
  rename("Center of Distribution" = "CDist")
All_Sp_CDist_Weights_RC_Output

# Combining CDist w/ Abun weights to create one single table
All_Sp_All_Weights_RC_Output <- 
  All_Sp_Abun_Weights_RC_Output %>% 
  left_join(All_Sp_CDist_Weights_RC_Output,  by = c("RC", "Species"))
All_Sp_All_Weights_RC_Output

# Final table that includes weights for all models (Abundance + Distribution)
write.csv(All_Sp_All_Weights_RC_Output, 
          "Tables/All_Sp_All_Weights_RC_Output.csv", 
          row.names = FALSE)

# ***Figures*** -----------------------------------------------------------------


# i. Abundance ------------------------------------------------------------


# SAPM --------------------------------------------------------------------

head(SAPM_Index_Final)

SAPM_Index_Final_Long <- 
  pivot_longer(SAPM_Index_Final,
               cols = c(Delta_Index, Sac_River_Index, San_Joaquin_River_Index),
               names_to = "Regions") %>% 
  select(Year, Regions, value)

SAPM_Abun_GGPLOT <- 
  ggplot(data= SAPM_Index_Final_Long, aes(x=Year, y=value, fill = Regions)) +
  geom_bar(position="stack", stat = "identity", color = "black") +
  scale_fill_manual(values = c("White", "Grey", "Black"),
                    labels = c("Delta", "Sacramento", "San Joaquin")) +
  labs(x = "", y = "") +
  ggtitle("(A) Sacramento Pikeminnow") +
  scale_x_discrete(expand = c(0, 0), breaks = seq(1995, 2019, by = 2)) +
  scale_y_continuous(limits = c(0, 2), expand = c(0, 0)) +
  theme_classic(base_size = 24) +
  theme(axis.text = element_text(colour = "black")) +
  theme(axis.line = element_line(colour = "black", size = .65)) +
  theme(axis.ticks.y = element_line(size = .65)) +
  theme(text = element_text(color = "black", family = "Times")) +
  theme(legend.position="")
SAPM_Abun_GGPLOT


# SPLT --------------------------------------------------------------------

head(SPLT_Index_Final)

SPLT_Index_Final_Long <- 
  pivot_longer(SPLT_Index_Final,
               cols = c(Delta_Index, Sac_River_Index, San_Joaquin_River_Index),
               names_to = "Regions") %>% 
  select(Year, Regions, value)

SPLT_Abun_GGPLOT <- 
  ggplot(data= SPLT_Index_Final_Long, aes(x=Year, y=value, fill = Regions)) +
  geom_bar(position="stack", stat = "identity", color = "black") +
  scale_fill_manual(values = c("White", "Grey", "Black"),
                    labels = c("Delta", "Sacramento", "San Joaquin")) +
  labs(x = "", y = "Abundance Index") +
  ggtitle("(B) Sacramento Splittail") +
  scale_x_discrete(expand = c(0, 0), breaks = seq(1995, 2019, by = 2)) +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
  theme_classic(base_size = 24) +
  theme(axis.text = element_text(colour = "black")) +
  theme(axis.line = element_line(colour = "black", size = .65)) +
  theme(axis.ticks.y = element_line(size = .65)) +
  theme(text = element_text(color = "black", family = "Times")) 
SPLT_Abun_GGPLOT

# SASU --------------------------------------------------------------------

head(SASU_Index_Final)

SASU_Index_Final_Long <- 
  pivot_longer(SASU_Index_Final,
               cols = c(Delta_Index, Sac_River_Index, San_Joaquin_River_Index),
               names_to = "Regions") %>% 
  select(Year, Regions, value)

SASU_Abun_GGPLOT <- 
  ggplot(data= SASU_Index_Final_Long, aes(x=Year, y=value, fill = Regions)) +
  geom_bar(position="stack", stat = "identity", color = "black") +
  scale_fill_manual(values = c("White", "Grey", "Black"),
                    labels = c("Delta", "Sacramento", "San Joaquin")) +
  labs(x = "Year", y = "") +
  scale_x_discrete(expand = c(0, 0), breaks = seq(1995, 2019, by = 2)) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0), 
                     breaks = seq(0, 10, by = 2)) +
  theme_classic(base_size = 24) +
  ggtitle("(C) Sacramento Sucker") +
  theme(axis.text = element_text(colour = "black")) +
  theme(axis.line = element_line(colour = "black", size = .65)) +
  theme(axis.ticks.y = element_line(size = .65)) +
  theme(text = element_text(color = "black", family = "Times")) +
  theme(legend.position="")
SASU_Abun_GGPLOT


# Final Abun Figure -------------------------------------------------------

pdf("Figures/Sp_Abun_Trends.pdf", width = 12, height = 15)
SAPM_Abun_GGPLOT/SPLT_Abun_GGPLOT/SASU_Abun_GGPLOT
dev.off()


# Station Codes for Map ---------------------------------------------------

# I checked the stations for each species and they were the same
# The SAPM_Index data is what was used to esitmate the index and only includes the months used for the  index
Station_Codes = unique(SAPM_Index$StationCode)

Station_Codes_Final <- 
  CDFW_Subreagions %>% 
  filter(StationCode %in% Station_Codes,
         subarea != 8)
head(Station_Codes_Final)

write.csv(Station_Codes_Final, "Data/Station_Codes_Final.csv",
          row.names = FALSE)

# Disconnect from DB ------------------------------------------------------

DBI::dbDisconnect(conn=DJFMP_Database_Con)
