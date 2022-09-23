# Background --------------------------------------------------------------

# V_9 uses non-rotated PCs for the final analyses

# V_8 Gets rid of the DB code that takes a little while to run

# V_7 Updates DB code, removes subregion from index and environmental data calculations, removed FP inundation code, and removed WY flow code from scaled covariate dataframe 


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
  mutate(DV = 1) # Used to create sample count of stations below
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
  filter(OrganismCode == "SAPM") %>%  # Only keeping catch for SAPM
  mutate(OrganismCode = as.character(OrganismCode))
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
  replace_na(list(OrganismCode = "")) %>% 
  mutate(OrganismCode = as.factor(OrganismCode))
head(SAPM_Data)
tail(SAPM_Data)
str(SAPM_Data)

# SPLT Data ---------------------------------------------------------------

Catch_Table_SPLT <- 
  Catch_Table %>% 
  select(CatchID, SampleID, OrganismCode, ForkLength, CatchCount) %>% 
  filter(OrganismCode == "SPLT") %>%  # Only keeping catch for SPLT
  mutate(OrganismCode = as.character(OrganismCode))
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
  replace_na(list(OrganismCode = "")) %>% 
  mutate(OrganismCode = as.factor(OrganismCode))
head(SPLT_Data)
tail(SPLT_Data)
str(SPLT_Data)

# SASU Data ---------------------------------------------------------------

Catch_Table_SASU <- 
  Catch_Table %>% 
  select(CatchID, SampleID, OrganismCode, ForkLength, CatchCount) %>% 
  filter(OrganismCode == "SASU") %>%  # Only keeping catch for SASU
  mutate(OrganismCode = as.character(OrganismCode))
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
  replace_na(list(OrganismCode = "")) %>% 
  mutate(OrganismCode = as.factor(OrganismCode))
head(SASU_Data)
tail(SASU_Data)
str(SASU_Data)

# ***Length Frequency Analyses -----------------------------------------------

## Length Frequency: Staring with length frequency analyses/histograms to determine what months and length cutoffs to use when creating age-0 abundance indices 
## See previous versions of code for the length frequency analyses


#***Plus Count Proportions --------------------------------------------------

# I will be using FL of Age-0 fishes caught within a seine haul to proporiton FL of plus count fish

# Adjusted Count =  Total_Count * (FL_Range_Count/FL_Count)

# Total_count = All fish in one sample including plus count fish
# FL_Range_Count = Count of fish in the FL range that we are using
# FL_Count = Count of measured fish

# CPUE = Adjusted Count/Seine Volume


# SAPM  -------------------------------------------------------------------

# Based on length freq histograms, age 0 fish don't start showing up until June and July

# June cutoff for age-0 fish is ~ 60 mm
# July cutoff for age-0 fish is ~ 70 mm

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
head(SAPM_Index) 

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

##### Subarea 8 is no longer being sampled...removing subarea 8 to see if it influences results of Sac Models
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

SAPM_Index_Final <- 
  SAPM_Index_Final[,c(1,5:8)]
head(SAPM_Index_Final)

write.csv(SAPM_Index_Final, "Output/SAPM_Index_Final.csv", row.names = FALSE)

# SPLT --------------------------------------------------------------------

# Subarea 8 is no longer being sampled...removing subarea 8 
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

SPLT_Index_Final <- 
  SPLT_Index_Final[,c(1,5:8)]
head(SPLT_Index_Final)

write.csv(SPLT_Index_Final, "Output/SPLT_Index_Final.csv", row.names = FALSE)


# SASU --------------------------------------------------------------------

# Subarea 8 is no longer being sampled...removing subarea 8 
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


SASU_Index_Final <- 
  SASU_Index_Final[,c(1,5:8)]
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
  filter(CPUE != 0, # Removing records where CPUE was 0
         Month != 12) # Removing records from December
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
         subarea != 8,
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
         subarea != 8,
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
# tau = 0.136, 2-sided pvalue =0.3966

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

# Because IEP gets criticism for only using flow and water temp is correlated with flow, I am going to use PCA to get uncor. covariates. I'm not worried about overfitting so I will not be reducing the number of variables.

# Will not be rotating and will take the top 2 PCs

# SAPM --------------------------------------------------------------------

# Final index data
head(SAPM_Index_Final)

# Final scaled covariate data
head(SAPM_Covariates_Scaled)
cor(SAPM_Covariates_Scaled)

SAPM_Model_Data <- 
  as.data.frame(cbind(SAPM_Index_Final[,2:5], 
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

# Loadings
SAPM_SAC_PCA_Loadings_No_Rot_1 <- 
  SAPM_SAC_PCA_No_Rot$loadings

SAPM_SAC_PCA_Loadings_No_Rot <- 
  round(SAPM_SAC_PCA_Loadings_No_Rot_1[,], 2) 
SAPM_SAC_PCA_Loadings_No_Rot

# Scores
SAPM_SAC_PC_Scores <- 
  as.data.frame(SAPM_SAC_PCA_No_Rot$scores)

colnames(SAPM_SAC_PC_Scores) <- 
  c("SAC_PC1", 
    "SAC_PC2", 
    "SAC_PC3")
SAPM_SAC_PC_Scores



# SJ -> Dot fitting a model for this region/species



# Delta
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

# Loadings
SAPM_DELTA_PCA_Loadings_No_Rot_1 <- 
  SAPM_DELTA_PCA_No_Rot$loadings

SAPM_DELTA_PCA_Loadings_No_Rot <- 
  round(SAPM_DELTA_PCA_Loadings_No_Rot_1[,], 2)
SAPM_DELTA_PCA_Loadings_No_Rot


# Getting scores
SAPM_DELTA_PC_Scores <- 
  as.data.frame(SAPM_DELTA_PCA_No_Rot$scores)

colnames(SAPM_DELTA_PC_Scores) <- 
  c("DELTA_PC1", 
    "DELTA_PC2", 
    "DELTA_PC3")
SAPM_DELTA_PC_Scores

# Watershed Level
# Low index in SJ so going to leave those variables out for now...
SAPM_Model_Data_WS <- 
  SAPM_Model_Data %>% 
  select(Mean_SAC_Discharge, SAC_Flow_Timing, SAC_Mean_WT,
         Mean_DELTA_Discharge, DELTA_Flow_Timing, DELTA_Mean_WT)
head(SAPM_Model_Data_WS)
cor(SAPM_Model_Data_WS)

SAPM_Model_Data_WS_Mat <- 
  SAPM_Model_Data_WS %>% 
  as.matrix()
head(SAPM_Model_Data_WS_Mat)

SAPM_WS_PCA_No_Rot <- 
  psych::principal(SAPM_Model_Data_WS_Mat, 
                   nfactors = 6, 
                   rotate = "none")
SAPM_WS_PCA_No_Rot

# Loadings
SAPM_WS_PCA_Loadings_No_Rot_1 <- 
  SAPM_WS_PCA_No_Rot$loadings

SAPM_WS_PCA_Loadings_No_Rot <- 
  round(SAPM_WS_PCA_Loadings_No_Rot_1[,], 2)
SAPM_WS_PCA_Loadings_No_Rot


# Getting scores
SAPM_WS_PC_Scores <- 
  as.data.frame(SAPM_WS_PCA_No_Rot$scores[,1:3])

colnames(SAPM_WS_PC_Scores) <- 
  c("WS_PC1", 
    "WS_PC2", 
    "WS_PC3")
SAPM_WS_PC_Scores

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
SAPM_CDist_PCA_No_Rot <- 
  psych::principal(SAPM_Model_Data_CDist_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SAPM_CDist_PCA_No_Rot

# Loadings
SAPM_CDist_PCA_Loadings_No_Rot_1 <- 
  SAPM_CDist_PCA_No_Rot$loadings

SAPM_CDist_PCA_Loadings_No_Rot <- 
  round(SAPM_CDist_PCA_Loadings_No_Rot_1[,], 2)
SAPM_CDist_PCA_Loadings_No_Rot

# Scores
SAPM_CDist_PC_Scores <- 
  as.data.frame(SAPM_CDist_PCA_No_Rot$scores)
colnames(SAPM_CDist_PC_Scores) <- 
  c("CDist_PC1", 
    "CDist_PC2", 
    "CDist_PC3")
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
            SAPM_WS_PC_Scores,
            SAPM_CDist_PC_Scores)
head(SAPM_Model_Data_Final)

write.csv(SAPM_Model_Data_Final, "Output/SAPM_Model_Data_Final.csv", row.names = FALSE)

# Loadings output

SAPM_WS_PCA_Loadings_No_Rot_Out <- 
  as.data.frame(SAPM_WS_PCA_Loadings_No_Rot) %>% 
  mutate(Region = "Watershed",
         Var = row.names(SAPM_WS_PCA_Loadings_No_Rot)) %>% 
  select("Var","PC1", "PC2", "Region")
head(SAPM_WS_PCA_Loadings_No_Rot_Out)

SAPM_SAC_PCA_Loadings_No_Rot_Out <- 
  as.data.frame(SAPM_SAC_PCA_Loadings_No_Rot) %>% 
  mutate(Region = "Sacramento",
         Var = row.names(SAPM_SAC_PCA_Loadings_No_Rot))%>% 
  select("Var", "PC1", "PC2", "Region")
head(SAPM_SAC_PCA_Loadings_No_Rot_Out)

SAPM_DELTA_PCA_Loadings_No_Rot_Out <- 
  as.data.frame(SAPM_DELTA_PCA_Loadings_No_Rot) %>% 
  mutate(Region = "DELTA",
         Var = row.names(SAPM_DELTA_PCA_Loadings_No_Rot))%>% 
  select("Var", "PC1", "PC2", "Region")
head(SAPM_DELTA_PCA_Loadings_No_Rot_Out)

SAPM_CDist_PCA_Loadings_No_Rot_Out <- 
  as.data.frame(SAPM_CDist_PCA_Loadings_No_Rot) %>% 
  mutate(Region = "CDist_SAC",
         Var = row.names(SAPM_CDist_PCA_Loadings_No_Rot))%>% 
  select("Var", "PC1", "PC2", "Region")
head(SAPM_CDist_PCA_Loadings_No_Rot_Out)

SAPM_Loadings_Final <- 
  rbind(SAPM_WS_PCA_Loadings_No_Rot_Out,
        SAPM_SAC_PCA_Loadings_No_Rot_Out,
        SAPM_DELTA_PCA_Loadings_No_Rot_Out,
        SAPM_CDist_PCA_Loadings_No_Rot_Out)
SAPM_Loadings_Final

write.csv(SAPM_Loadings_Final, "Tables/SAPM_Loadings_Final.csv", row.names = FALSE)

# SPLT --------------------------------------------------------------------

# Final index data
head(SPLT_Index_Final)

# Final scaled covariate data
head(SPLT_Covariates_Scaled)
cor(SPLT_Covariates_Scaled)

SPLT_Model_Data <- 
  cbind(SPLT_Index_Final[,2:5], 
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
SPLT_SAC_PCA_No_Rot <- 
  psych::principal(SPLT_Model_Data_SAC_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SPLT_SAC_PCA_No_Rot

# Loadings
SPLT_SAC_PCA_Loadings_No_Rot_1 <- 
  SPLT_SAC_PCA_No_Rot$loadings

SPLT_SAC_PCA_Loadings_No_Rot <- 
  round(SPLT_SAC_PCA_Loadings_No_Rot_1[,], 2)
SPLT_SAC_PCA_Loadings_No_Rot

# Scores
SPLT_SAC_PC_Scores <- 
  as.data.frame(SPLT_SAC_PCA_No_Rot$scores)
colnames(SPLT_SAC_PC_Scores) <- 
  c("SAC_PC1", 
    "SAC_PC2", 
    "SAC_PC3")
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
SPLT_SJ_PCA_No_Rot <- 
  psych::principal(SPLT_Model_Data_SJ_Mat, 
                   nfactors = 3, 
                   rotate = "none")

# Loadings
SPLT_SJ_PCA_Loadings_No_Rot_1 <- 
  SPLT_SJ_PCA_No_Rot$loadings

SPLT_SJ_PCA_Loadings_No_Rot <- 
  round(SPLT_SJ_PCA_Loadings_No_Rot_1[,], 2)
SPLT_SJ_PCA_Loadings_No_Rot

# Scores
SPLT_SJ_PC_Scores <- 
  as.data.frame(SPLT_SJ_PCA_No_Rot$scores)
colnames(SPLT_SJ_PC_Scores) <- 
  c("SJ_PC1", 
    "SJ_PC2", 
    "SJ_PC3")
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

SPLT_DELTA_PCA_No_Rot <- 
  psych::principal(SPLT_Model_Data_DELTA_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SPLT_DELTA_PCA_No_Rot

# Loadings
SPLT_DELTA_PCA_Loadings_No_Rot_1 <- 
  SPLT_DELTA_PCA_No_Rot$loadings

SPLT_DELTA_PCA_Loadings_No_Rot <- 
  round(SPLT_DELTA_PCA_Loadings_No_Rot_1[,], 2)
SPLT_DELTA_PCA_Loadings_No_Rot

# Getting scores
SPLT_DELTA_PC_Scores <- 
  as.data.frame(SPLT_DELTA_PCA_No_Rot$scores)
colnames(SPLT_DELTA_PC_Scores) <- 
  c("DELTA_PC1", 
    "DELTA_PC2", 
    "DELTA_PC3")
SPLT_DELTA_PC_Scores


# Watershed Level

SPLT_Model_Data_WS <- 
  SPLT_Model_Data %>% 
  select(Mean_SAC_Discharge, SAC_Flow_Timing, SAC_Mean_WT,
         Mean_SJ_Discharge, SJ_Flow_Timing, SJ_Mean_WT,
         Mean_DELTA_Discharge, DELTA_Flow_Timing, DELTA_Mean_WT)
head(SPLT_Model_Data_WS)
cor(SPLT_Model_Data_WS)

SPLT_Model_Data_WS_Mat <- 
  SPLT_Model_Data_WS %>% 
  as.matrix()
head(SPLT_Model_Data_WS_Mat)

SPLT_WS_PCA_No_Rot <- 
  psych::principal(SPLT_Model_Data_WS_Mat, 
                   nfactors = 9, 
                   rotate = "none")
SPLT_WS_PCA_No_Rot

# Loadings
SPLT_WS_PCA_Loadings_No_Rot_1 <- 
  SPLT_WS_PCA_No_Rot$loadings

SPLT_WS_PCA_Loadings_No_Rot <- 
  round(SPLT_WS_PCA_Loadings_No_Rot_1[,], 2)
SPLT_WS_PCA_Loadings_No_Rot


# Getting scores
SPLT_WS_PC_Scores <- 
  as.data.frame(SPLT_WS_PCA_No_Rot$scores[,1:3])

colnames(SPLT_WS_PC_Scores) <- 
  c("WS_PC1", 
    "WS_PC2", 
    "WS_PC3")
SPLT_WS_PC_Scores

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
SPLT_SAC_CDist_PCA_No_Rot <- 
  psych::principal(SPLT_Model_Data_SAC_CDist_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SPLT_SAC_CDist_PCA_No_Rot

# Loadings
SPLT_SAC_CDist_PCA_Loadings_No_Rot_1 <- 
  SPLT_SAC_CDist_PCA_No_Rot$loadings

SPLT_SAC_CDist_PCA_Loadings_No_Rot <- 
  round(SPLT_SAC_CDist_PCA_Loadings_No_Rot_1[,], 2)
SPLT_SAC_CDist_PCA_Loadings_No_Rot

# Scores
SPLT_SAC_CDist_PC_Scores <- 
  as.data.frame(SPLT_SAC_CDist_PCA_No_Rot$scores)
colnames(SPLT_SAC_CDist_PC_Scores) <- 
  c("SAC_CDist_PC1", 
    "SAC_CDist_PC2", 
    "SAC_CDist_PC3")
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
SPLT_SJ_CDist_PCA_No_Rot <- 
  psych::principal(SPLT_Model_Data_SJ_CDist_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SPLT_SJ_CDist_PCA_No_Rot

# Loadings
SPLT_SJ_CDist_PCA_Loadings_No_Rot_1 <- 
  SPLT_SJ_CDist_PCA_No_Rot$loadings

SPLT_SJ_CDist_PCA_Loadings_No_Rot <- 
  round(SPLT_SJ_CDist_PCA_Loadings_No_Rot_1[,], 2)
SPLT_SJ_CDist_PCA_Loadings_No_Rot

# Scores
SPLT_SJ_CDist_PC_Scores <- 
  as.data.frame(SPLT_SJ_CDist_PCA_No_Rot$scores)
colnames(SPLT_SJ_CDist_PC_Scores) <- 
  c("SJ_CDist_PC1", 
    "SJ_CDist_PC2", 
    "SJ_CDist_PC3")
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
            SPLT_WS_PC_Scores,
            SPLT_SAC_CDist_PC_Scores,
            SPLT_SJ_CDist_PC_Scores)
head(SPLT_Model_Data_Final)

write.csv(SPLT_Model_Data_Final, "Output/SPLT_Model_Data_Final.csv", row.names = FALSE)


# Loadings output

SPLT_WS_PCA_Loadings_No_Rot_Out <- 
  as.data.frame(SPLT_WS_PCA_Loadings_No_Rot) %>% 
  mutate(Region = "Watershed",
         Var = row.names(SPLT_WS_PCA_Loadings_No_Rot)) %>% 
  select("Var","PC1", "PC2", "Region")
head(SPLT_WS_PCA_Loadings_No_Rot_Out)

SPLT_SAC_PCA_Loadings_No_Rot_Out <- 
  as.data.frame(SPLT_SAC_PCA_Loadings_No_Rot) %>% 
  mutate(Region = "Sacramento",
         Var = row.names(SPLT_SAC_PCA_Loadings_No_Rot))%>% 
  select("Var", "PC1", "PC2", "Region")
head(SPLT_SAC_PCA_Loadings_No_Rot_Out)

SPLT_DELTA_PCA_Loadings_No_Rot_Out <- 
  as.data.frame(SPLT_DELTA_PCA_Loadings_No_Rot) %>% 
  mutate(Region = "DELTA",
         Var = row.names(SPLT_DELTA_PCA_Loadings_No_Rot))%>% 
  select("Var", "PC1", "PC2", "Region")
head(SPLT_DELTA_PCA_Loadings_No_Rot_Out)

SPLT_SJ_PCA_Loadings_No_Rot_Out <- 
  as.data.frame(SPLT_SJ_PCA_Loadings_No_Rot) %>% 
  mutate(Region = "SJ",
         Var = row.names(SPLT_SJ_PCA_Loadings_No_Rot))%>% 
  select("Var", "PC1", "PC2", "Region")
head(SPLT_SJ_PCA_Loadings_No_Rot_Out)

SPLT_SAC_CDist_PCA_Loadings_No_Rot_Out <- 
  as.data.frame(SPLT_SAC_CDist_PCA_Loadings_No_Rot) %>% 
  mutate(Region = "CDist_SAC",
         Var = row.names(SPLT_SAC_CDist_PCA_Loadings_No_Rot))%>% 
  select("Var", "PC1", "PC2", "Region")
head(SPLT_SAC_CDist_PCA_Loadings_No_Rot_Out)

SPLT_SJ_CDist_PCA_Loadings_No_Rot_Out <- 
  as.data.frame(SPLT_SJ_CDist_PCA_Loadings_No_Rot) %>% 
  mutate(Region = "CDist_SJ",
         Var = row.names(SPLT_SJ_CDist_PCA_Loadings_No_Rot))%>% 
  select("Var", "PC1", "PC2", "Region")
head(SPLT_SJ_CDist_PCA_Loadings_No_Rot_Out)

SPLT_Loadings_Final <- 
  rbind(SPLT_WS_PCA_Loadings_No_Rot_Out,
        SPLT_SAC_PCA_Loadings_No_Rot_Out,
        SPLT_DELTA_PCA_Loadings_No_Rot_Out,
        SPLT_SJ_PCA_Loadings_No_Rot_Out,
        SPLT_SAC_CDist_PCA_Loadings_No_Rot_Out,
        SPLT_SJ_CDist_PCA_Loadings_No_Rot_Out)
SPLT_Loadings_Final

write.csv(SPLT_Loadings_Final, "Tables/SPLT_Loadings_Final.csv", row.names = FALSE)


# SASU --------------------------------------------------------------------

# Final index data
head(SASU_Index_Final)

# Final scaled covariate data
head(SASU_Covariates_Scaled)
cor(SASU_Covariates_Scaled)

SASU_Model_Data <- 
  cbind(SASU_Index_Final[,2:5], 
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
SASU_SAC_PCA_No_Rot <- 
  psych::principal(SASU_Model_Data_SAC_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SASU_SAC_PCA_No_Rot

# Loadings
SASU_SAC_PCA_Loadings_No_Rot_1 <- 
  SASU_SAC_PCA_No_Rot$loadings

SASU_SAC_PCA_Loadings_No_Rot <- 
  round(SASU_SAC_PCA_Loadings_No_Rot_1[,], 2)
SASU_SAC_PCA_Loadings_No_Rot

# Scores
SASU_SAC_PC_Scores <- 
  as.data.frame(SASU_SAC_PCA_No_Rot$scores)
colnames(SASU_SAC_PC_Scores) <- 
  c("SAC_PC1", 
    "SAC_PC2", 
    "SAC_PC3")
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
SASU_SJ_PCA_No_Rot <- 
  psych::principal(SASU_Model_Data_SJ_Mat, 
                   nfactors = 3, 
                   rotate = "none")

# Loadings
SASU_SJ_PCA_Loadings_No_Rot_1 <- 
  SASU_SJ_PCA_No_Rot$loadings

SASU_SJ_PCA_Loadings_No_Rot <- 
  round(SASU_SJ_PCA_Loadings_No_Rot_1[,], 2)
SASU_SJ_PCA_Loadings_No_Rot

# Scores
SASU_SJ_PC_Scores <- 
  as.data.frame(SASU_SJ_PCA_No_Rot$scores)
colnames(SASU_SJ_PC_Scores) <- 
  c("SJ_PC1", 
    "SJ_PC2", 
    "SJ_PC3")
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

SASU_DELTA_PCA_No_Rot <- 
  psych::principal(SASU_Model_Data_DELTA_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SASU_DELTA_PCA_No_Rot

# Loadings
SASU_DELTA_PCA_Loadings_No_Rot_1 <- 
  SASU_DELTA_PCA_No_Rot$loadings

SASU_DELTA_PCA_Loadings_No_Rot <- 
  round(SASU_DELTA_PCA_Loadings_No_Rot_1[,], 2)
SASU_DELTA_PCA_Loadings_No_Rot

# Getting scores
SASU_DELTA_PC_Scores <- 
  as.data.frame(SASU_DELTA_PCA_No_Rot$scores)
colnames(SASU_DELTA_PC_Scores) <- 
  c("DELTA_PC1", 
    "DELTA_PC2", 
    "DELTA_PC3")
SASU_DELTA_PC_Scores


# Watershed Level

SASU_Model_Data_WS <- 
  SASU_Model_Data %>% 
  select(Mean_SAC_Discharge, SAC_Flow_Timing, SAC_Mean_WT,
         Mean_SJ_Discharge, SJ_Flow_Timing, SJ_Mean_WT,
         Mean_DELTA_Discharge, DELTA_Flow_Timing, DELTA_Mean_WT)
head(SASU_Model_Data_WS)
cor(SASU_Model_Data_WS)

SASU_Model_Data_WS_Mat <- 
  SASU_Model_Data_WS %>% 
  as.matrix()
head(SASU_Model_Data_WS_Mat)

SASU_WS_PCA_No_Rot <- 
  psych::principal(SASU_Model_Data_WS_Mat, 
                   nfactors = 9, 
                   rotate = "none")
SASU_WS_PCA_No_Rot

# Loadings
SASU_WS_PCA_Loadings_No_Rot_1 <- 
  SASU_WS_PCA_No_Rot$loadings

SASU_WS_PCA_Loadings_No_Rot <- 
  round(SASU_WS_PCA_Loadings_No_Rot_1[,], 2)
SASU_WS_PCA_Loadings_No_Rot


# Getting scores
SASU_WS_PC_Scores <- 
  as.data.frame(SASU_WS_PCA_No_Rot$scores[,1:3])

colnames(SASU_WS_PC_Scores) <- 
  c("WS_PC1", 
    "WS_PC2", 
    "WS_PC3")
SASU_WS_PC_Scores




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
SASU_SAC_CDist_PCA_No_Rot <- 
  psych::principal(SASU_Model_Data_SAC_CDist_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SASU_SAC_CDist_PCA_No_Rot

# Loadings
SASU_SAC_CDist_PCA_Loadings_No_Rot_1 <- 
  SASU_SAC_CDist_PCA_No_Rot$loadings

SASU_SAC_CDist_PCA_Loadings_No_Rot <- 
  round(SASU_SAC_CDist_PCA_Loadings_No_Rot_1[,], 2)
SASU_SAC_CDist_PCA_Loadings_No_Rot

# Scores
SASU_SAC_CDist_PC_Scores <- 
  as.data.frame(SASU_SAC_CDist_PCA_No_Rot$scores)
colnames(SASU_SAC_CDist_PC_Scores) <- 
  c("SAC_CDist_PC1", 
    "SAC_CDist_PC2", 
    "SAC_CDist_PC3")
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
SASU_SJ_CDist_PCA_No_Rot <- 
  psych::principal(SASU_Model_Data_SJ_CDist_Mat, 
                   nfactors = 3, 
                   rotate = "none")
SASU_SJ_CDist_PCA_No_Rot

# Loadings
SASU_SJ_CDist_PCA_Loadings_No_Rot_1 <- 
  SASU_SJ_CDist_PCA_No_Rot$loadings

SASU_SJ_CDist_PCA_Loadings_No_Rot <- 
  round(SASU_SJ_CDist_PCA_Loadings_No_Rot_1[,], 2)
SASU_SJ_CDist_PCA_Loadings_No_Rot

# Scores
SASU_SJ_CDist_PC_Scores <- 
  as.data.frame(SASU_SJ_CDist_PCA_No_Rot$scores)
colnames(SASU_SJ_CDist_PC_Scores) <- 
  c("SJ_CDist_PC1", 
    "SJ_CDist_PC2", 
    "SJ_CDist_PC3")
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
            SASU_WS_PC_Scores,
            SASU_SAC_CDist_PC_Scores,
            SASU_SJ_CDist_PC_Scores)
head(SASU_Model_Data_Final)

write.csv(SASU_Model_Data_Final, "Output/SASU_Model_Data_Final.csv", row.names = FALSE)


# iv. Abundance Models --------------------------------------------------------

# All subset selection that examine the influence of Water temp, mean flow during spawning, and timing of flow on age-0 abundance

# SAPM --------------------------------------------------------------------

head(SAPM_Model_Data_Final)

# 1) Watershed Models ----------------------------------------------------

pairs(SAPM_Model_Data_Final[,c(3,5:7)], pch = 16)
# Best relationship with RC2...but slightly non-linear

# Delta-Wide Index
SAPM_DW_Global_lm <- 
  lm(log(Watershed_Index) ~ WS_PC1 + WS_PC2, 
     data = SAPM_Model_Data_Final, na.action = "na.fail")
summary(SAPM_DW_Global_lm)

# Model assumptions
plot(SAPM_DW_Global_lm) # Assumptions look fine
acf(SAPM_DW_Global_lm$residuals) # No Autocorreltion


## All subset selection
SAPM_DW_Global_Dredge <- 
  dredge(SAPM_DW_Global_lm,
         extra = list(
           "R^2", "Dredge_Function" = function(x) {
             s <- summary(x)
             c(adjRsq = s$adj.r.squared)
           }))
SAPM_DW_Global_Dredge

importance(SAPM_DW_Global_Dredge)

### Getting model output and weights
SAPM_DW_Dredge_Output <- 
  as.data.frame(cbind(round(SAPM_DW_Global_Dredge$`(Intercept)`, 2),
                      round(SAPM_DW_Global_Dredge$WS_PC1, 2),
                      round(SAPM_DW_Global_Dredge$WS_PC2, 2), 
                      SAPM_DW_Global_Dredge$df,
                      round(SAPM_DW_Global_Dredge$`R^2`, 2),
                      round(SAPM_DW_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SAPM_DW_Global_Dredge$logLik, 2), 
                      round(SAPM_DW_Global_Dredge$AICc, 2),
                      round(SAPM_DW_Global_Dredge$delta, 2), 
                      SAPM_DW_Global_Dredge$weight)) %>% 
  mutate(Species = "SAPM", Region = "DW")
colnames(SAPM_DW_Dredge_Output) = c("Intercept", "WS_PC1", "WS_PC2", 
                                    "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                    "Delta", "Weights", "Species", "Region")
SAPM_DW_Dredge_Output

# Getting weights for output
SAPM_DW_PC2_Weights <- 
  SAPM_DW_Dredge_Output %>% 
  filter(!(is.na(WS_PC2))) %>% # Selecting rows where SAC_PC2 is not NA
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", 
         Region = "DW", 
         Variable = "WS_PC2")

SAPM_DW_Weights <- 
  SAPM_DW_Dredge_Output %>% 
  filter(!(is.na(WS_PC1))) %>% # Selecting rows where SAC_PC1 is not NA
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", 
         Region = "DW", 
         Variable = "WS_PC1") %>% 
  bind_rows(SAPM_DW_PC2_Weights) # Binding summed weights of SAC_PC2
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
  replace_na(list(WS_PC1 = "", WS_PC2 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SAPM_DW_Dredge_Output_Final

write.csv(SAPM_DW_Dredge_Output_Final, 
          "Tables/SAPM_DW_Dredge_Abun_Output.csv", 
          row.names = FALSE)


# 2) SAC Models ----------------------------------------------------

pairs(SAPM_Model_Data_Final[,c(2,5:7)], pch = 16)


# Global model for all subset selection
SAPM_SAC_Global_lm <- 
  lm(log(Sacramento_Index) ~ SAC_PC1 + SAC_PC2, 
     data = SAPM_Model_Data_Final, na.action = "na.fail")
summary(SAPM_SAC_Global_lm)

# Model assumptions
plot(SAPM_SAC_Global_lm) # Assumptions look fine
acf(SAPM_SAC_Global_lm$residuals) # No Autocorreltion

## All subset selection
SAPM_SAC_Global_Dredge <- 
  dredge(SAPM_SAC_Global_lm,
         extra = list(
           "R^2", "Dredge_Function" = function(x) {
             s <- summary(x)
             c(adjRsq = s$adj.r.squared)
           }))
SAPM_SAC_Global_Dredge

importance(SAPM_SAC_Global_Dredge)

### Getting model output and weights
SAPM_SAC_Dredge_Output <- 
  as.data.frame(cbind(round(SAPM_SAC_Global_Dredge$`(Intercept)`, 2), 
                      round(SAPM_SAC_Global_Dredge$SAC_PC1, 2),
                      round(SAPM_SAC_Global_Dredge$SAC_PC2, 2),
                      SAPM_SAC_Global_Dredge$df,
                      round(SAPM_SAC_Global_Dredge$`R^2`, 2),
                      round(SAPM_SAC_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SAPM_SAC_Global_Dredge$logLik, 2), 
                      round(SAPM_SAC_Global_Dredge$AICc, 2),
                      round(SAPM_SAC_Global_Dredge$delta, 2), 
                      SAPM_SAC_Global_Dredge$weight)) %>% 
  mutate(Species = "SAPM", Region = "SAC")
colnames(SAPM_SAC_Dredge_Output) = c("Intercept", "SAC_PC1", "SAC_PC2", 
                                     "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                     "Delta", "Weights", "Species", "Region")
SAPM_SAC_Dredge_Output

# Model weights output
SAPM_SAC_PC2_Weights <- 
  SAPM_SAC_Dredge_Output %>% 
  filter(!(is.na(SAC_PC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", Region = "SAC", Variable = "SAC_PC2")

SAPM_SAC_Weights <- 
  SAPM_SAC_Dredge_Output %>% 
  filter(!(is.na(SAC_PC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", Region = "SAC", Variable = "SAC_PC1") %>% 
  bind_rows(SAPM_SAC_PC2_Weights)
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
  replace_na(list(SAC_PC1 = "", SAC_PC2 = "")) %>% 
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
  lm(log(Delta_Index) ~ DELTA_PC1 + DELTA_PC2, 
     data = SAPM_Model_Data_Final, na.action = "na.fail")
summary(SAPM_Delta_Global_lm)

# Model assumptions
plot(SAPM_Delta_Global_lm) # Assumptions look fine
acf(SAPM_Delta_Global_lm$residuals) # No Autocorreltion


## All subset selection
SAPM_DELTA_Global_Dredge <- 
  dredge(SAPM_Delta_Global_lm,
         extra = list(
           "R^2", "Dredge_Function" = function(x) {
             s <- summary(x)
             c(adjRsq = s$adj.r.squared)
           }))
SAPM_DELTA_Global_Dredge
# Top models include PC1

importance(SAPM_DELTA_Global_Dredge)

### Getting model output and weights
SAPM_DELTA_Dredge_Output <- 
  as.data.frame(cbind(round(SAPM_DELTA_Global_Dredge$`(Intercept)`, 2),
                      round(SAPM_DELTA_Global_Dredge$DELTA_PC1, 2), 
                      round(SAPM_DELTA_Global_Dredge$DELTA_PC2, 2),
                      SAPM_DELTA_Global_Dredge$df,
                      round(SAPM_DELTA_Global_Dredge$`R^2`, 2),
                      round(SAPM_DELTA_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SAPM_DELTA_Global_Dredge$logLik, 2), 
                      round(SAPM_DELTA_Global_Dredge$AICc, 2),
                      round(SAPM_DELTA_Global_Dredge$delta, 2), 
                      SAPM_DELTA_Global_Dredge$weight)) %>% 
  mutate(Species = "SAPM", Region = "DELTA")
colnames(SAPM_DELTA_Dredge_Output) = c("Intercept", "DELTA_PC1", "DELTA_PC2", 
                                       "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                       "Delta", "Weights", "Species", "Region")
SAPM_DELTA_Dredge_Output

# Getting weights for output
SAPM_DELTA_PC2_Weights <- 
  SAPM_DELTA_Dredge_Output %>% 
  filter(!(is.na(DELTA_PC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", Region = "DELTA", Variable = "DELTA_PC2")

SAPM_DELTA_Weights <- 
  SAPM_DELTA_Dredge_Output %>% 
  filter(!(is.na(DELTA_PC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", Region = "DELTA", Variable = "DELTA_PC1") %>% 
  bind_rows(SAPM_DELTA_PC2_Weights)
SAPM_DELTA_Weights

# Checking weights
importance(SAPM_DELTA_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set (can't due because only one model)
SAPM_DELTA_MA <- model.avg(SAPM_DELTA_Global_Dredge, 
                           rank = "AIC")
SAPM_DELTA_MA_Sum = summary(SAPM_DELTA_MA)
SAPM_DELTA_MA_Full_Out = SAPM_DELTA_MA_Sum$coefmat.full
SAPM_DELTA_MA_Full_Out

# i. Dredge Output ------------------------------------------------------------------


# Dredge output
SAPM_DELTA_Dredge_Output_Final <- 
  SAPM_DELTA_Dredge_Output %>% 
  replace_na(list(DELTA_PC1 = "", DELTA_PC2 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SAPM_DELTA_Dredge_Output_Final

write.csv(SAPM_DELTA_Dredge_Output_Final, 
          "Tables/SAPM_DELTA_Dredge_Abun_Output.csv", 
          row.names = FALSE)
# SPLT --------------------------------------------------------------------

# Final data for models
head(SPLT_Model_Data_Final)

# 1) Delta-Wide Models ----------------------------------------------------

pairs(SPLT_Model_Data_Final[,c(4,16:18)], pch = 16)

# Delta-Wide Index
SPLT_DW_Global_lm <- 
  lm(log(Watershed_Index) ~ WS_PC1 + WS_PC2, 
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


importance(SPLT_DW_Global_Dredge)

### Getting model output and weights
SPLT_DW_Dredge_Output <- 
  as.data.frame(cbind(round(SPLT_DW_Global_Dredge$`(Intercept)`, 2),
                      round(SPLT_DW_Global_Dredge$WS_PC1, 2),
                      round(SPLT_DW_Global_Dredge$WS_PC2, 2), 
                      SPLT_DW_Global_Dredge$df,
                      round(SPLT_DW_Global_Dredge$`R^2`, 2),
                      round(SPLT_DW_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SPLT_DW_Global_Dredge$logLik, 2), 
                      round(SPLT_DW_Global_Dredge$AICc, 2),
                      round(SPLT_DW_Global_Dredge$delta, 2), 
                      SPLT_DW_Global_Dredge$weight)) %>% 
  mutate(Species = "SPLT", Region = "DW")
colnames(SPLT_DW_Dredge_Output) = c("Intercept", "WS_PC1", "WS_PC2", 
                                    "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                    "Delta", "Weights", "Species", "Region")

# Getting weights for output
SPLT_DW_PC2_Weights <- 
  SPLT_DW_Dredge_Output %>% 
  filter(!(is.na(WS_PC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "DW", Variable = "WS_PC2")

SPLT_DW_Weights <- 
  SPLT_DW_Dredge_Output %>% 
  filter(!(is.na(WS_PC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "DW", Variable = "WS_PC1") %>% 
  bind_rows(SPLT_DW_PC2_Weights)
SPLT_DW_Weights

# Checking weight output
importance(SPLT_DW_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set - can't because too few models
SPLT_DW_MA <- model.avg(SPLT_DW_Global_Dredge, 
                        rank = "AIC")
SPLT_DW_MA_Sum = summary(SPLT_DW_MA)
SPLT_DW_MA_Full_Out = SPLT_DW_MA_Sum$coefmat.full
SPLT_DW_MA_Full_Out

# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SPLT_DW_Dredge_Output_Final <- 
  SPLT_DW_Dredge_Output %>% 
  replace_na(list(WS_PC1 = "", WS_PC2 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SPLT_DW_Dredge_Output_Final

write.csv(SPLT_DW_Dredge_Output_Final, 
          "Tables/SPLT_DW_Dredge_Abun_Output.csv", 
          row.names = FALSE)


# 2) SJ Models ----------------------------------------------------

pairs(SPLT_Model_Data_Final[,c(3,10:12)], pch = 16)

# Global model for all subset selection
SPLT_SJ_Global_lm <- 
  lm(log(San_Joaquin_Index) ~ SJ_PC1 + SJ_PC2, 
     data = SPLT_Model_Data_Final, na.action = "na.fail")
summary(SPLT_SJ_Global_lm)

# Model assumptions
plot(SPLT_SJ_Global_lm) # Assumptions look fine
acf(SPLT_SJ_Global_lm$residuals) # No Autocorreltion

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


### Getting model output and weights
SPLT_SJ_Dredge_Output <- 
  as.data.frame(cbind(round(SPLT_SJ_Global_Dredge$`(Intercept)`, 2),
                      round(SPLT_SJ_Global_Dredge$SJ_PC1, 2),
                      round(SPLT_SJ_Global_Dredge$SJ_PC2, 2),
                      SPLT_SJ_Global_Dredge$df,
                      round(SPLT_SJ_Global_Dredge$`R^2`, 2),
                      round(SPLT_SJ_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SPLT_SJ_Global_Dredge$logLik, 2), 
                      round(SPLT_SJ_Global_Dredge$AICc, 2),
                      round(SPLT_SJ_Global_Dredge$delta, 2), 
                      SPLT_SJ_Global_Dredge$weight)) %>% 
  mutate(Species = "SPLT", Region = "SJ")
colnames(SPLT_SJ_Dredge_Output) = c("Intercept", "SJ_PC1", "SJ_PC2",
                                    "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                    "Delta", "Weights", "Species", "Region")
SPLT_SJ_Dredge_Output

# Getting weights output
SPLT_SJ_PC2_Weights <- 
  SPLT_SJ_Dredge_Output %>% 
  filter(!(is.na(SJ_PC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "SJ", Variable = "SJ_PC2")

SPLT_SJ_Weights <- 
  SPLT_SJ_Dredge_Output %>% 
  filter(!(is.na(SJ_PC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "SJ", Variable = "SJ_PC1") %>% 
  bind_rows(SPLT_SJ_PC2_Weights)
SPLT_SJ_Weights

# Checking weights output
importance(SPLT_SJ_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set - only one model 
SPLT_SJ_MA <- model.avg(SPLT_SJ_Global_Dredge, 
                        rank = "AIC")
SPLT_SJ_MA_Sum = summary(SPLT_SJ_MA)
SPLT_SJ_MA_Full_Out = SPLT_SJ_MA_Sum$coefmat.full
SPLT_SJ_MA_Full_Out


# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SPLT_SJ_Dredge_Output_Final <- 
  SPLT_SJ_Dredge_Output %>% 
  replace_na(list(SJ_PC1 = "", SJ_PC2 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SPLT_SJ_Dredge_Output_Final

write.csv(SPLT_SJ_Dredge_Output_Final, 
          "Tables/SPLT_SJ_Dredge_Abun_Output.csv", 
          row.names = FALSE)

# 3) SAC Models ----------------------------------------------------

pairs(SPLT_Model_Data_Final[,c(2,7:9)], pch = 16)

# Global model for all subset selection
SPLT_SAC_Global_lm <- 
  lm(log(Sacramento_Index) ~ SAC_PC1 + SAC_PC2, 
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


### Getting model output and weights
SPLT_SAC_Dredge_Output <- 
  as.data.frame(cbind(round(SPLT_SAC_Global_Dredge$`(Intercept)`, 2),
                      round(SPLT_SAC_Global_Dredge$SAC_PC1, 2),
                      round(SPLT_SAC_Global_Dredge$SAC_PC2, 2),
                      SPLT_SAC_Global_Dredge$df,
                      round(SPLT_SAC_Global_Dredge$`R^2`, 2),
                      round(SPLT_SAC_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SPLT_SAC_Global_Dredge$logLik, 2), 
                      round(SPLT_SAC_Global_Dredge$AICc, 2),
                      round(SPLT_SAC_Global_Dredge$delta, 2), 
                      SPLT_SAC_Global_Dredge$weight)) %>% 
  mutate(Species = "SPLT", Region = "SAC")
colnames(SPLT_SAC_Dredge_Output) = c("Intercept", "SAC_PC1", "SAC_PC2",
                                     "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                     "Delta", "Weights", "Species", "Region")
SPLT_SAC_Dredge_Output

# Getting weights output
SPLT_SAC_PC2_Weights <- 
  SPLT_SAC_Dredge_Output %>% 
  filter(!(is.na(SAC_PC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "SAC", Variable = "SAC_PC2")

SPLT_SAC_Weights <- 
  SPLT_SAC_Dredge_Output %>% 
  filter(!(is.na(SAC_PC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "SAC", Variable = "SAC_PC1") %>% 
  bind_rows(SPLT_SAC_PC2_Weights)
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
  replace_na(list(SAC_PC1 = "", SAC_PC2 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SPLT_SAC_Dredge_Output_Final

write.csv(SPLT_SAC_Dredge_Output_Final, 
          "Tables/SPLT_SAC_Dredge_Abun_Output.csv", 
          row.names = FALSE)

# 4) Delta Models ----------------------------------------------------

pairs(SPLT_Model_Data_Final[,c(1,13:15)], pch = 16)

# Global model for all subset selection
SPLT_Delta_Global_lm <- 
  lm(log(Delta_Index) ~ DELTA_PC1 + DELTA_PC2, 
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
                      round(SPLT_DELTA_Global_Dredge$DELTA_PC1, 2),
                      round(SPLT_DELTA_Global_Dredge$DELTA_PC2, 2), 
                      SPLT_DELTA_Global_Dredge$df,
                      round(SPLT_DELTA_Global_Dredge$`R^2`, 2),
                      round(SPLT_DELTA_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SPLT_DELTA_Global_Dredge$logLik, 2), 
                      round(SPLT_DELTA_Global_Dredge$AICc, 2),
                      round(SPLT_DELTA_Global_Dredge$delta, 2), 
                      SPLT_DELTA_Global_Dredge$weight)) %>% 
  mutate(Species = "SPLT", Region = "DELTA")
colnames(SPLT_DELTA_Dredge_Output) = c("Intercept", "DELTA_PC1", "DELTA_PC2", 
                                       "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                       "Delta", "Weights", "Species", "Region")
SPLT_DELTA_Dredge_Output

# Getting weights output
SPLT_DELTA_PC2_Weights <- 
  SPLT_DELTA_Dredge_Output %>% 
  filter(!(is.na(DELTA_PC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "DELTA", Variable = "DELTA_PC2")

SPLT_DELTA_Weights <- 
  SPLT_DELTA_Dredge_Output %>% 
  filter(!(is.na(DELTA_PC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "DELTA", Variable = "DELTA_PC1") %>% 
  bind_rows(SPLT_DELTA_PC2_Weights)
SPLT_DELTA_Weights

# Checking weights output
importance(SPLT_DELTA_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Can't do top 95% as there is only one model
SPLT_DELTA_MA <- model.avg(SPLT_DELTA_Global_Dredge, 
                           rank = "AIC")
SPLT_DELTA_MA_Sum = summary(SPLT_DELTA_MA)
SPLT_DELTA_MA_Full_Out = SPLT_DELTA_MA_Sum$coefmat.full
SPLT_DELTA_MA_Full_Out

# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SPLT_DELTA_Dredge_Output_Final <- 
  SPLT_DELTA_Dredge_Output %>% 
  replace_na(list(DELTA_PC1 = "", DELTA_PC2 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SPLT_DELTA_Dredge_Output_Final

write.csv(SPLT_DELTA_Dredge_Output_Final, 
          "Tables/SPLT_DELTA_Dredge_Abun_Output.csv", 
          row.names = FALSE)



# SASU --------------------------------------------------------------------

# Final data
head(SASU_Model_Data_Final)

# 1) Delta-Wide Models ----------------------------------------------------

pairs(SASU_Model_Data_Final[,c(4,16:18)], pch = 16)

SASU_DW_Global_lm <- 
  lm(log(Watershed_Index) ~ WS_PC1 + WS_PC2, 
     data = SASU_Model_Data_Final, na.action = "na.fail")
summary(SASU_DW_Global_lm)

# Model assumptions
plot(SASU_DW_Global_lm) # Assumptions look fine
acf(SASU_DW_Global_lm$residuals) # No Autocorreltion

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


### Getting model output and weights
SASU_DW_Dredge_Output <- 
  as.data.frame(cbind(round(SASU_DW_Global_Dredge$`(Intercept)`, 2),
                      round(SASU_DW_Global_Dredge$WS_PC1, 2),
                      round(SASU_DW_Global_Dredge$WS_PC2, 2),
                      SASU_DW_Global_Dredge$df,
                      round(SASU_DW_Global_Dredge$`R^2`, 2),
                      round(SASU_DW_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SASU_DW_Global_Dredge$logLik, 2), 
                      round(SASU_DW_Global_Dredge$AICc, 2),
                      round(SASU_DW_Global_Dredge$delta, 2), 
                      SASU_DW_Global_Dredge$weight)) %>% 
  mutate(Species = "SASU", Region = "DW")
colnames(SASU_DW_Dredge_Output) = c("Intercept", "WS_PC1", "WS_PC2", 
                                    "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                    "Delta", "Weights", "Species", "Region")
SASU_DW_Dredge_Output

# Getting weights output
SASU_DW_PC2_Weights <- 
  SASU_DW_Dredge_Output %>% 
  filter(!(is.na(WS_PC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "DW", Variable = "WS_PC2")

SASU_DW_Weights <- 
  SASU_DW_Dredge_Output %>% 
  filter(!(is.na(WS_PC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "DW", Variable = "WS_PC1") %>% 
  bind_rows(SASU_DW_PC2_Weights)
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
  replace_na(list(WS_PC1 = "", WS_PC2 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SASU_DW_Dredge_Output_Final

write.csv(SASU_DW_Dredge_Output_Final, 
          "Tables/SASU_DW_Dredge_Abun_Output.csv", 
          row.names = FALSE)

# 2) SJ Models ----------------------------------------------------

pairs(SASU_Model_Data_Final[,c(3,10:12)], pch = 16)

# Global model for all subset selection
SASU_SJ_Global_lm <- 
  lm(log(San_Joaquin_Index) ~ SJ_PC1 + SJ_PC2, 
     data = SASU_Model_Data_Final, na.action = "na.fail")
summary(SASU_SJ_Global_lm)

# Model assumptions
plot(SASU_SJ_Global_lm) # Normality is questionable 
acf(SASU_SJ_Global_lm$residuals) # No Autocorreltion

## All subset selection
SASU_SJ_Global_Dredge <- 
  dredge(SASU_SJ_Global_lm,
         extra = list(
           "R^2", "Dredge_Function" = function(x) {
             s <- summary(x)
             c(adjRsq = s$adj.r.squared)
           }))
SASU_SJ_Global_Dredge

importance(SASU_SJ_Global_Dredge)


### Getting model output and weights
SASU_SJ_Dredge_Output <- 
  as.data.frame(cbind(round(SASU_SJ_Global_Dredge$`(Intercept)`, 2), 
                      round(SASU_SJ_Global_Dredge$SJ_PC1, 2),
                      round(SASU_SJ_Global_Dredge$SJ_PC2, 2),
                      SASU_SJ_Global_Dredge$df,
                      round(SASU_SJ_Global_Dredge$`R^2`, 2),
                      round(SASU_SJ_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SASU_SJ_Global_Dredge$logLik, 2), 
                      round(SASU_SJ_Global_Dredge$AICc, 2),
                      round(SASU_SJ_Global_Dredge$delta, 2), 
                      SASU_SJ_Global_Dredge$weight)) %>% 
  mutate(Species = "SASU", Region = "SJ")
colnames(SASU_SJ_Dredge_Output) = c("Intercept", "SJ_PC1", "SJ_PC2", 
                                    "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                    "Delta", "Weights", "Species", "Region")
SASU_SJ_Dredge_Output

# Getting weights output
SASU_SJ_PC2_Weights <- 
  SASU_SJ_Dredge_Output %>% 
  filter(!(is.na(SJ_PC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "SJ", Variable = "SJ_PC2")

SASU_SJ_Weights <- 
  SASU_SJ_Dredge_Output %>% 
  filter(!(is.na(SJ_PC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "SJ", Variable = "SJ_PC1") %>% 
  bind_rows(SASU_SJ_PC2_Weights)
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
  replace_na(list(SJ_PC1 = "", SJ_PC2 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SASU_SJ_Dredge_Output_Final

write.csv(SASU_SJ_Dredge_Output_Final, 
          "Tables/SASU_SJ_Dredge_Abun_Output.csv", 
          row.names = FALSE)

# 3) SAC Models ----------------------------------------------------

pairs(SASU_Model_Data_Final[,c(2,7:9)], pch = 16)

# Global model for all subset selection
SASU_SAC_Global_lm <- 
  lm(log(Sacramento_Index) ~ SAC_PC1 + SAC_PC2, 
     data = SASU_Model_Data_Final, na.action = "na.fail")
summary(SASU_SAC_Global_lm)

# Model assumptions
plot(SASU_SAC_Global_lm) # Assumptions look fine
acf(SASU_SAC_Global_lm$residuals) # No Autocorreltion

## All subset selection
SASU_SAC_Global_Dredge <- 
  dredge(SASU_SAC_Global_lm,
         extra = list(
           "R^2", "Dredge_Function" = function(x) {
             s <- summary(x)
             c(adjRsq = s$adj.r.squared)
           }))
SASU_SAC_Global_Dredge
# Top models include discharge (PC 1) and water temp (PC 3)

importance(SASU_SAC_Global_Dredge)

### Getting model output and weights
SASU_SAC_Dredge_Output <- 
  as.data.frame(cbind(round(SASU_SAC_Global_Dredge$`(Intercept)`, 2),
                      round(SASU_SAC_Global_Dredge$SAC_PC1, 2),
                      round(SASU_SAC_Global_Dredge$SAC_PC2, 2),
                      SASU_SAC_Global_Dredge$df,
                      round(SASU_SAC_Global_Dredge$`R^2`, 2),
                      round(SASU_SAC_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SASU_SAC_Global_Dredge$logLik, 2), 
                      round(SASU_SAC_Global_Dredge$AICc, 2),
                      round(SASU_SAC_Global_Dredge$delta, 2), 
                      SASU_SAC_Global_Dredge$weight)) %>% 
  mutate(Species = "SASU", Region = "SAC")
colnames(SASU_SAC_Dredge_Output) = c("Intercept", "SAC_PC1", "SAC_PC2", 
                                     "DF", "R^2", "R^2_Adj","logLik","AICc", 
                                     "Delta", "Weights", "Species", "Region")
SASU_SAC_Dredge_Output

# Getting weights output
SASU_SAC_PC2_Weights <- 
  SASU_SAC_Dredge_Output %>% 
  filter(!(is.na(SAC_PC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "SAC", Variable = "SAC_PC2")

SASU_SAC_Weights <- 
  SASU_SAC_Dredge_Output %>% 
  filter(!(is.na(SAC_PC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "SAC", Variable = "SAC_PC1") %>% 
  bind_rows(SASU_SAC_PC2_Weights)
SASU_SAC_Weights

# Checking weights output
importance(SASU_SAC_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set - only one model so can't do this
SASU_SAC_MA <- model.avg(SASU_SAC_Global_Dredge, 
                         rank = "AIC")
SASU_SAC_MA_Sum = summary(SASU_SAC_MA)
SASU_SAC_MA_Full_Out = SASU_SAC_MA_Sum$coefmat.full
SASU_SAC_MA_Full_Out

# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SASU_SAC_Dredge_Output_Final <- 
  SASU_SAC_Dredge_Output %>% 
  replace_na(list(SAC_PC1 = "", SAC_PC2 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SASU_SAC_Dredge_Output_Final

write.csv(SASU_SAC_Dredge_Output_Final, 
          "Tables/SASU_SAC_Dredge_Abun_Output.csv", 
          row.names = FALSE)

# 4) Delta Models ----------------------------------------------------

pairs(SASU_Model_Data_Final[,c(1,13:15)], pch = 16)

# Global model for all subset selection
SASU_Delta_Global_lm <- 
  lm(log(Delta_Index) ~ DELTA_PC1 + DELTA_PC2, 
     data = SASU_Model_Data_Final, na.action = "na.fail")
summary(SASU_Delta_Global_lm)

# Model assumptions
plot(SASU_Delta_Global_lm) # Assumptions look fine
acf(SASU_Delta_Global_lm$residuals) # No Autocorreltion

## All subset selection
SASU_DELTA_Global_Dredge <- 
  dredge(SASU_Delta_Global_lm,
         extra = list(
           "R^2", "Dredge_Function" = function(x) {
             s <- summary(x)
             c(adjRsq = s$adj.r.squared)
           }))
SASU_DELTA_Global_Dredge

importance(SASU_DELTA_Global_Dredge)

### Getting model output and weights
SASU_DELTA_Dredge_Output <- 
  as.data.frame(cbind(round(SASU_DELTA_Global_Dredge$`(Intercept)`, 2),
                      round(SASU_DELTA_Global_Dredge$DELTA_PC1, 2),
                      round(SASU_DELTA_Global_Dredge$DELTA_PC2, 2),
                      SASU_DELTA_Global_Dredge$df,
                      round(SASU_DELTA_Global_Dredge$`R^2`, 2),
                      round(SASU_DELTA_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SASU_DELTA_Global_Dredge$logLik, 2), 
                      round(SASU_DELTA_Global_Dredge$AICc, 2),
                      round(SASU_DELTA_Global_Dredge$delta, 2), 
                      SASU_DELTA_Global_Dredge$weight)) %>% 
  mutate(Species = "SASU", Region = "DELTA")
colnames(SASU_DELTA_Dredge_Output) = c("Intercept", "DELTA_PC1", "DELTA_PC2",
                                       "DF", "R^2", "R^2_Adj",
                                       "logLik","AICc", "Delta", "Weights",
                                       "Species", "Region")
SASU_DELTA_Dredge_Output

# Getting weights output
SASU_DELTA_PC2_Weights <- 
  SASU_DELTA_Dredge_Output %>% 
  filter(!(is.na(DELTA_PC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "DELTA", Variable = "DELTA_PC2")

SASU_DELTA_Weights <- 
  SASU_DELTA_Dredge_Output %>% 
  filter(!(is.na(DELTA_PC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "DELTA", Variable = "DELTA_PC1") %>% 
  bind_rows(SASU_DELTA_PC2_Weights)
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
  replace_na(list(DELTA_PC1 = "", DELTA_PC2 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights)) 
SASU_DELTA_Dredge_Output_Final

write.csv(SASU_DELTA_Dredge_Output_Final, 
          "Tables/SASU_DELTA_Dredge_Abun_Output.csv", 
          row.names = FALSE)



# ***Species Combined Weights Output***----------------------------------------------


# W/ original variables
All_Sp_Abun_Weights_Output <- 
  bind_rows(SAPM_DW_Weights,
            SAPM_SAC_Weights,
            SAPM_DELTA_Weights,
            SPLT_DW_Weights,
            SPLT_SAC_Weights,
            SPLT_SJ_Weights,
            SPLT_DELTA_Weights,
            SASU_DW_Weights,
            SASU_SAC_Weights,
            SASU_SJ_Weights,
            SASU_DELTA_Weights) %>% 
  mutate(Region = case_when(Region == "DW" ~ "Watershed",
                            Region == "DELTA" ~ "Delta",
                            Region == "SAC" ~ "Sacramento",
                            Region == "SJ" ~ "San Joaquin"),
         Covariate = case_when(grepl("PC1", Variable) ~ "PC1",
                               grepl("PC2", Variable) ~ "PC2")) %>%
  select(!Variable)

All_Sp_Abun_Weights_Wide_Output <- 
  pivot_wider(All_Sp_Abun_Weights_Output,
              names_from = Region,
              values_from = Weights_Sum)
All_Sp_Abun_Weights_Wide_Output

### Final Output included below w/ CDist output



# v. Center of Distribution ---------------------------------------------

# Look at center of distribution (what Ted did) based on river KM

### Vary months used for the distribution (later then what was used for the indices) and maybe look at different months of flow too

# When considering months of flow, it probably doesn't make sense to look at flows before spawning...we are using center of dist that was calculated after those flows. It probably doesn't make sense to look at flows during spawning either unless some of the earlier hatched fish are washed DS

# CDist Summary Stats -------------------------------------------------------------

# General plots looking at change in Dist on Sac over the time series. What's apparent is that there's greater variability in the dist of SPLT, which might provide enough contrast to detect relationships with covariates. Also we might be missing a lot of larger part of SAPM and SASU pops.
plot(CDist_SAC ~ as.numeric(Year), SAPM_CDist, pch = 16)

plot(CDist_SAC ~ as.numeric(Year), SPLT_CDist, pch = 16)
plot(CDist_SJ ~ as.numeric(Year), SPLT_CDist, pch = 16)

plot(CDist_SAC ~ as.numeric(Year), SASU_CDist, pch = 16) # looks like decreasing trend
plot(CDist_SJ ~ as.numeric(Year), SASU_CDist, pch = 16) # looks like increasing trend


# Species
SAPM_Sum_Dist_SAC_Stats <- 
  SAPM_CDist %>% 
  mutate(Species = "SAPM",
         Region = "Sacramento") %>% 
  group_by(Species,
           Region) %>% 
  summarise(Mean = mean(CDist_SAC), Min = min(CDist_SAC), Max = max(CDist_SAC), SD = sd(CDist_SAC))
head(SAPM_Sum_Dist_SAC_Stats)


SPLT_Sum_Dist_SAC_Stats <- 
  SPLT_CDist %>% 
  select(-CDist_SJ) %>% 
  mutate(Species = "SPLT",
         Region = "Sacramento") %>% 
  group_by(Species,
           Region) %>% 
  summarise(Mean = mean(CDist_SAC), Min = min(CDist_SAC), Max = max(CDist_SAC), SD = sd(CDist_SAC))
head(SPLT_Sum_Dist_SAC_Stats)


SPLT_Sum_Dist_SJ_Stats <- 
  SPLT_CDist %>% 
  select(-CDist_SAC) %>% 
  mutate(Species = "SPLT",
         Region = "San Joaquin") %>% 
  group_by(Species,
           Region) %>% 
  summarise(Mean = mean(CDist_SJ), Min = min(CDist_SJ), Max = max(CDist_SJ), SD = sd(CDist_SJ))
head(SPLT_Sum_Dist_SJ_Stats)



SASU_Sum_Dist_SAC_Stats <- 
  SASU_CDist %>% 
  select(-CDist_SJ) %>% 
  mutate(Species = "SASU",
         Region = "Sacramento") %>% 
  group_by(Species,
           Region) %>% 
  summarise(Mean = mean(CDist_SAC), Min = min(CDist_SAC), Max = max(CDist_SAC), SD = sd(CDist_SAC))
head(SASU_Sum_Dist_SAC_Stats)


SASU_Sum_Dist_SJ_Stats <- 
  SASU_CDist %>% 
  select(-CDist_SAC) %>% 
  mutate(Species = "SASU",
         Region = "San Joaquin") %>% 
  group_by(Species,
           Region) %>% 
  summarise(Mean = mean(CDist_SJ), Min = min(CDist_SJ), Max = max(CDist_SJ), SD = sd(CDist_SJ))
head(SASU_Sum_Dist_SJ_Stats)

# ***Data Output*** ------------------------------------------------------------

Dist_Summaries <- 
  bind_rows(SAPM_Sum_Dist_SAC_Stats,
        SPLT_Sum_Dist_SAC_Stats,
        SPLT_Sum_Dist_SJ_Stats,
        SASU_Sum_Dist_SAC_Stats,
        SASU_Sum_Dist_SJ_Stats) %>% 
  mutate(Species = case_when (Species == "SAPM" ~ "Sacramento Pikeminnow",
                              Species == "SPLT" ~ "Sacramento Splittail",
                              Species == "SASU" ~ "Sacramento Sucker" ))

write.csv(Dist_Summaries, "Tables/Dist_Summaries.csv", row.names = FALSE)


# SAPM --------------------------------------------------------------------

head(SAPM_Model_Data_Final) # Distribution data

# Visualizing relationships between dist and covariates 
pairs(SAPM_Model_Data_Final[,c(4,14:16)], pch = 16)

SAPM_SAC_Dist_Global_lm <- 
  lm(log(CDist_SAC) ~ CDist_PC1 + CDist_PC2, 
     data = SAPM_Model_Data_Final, 
     na.action = "na.fail")
summary(SAPM_SAC_Dist_Global_lm)

# Model assumptions
plot(SAPM_SAC_Dist_Global_lm) # Normality is questionable
acf(SAPM_SAC_Dist_Global_lm$residuals) # Autocorreltion is fine


## All subset selection
SAPM_SAC_Dist_Global_Dredge <- 
  dredge(SAPM_SAC_Dist_Global_lm,
         extra = list("R^2", "Dredge_Function" = function(x) {
           s <- summary(x)
           c(adjRsq = s$adj.r.squared)
         }))
SAPM_SAC_Dist_Global_Dredge


importance(SAPM_SAC_Dist_Global_Dredge)

### Getting model output and weights
SAPM_CDist_Dredge_Output <- 
  as.data.frame(cbind(round(SAPM_SAC_Dist_Global_Dredge$`(Intercept)`, 2), 
                      round(SAPM_SAC_Dist_Global_Dredge$CDist_PC1, 2), 
                      round(SAPM_SAC_Dist_Global_Dredge$CDist_PC2, 3), 
                      SAPM_SAC_Dist_Global_Dredge$df,
                      round(SAPM_SAC_Dist_Global_Dredge$`R^2`, 2),
                      round(SAPM_SAC_Dist_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SAPM_SAC_Dist_Global_Dredge$logLik, 2), 
                      round(SAPM_SAC_Dist_Global_Dredge$AICc, 2),
                      round(SAPM_SAC_Dist_Global_Dredge$delta, 2), 
                      SAPM_SAC_Dist_Global_Dredge$weight)) %>% 
  mutate(Species = "SAPM")
colnames(SAPM_CDist_Dredge_Output) = c("Intercept", "CDist_PC1", "CDist_PC2",
                                       "DF", "R^2", "R^2_Adj",
                                       "logLik","AICc", "Delta", "Weights",
                                       "Species")
SAPM_CDist_Dredge_Output

SAPM_CDist_PC2_Weights <- 
  SAPM_CDist_Dredge_Output %>% 
  filter(!(is.na(CDist_PC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", Region = "SAC_Dist", Variable = "CDist_PC2")


SAPM_CDist_Weights <- 
  SAPM_CDist_Dredge_Output %>% 
  filter(!(is.na(CDist_PC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SAPM", Region = "SAC_Dist", Variable = "CDist_PC1") %>% 
  bind_rows(SAPM_CDist_PC2_Weights)
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
SAPM_CDist_Dredge_Output_SAC_Final <- 
  SAPM_CDist_Dredge_Output %>% 
  replace_na(list(CDist_PC1 = "", CDist_PC2 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights),
         Region = "Sacramento") 
SAPM_CDist_Dredge_Output_SAC_Final

write.csv(SAPM_CDist_Dredge_Output_SAC_Final, 
          "Tables/SAPM_CDist_Dredge_Abun_Output.csv", 
          row.names = FALSE)



# SPLT --------------------------------------------------------------------

## Sac Region

head(SPLT_Model_Data_Final)

# Visualizing relationships between dist and covariates 
pairs(SPLT_Model_Data_Final[,c(5,19:20)], pch = 16)

SPLT_SAC_Dist_Global_lm <- 
  lm(log(CDist_SAC) ~ SAC_CDist_PC1 + SAC_CDist_PC2, 
     data = SPLT_Model_Data_Final, 
     na.action = "na.fail")
summary(SPLT_SAC_Dist_Global_lm)

# Model assumptions
plot(SPLT_SAC_Dist_Global_lm) # Normality is questionable
acf(SPLT_SAC_Dist_Global_lm$residuals) # Autocorreltion is fine


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
SPLT_CDist_Dredge_SAC_Output <- 
  as.data.frame(cbind(round(SPLT_SAC_Dist_Global_Dredge$`(Intercept)`, 2),
                      round(SPLT_SAC_Dist_Global_Dredge$SAC_CDist_PC1, 2), 
                      round(SPLT_SAC_Dist_Global_Dredge$SAC_CDist_PC2, 3),
                      SPLT_SAC_Dist_Global_Dredge$df,
                      round(SPLT_SAC_Dist_Global_Dredge$`R^2`, 2),
                      round(SPLT_SAC_Dist_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SPLT_SAC_Dist_Global_Dredge$logLik, 2), 
                      round(SPLT_SAC_Dist_Global_Dredge$AICc, 2),
                      round(SPLT_SAC_Dist_Global_Dredge$delta, 2), 
                      SPLT_SAC_Dist_Global_Dredge$weight)) %>% 
  mutate(Species = "SPLT")
colnames(SPLT_CDist_Dredge_SAC_Output) = c("Intercept", "CDist_PC1", "CDist_PC2",
                                       "DF", "R^2", "R^2_Adj",
                                       "logLik","AICc", "Delta", "Weights",
                                       "Species")
SPLT_CDist_Dredge_SAC_Output

SPLT_SAC_CDist_PC2_Weights <- 
  SPLT_CDist_Dredge_SAC_Output %>% 
  filter(!(is.na(CDist_PC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "SAC_Dist", Variable = "CDist_PC2")

SPLT_SAC_CDist_Weights <- 
  SPLT_CDist_Dredge_SAC_Output %>% 
  filter(!(is.na(CDist_PC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "SAC_Dist", Variable = "CDist_PC1") %>% 
  bind_rows(SPLT_SAC_CDist_PC2_Weights)
SPLT_SAC_CDist_Weights

# Checking weights DF
importance(SPLT_SAC_Dist_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set
SPLT_SAC_CDist_MA = model.avg(SPLT_SAC_Dist_Global_Dredge,
                          cumsum(weight) <= .95,
                          rank = "AIC")
SPLT_SAC_CDist_MA_Sum = summary(SPLT_SAC_CDist_MA)
SPLT_SAC_CDist_MA_Full_Out = SPLT_SAC_CDist_MA_Sum$coefmat.full
SPLT_SAC_CDist_MA_Full_Out


## San Joaquin Region


head(SPLT_Model_Data_Final)

# Visualizing relationships between dist and covariates 
pairs(SPLT_Model_Data_Final[,c(6,22:23)], pch = 16)

SPLT_SJ_Dist_Global_lm <- 
  lm(log(CDist_SJ) ~ SJ_CDist_PC1 + SJ_CDist_PC2, 
     data = SPLT_Model_Data_Final, 
     na.action = "na.fail")
summary(SPLT_SJ_Dist_Global_lm)

# Model assumptions
plot(SPLT_SJ_Dist_Global_lm) # Normality is questionable
acf(SPLT_SJ_Dist_Global_lm$residuals) # Autocorreltion is fine


## All subset selection
SPLT_SJ_Dist_Global_Dredge <- 
  dredge(SPLT_SJ_Dist_Global_lm,
         extra = list("R^2", "Dredge_Function" = function(x) {
           s <- summary(x)
           c(adjRsq = s$adj.r.squared)
         }))
SPLT_SJ_Dist_Global_Dredge
# Top model includes discharge

importance(SPLT_SJ_Dist_Global_Dredge)

### Getting model output and weights
SPLT_CDist_Dredge_SJ_Output <- 
  as.data.frame(cbind(round(SPLT_SJ_Dist_Global_Dredge$`(Intercept)`, 2),
                      round(SPLT_SJ_Dist_Global_Dredge$SJ_CDist_PC1, 2), 
                      round(SPLT_SJ_Dist_Global_Dredge$SJ_CDist_PC2, 3),
                      SPLT_SJ_Dist_Global_Dredge$df,
                      round(SPLT_SJ_Dist_Global_Dredge$`R^2`, 2),
                      round(SPLT_SJ_Dist_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SPLT_SJ_Dist_Global_Dredge$logLik, 2), 
                      round(SPLT_SJ_Dist_Global_Dredge$AICc, 2),
                      round(SPLT_SJ_Dist_Global_Dredge$delta, 2), 
                      SPLT_SJ_Dist_Global_Dredge$weight)) %>% 
  mutate(Species = "SPLT")
colnames(SPLT_CDist_Dredge_SJ_Output) = c("Intercept", "CDist_PC1", "CDist_PC2",
                                           "DF", "R^2", "R^2_Adj",
                                           "logLik","AICc", "Delta", "Weights",
                                           "Species")
SPLT_CDist_Dredge_SJ_Output

SPLT_SJ_CDist_PC2_Weights <- 
  SPLT_CDist_Dredge_SJ_Output %>% 
  filter(!(is.na(CDist_PC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "SJ_Dist", Variable = "CDist_PC2")

SPLT_SJ_CDist_Weights <- 
  SPLT_CDist_Dredge_SJ_Output %>% 
  filter(!(is.na(CDist_PC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SPLT", Region = "SJ_Dist", Variable = "CDist_PC1") %>% 
  bind_rows(SPLT_SJ_CDist_PC2_Weights)
SPLT_SJ_CDist_Weights

# Checking weights DF
importance(SPLT_SJ_Dist_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set
SPLT_SJ_CDist_MA = model.avg(SPLT_SJ_Dist_Global_Dredge,
                              cumsum(weight) <= .95,
                              rank = "AIC")
SPLT_SJ_CDist_MA_Sum = summary(SPLT_SJ_CDist_MA)
SPLT_SJ_CDist_MA_Full_Out = SPLT_SJ_CDist_MA_Sum$coefmat.full
SPLT_SJ_CDist_MA_Full_Out


# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SPLT_CDist_Dredge_Output_SAC_Final <- 
  SPLT_CDist_Dredge_SAC_Output %>% 
  replace_na(list(CDist_PC1 = "", CDist_PC2 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights),
         Region = "Sacramento") 
SPLT_CDist_Dredge_Output_SAC_Final


# Dredge output
SPLT_CDist_Dredge_Output_SJ_Final <- 
  SPLT_CDist_Dredge_SJ_Output %>% 
  replace_na(list(CDist_PC1 = "", CDist_PC2 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights),
         Region = "San Joaquin") 
SPLT_CDist_Dredge_Output_SJ_Final

SPLT_CDist_Dredge_Output_Final <- 
  bind_rows(SPLT_CDist_Dredge_Output_SAC_Final,
            SPLT_CDist_Dredge_Output_SJ_Final)
SPLT_CDist_Dredge_Output_Final


write.csv(SPLT_CDist_Dredge_Output_Final, 
          "Tables/SPLT_CDist_Dredge_Abun_Output.csv", 
          row.names = FALSE)





# SASU --------------------------------------------------------------------

## Sac Region

head(SASU_Model_Data_Final)

# Visualizing relationships between dist and covariates 
pairs(SASU_Model_Data_Final[,c(5,19:20)], pch = 16)

SASU_SAC_Dist_Global_lm <- 
  lm(log(CDist_SAC) ~ SAC_CDist_PC1 + SAC_CDist_PC2, 
     data = SASU_Model_Data_Final, 
     na.action = "na.fail")
summary(SASU_SAC_Dist_Global_lm)

# Model assumptions
plot(SASU_SAC_Dist_Global_lm) # Normality is questionable
acf(SASU_SAC_Dist_Global_lm$residuals) # Autocorreltion is fine


## All subset selection
SASU_SAC_Dist_Global_Dredge <- 
  dredge(SASU_SAC_Dist_Global_lm,
         extra = list("R^2", "Dredge_Function" = function(x) {
           s <- summary(x)
           c(adjRsq = s$adj.r.squared)
         }))
SASU_SAC_Dist_Global_Dredge


importance(SASU_SAC_Dist_Global_Dredge)

### Getting model output and weights
SASU_CDist_Dredge_SAC_Output <- 
  as.data.frame(cbind(round(SASU_SAC_Dist_Global_Dredge$`(Intercept)`, 2),
                      round(SASU_SAC_Dist_Global_Dredge$SAC_CDist_PC1, 2), 
                      round(SASU_SAC_Dist_Global_Dredge$SAC_CDist_PC2, 3),
                      SASU_SAC_Dist_Global_Dredge$df,
                      round(SASU_SAC_Dist_Global_Dredge$`R^2`, 2),
                      round(SASU_SAC_Dist_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SASU_SAC_Dist_Global_Dredge$logLik, 2), 
                      round(SASU_SAC_Dist_Global_Dredge$AICc, 2),
                      round(SASU_SAC_Dist_Global_Dredge$delta, 2), 
                      SASU_SAC_Dist_Global_Dredge$weight)) %>% 
  mutate(Species = "SASU")
colnames(SASU_CDist_Dredge_SAC_Output) = c("Intercept", "CDist_PC1", "CDist_PC2",
                                           "DF", "R^2", "R^2_Adj",
                                           "logLik","AICc", "Delta", "Weights",
                                           "Species")
SASU_CDist_Dredge_SAC_Output

SASU_SAC_CDist_PC2_Weights <- 
  SASU_CDist_Dredge_SAC_Output %>% 
  filter(!(is.na(CDist_PC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "SAC_Dist", Variable = "CDist_PC2")

SASU_SAC_CDist_Weights <- 
  SASU_CDist_Dredge_SAC_Output %>% 
  filter(!(is.na(CDist_PC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "SAC_Dist", Variable = "CDist_PC1") %>% 
  bind_rows(SASU_SAC_CDist_PC2_Weights)
SASU_SAC_CDist_Weights

# Checking weights DF
importance(SASU_SAC_Dist_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set
SASU_SAC_CDist_MA = model.avg(SASU_SAC_Dist_Global_Dredge,
                              cumsum(weight) <= .95,
                              rank = "AIC")
SASU_SAC_CDist_MA_Sum = summary(SASU_SAC_CDist_MA)
SASU_SAC_CDist_MA_Full_Out = SASU_SAC_CDist_MA_Sum$coefmat.full
SASU_SAC_CDist_MA_Full_Out


## San Joaquin Region


head(SASU_Model_Data_Final)

# Visualizing relationships between dist and covariates 
pairs(SASU_Model_Data_Final[,c(6,22:23)], pch = 16)

SASU_SJ_Dist_Global_lm <- 
  lm(log(CDist_SJ) ~ SJ_CDist_PC1 + SJ_CDist_PC2, 
     data = SASU_Model_Data_Final, 
     na.action = "na.fail")
summary(SASU_SJ_Dist_Global_lm)

# Model assumptions
plot(SASU_SJ_Dist_Global_lm) # Normality is questionable
acf(SASU_SJ_Dist_Global_lm$residuals) # Autocorreltion is fine


## All subset selection
SASU_SJ_Dist_Global_Dredge <- 
  dredge(SASU_SJ_Dist_Global_lm,
         extra = list("R^2", "Dredge_Function" = function(x) {
           s <- summary(x)
           c(adjRsq = s$adj.r.squared)
         }))
SASU_SJ_Dist_Global_Dredge
# Top model includes discharge

importance(SASU_SJ_Dist_Global_Dredge)

### Getting model output and weights
SASU_CDist_Dredge_SJ_Output <- 
  as.data.frame(cbind(round(SASU_SJ_Dist_Global_Dredge$`(Intercept)`, 2),
                      round(SASU_SJ_Dist_Global_Dredge$SJ_CDist_PC1, 2), 
                      round(SASU_SJ_Dist_Global_Dredge$SJ_CDist_PC2, 2),
                      SASU_SJ_Dist_Global_Dredge$df,
                      round(SASU_SJ_Dist_Global_Dredge$`R^2`, 2),
                      round(SASU_SJ_Dist_Global_Dredge$`Dredge_Function.adjRsq`, 2),
                      round(SASU_SJ_Dist_Global_Dredge$logLik, 2), 
                      round(SASU_SJ_Dist_Global_Dredge$AICc, 2),
                      round(SASU_SJ_Dist_Global_Dredge$delta, 2), 
                      SASU_SJ_Dist_Global_Dredge$weight)) %>% 
  mutate(Species = "SASU")
colnames(SASU_CDist_Dredge_SJ_Output) = c("Intercept", "CDist_PC1", "CDist_PC2",
                                          "DF", "R^2", "R^2_Adj",
                                          "logLik","AICc", "Delta", "Weights",
                                          "Species")
SASU_CDist_Dredge_SJ_Output

SASU_SJ_CDist_PC2_Weights <- 
  SASU_CDist_Dredge_SJ_Output %>% 
  filter(!(is.na(CDist_PC2))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "SJ_Dist", Variable = "CDist_PC2")

SASU_SJ_CDist_Weights <- 
  SASU_CDist_Dredge_SJ_Output %>% 
  filter(!(is.na(CDist_PC1))) %>% 
  summarise(Weights_Sum = round(sum(Weights), 2)) %>% 
  mutate(Species = "SASU", Region = "SJ_Dist", Variable = "CDist_PC1") %>% 
  bind_rows(SASU_SJ_CDist_PC2_Weights)
SASU_SJ_CDist_Weights

# Checking weights DF
importance(SASU_SJ_Dist_Global_Dredge)

# Model averaged coefficients
# Will be used in figures below
# Going to get a 95% confidence set - can't otherwise PC2 is not included
SASU_SJ_CDist_MA = model.avg(SASU_SJ_Dist_Global_Dredge,
                             rank = "AIC")
SASU_SJ_CDist_MA_Sum = summary(SASU_SJ_CDist_MA)
SASU_SJ_CDist_MA_Full_Out = SASU_SJ_CDist_MA_Sum$coefmat.full
SASU_SJ_CDist_MA_Full_Out


# i. Dredge Output ------------------------------------------------------------------

# Dredge output
SASU_CDist_Dredge_Output_SAC_Final <- 
  SASU_CDist_Dredge_SAC_Output %>% 
  replace_na(list(CDist_PC1 = "", CDist_PC2 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights),
         Region = "Sacramento") 
SASU_CDist_Dredge_Output_SAC_Final


# Dredge output
SASU_CDist_Dredge_Output_SJ_Final <- 
  SASU_CDist_Dredge_SJ_Output %>% 
  replace_na(list(CDist_PC1 = "", CDist_PC2 = "")) %>% 
  mutate(cum_sum_weights = cumsum(Weights),
         Region = "San Joaquin") 
SASU_CDist_Dredge_Output_SJ_Final

SASU_CDist_Dredge_Output_Final <- 
  bind_rows(SASU_CDist_Dredge_Output_SAC_Final,
            SASU_CDist_Dredge_Output_SJ_Final)
SASU_CDist_Dredge_Output_Final


write.csv(SASU_CDist_Dredge_Output_Final, 
          "Tables/SASU_CDist_Dredge_Abun_Output.csv", 
          row.names = FALSE)

# ***Species Combined Weights Output***----------------------------------------------

All_Sp_Abun_Weights_CDist_Output <- 
  bind_rows(SAPM_CDist_Weights,
            SPLT_SAC_CDist_Weights,
            SPLT_SJ_CDist_Weights,
            SASU_SAC_CDist_Weights,
            SASU_SJ_CDist_Weights) %>% 
  mutate(Region = case_when(Region == "SAC_Dist" ~ "Sacramento Distribution",
                            Region == "SJ_Dist" ~ "San Joaquin Distribution"),
         Covariate = case_when(grepl("PC1", Variable) ~ "PC1",
                               grepl("PC2", Variable) ~ "PC2")) %>%
  select(!Variable)
  
# All_Sp_Abun_Weights_CDist_Wide_Output <- 
  pivot_wider(All_Sp_Abun_Weights_CDist_Output,
              names_from = Region,
              values_from = Weights_Sum)


# Final Output that includes index weights
All_Sp_Abun_Weights_Output <- 
  bind_rows(SAPM_DW_Weights,
            SAPM_SAC_Weights,
            SAPM_DELTA_Weights,
            SPLT_DW_Weights,
            SPLT_SAC_Weights,
            SPLT_SJ_Weights,
            SPLT_DELTA_Weights,
            SASU_DW_Weights,
            SASU_SAC_Weights,
            SASU_SJ_Weights,
            SASU_DELTA_Weights,
            SAPM_CDist_Weights,
            SPLT_SAC_CDist_Weights,
            SPLT_SJ_CDist_Weights,
            SASU_SAC_CDist_Weights,
            SASU_SJ_CDist_Weights) %>% 
  mutate(Region = case_when(Region == "DW" ~ "Watershed",
                            Region == "DELTA" ~ "Delta",
                            Region == "SAC" ~ "Sacramento",
                            Region == "SJ" ~ "San Joaquin",
                            Region == "SAC_Dist" ~ "Sacramento Distribution",
                            Region == "SJ_Dist" ~ "San Joaquin Distribution"),
         Covariate = case_when(grepl("PC1", Variable) ~ "PC1",
                               grepl("PC2", Variable) ~ "PC2")) %>%
  select(!Variable)

All_Sp_Abun_Weights_Wide_Output <- 
  pivot_wider(All_Sp_Abun_Weights_Output,
              names_from = Region,
              values_from = Weights_Sum) %>% 
  replace_na(list(`San Joaquin` = "-",
                  `San Joaquin Distribution` = "-"))
All_Sp_Abun_Weights_Wide_Output

write.csv(All_Sp_Abun_Weights_Wide_Output, 
          "Tables/All_Sp_Abun_Weights_Wide_Output.csv", 
          row.names = FALSE)

# ***Figures*** -----------------------------------------------------------------


# i. Abundance ------------------------------------------------------------


# SAPM --------------------------------------------------------------------

head(SAPM_Index_Final)

SAPM_Index_Final_Long <- 
  pivot_longer(SAPM_Index_Final,
               cols = c(Delta_Index, Sacramento_Index, San_Joaquin_Index),
               names_to = "Regions") %>% 
   filter(Regions != "Watershed_Index") %>%
  select(Year, Regions, value)
head(SAPM_Index_Final_Long)


SAPM_Abun_GGPLOT <- 
  ggplot(data= SAPM_Index_Final_Long, aes(x=Year, y=value, fill = Regions)) +
  geom_bar(position="stack", stat = "identity", color = "black") +
  scale_fill_manual(values = c("White", "Grey", "Black"),
                    labels = c("Delta", "Sacramento", "San Joaquin")) +
  labs(x = "", y = "") +
  ggtitle("(A) Sacramento Pikeminnow") +
  scale_x_discrete(expand = c(0, 0), breaks = seq(1995, 2019, by = 2)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
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
               cols = c(Delta_Index, Sacramento_Index, San_Joaquin_Index),
               names_to = "Regions") %>% 
  filter(Regions != "Watershed_Index") %>%
  select(Year, Regions, value)

SPLT_Abun_GGPLOT <- 
  ggplot(data= SPLT_Index_Final_Long, aes(x=Year, y=value, fill = Regions)) +
  geom_bar(position="stack", stat = "identity", color = "black") +
  scale_fill_manual(values = c("White", "Grey", "Black"),
                    labels = c("Delta", "Sacramento", "San Joaquin")) +
  labs(x = "", y = "Abundance Index") +
  ggtitle("(B) Sacramento Splittail") +
  scale_x_discrete(expand = c(0, 0), breaks = seq(1995, 2019, by = 2)) +
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) +
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
               cols = c(Delta_Index, Sacramento_Index, San_Joaquin_Index),
               names_to = "Regions") %>% 
  filter(Regions != "Watershed_Index") %>%
  select(Year, Regions, value)

SASU_Abun_GGPLOT <- 
  ggplot(data= SASU_Index_Final_Long, aes(x=Year, y=value, fill = Regions)) +
  geom_bar(position="stack", stat = "identity", color = "black") +
  scale_fill_manual(values = c("White", "Grey", "Black"),
                    labels = c("Delta", "Sacramento", "San Joaquin")) +
  labs(x = "Year", y = "") +
  scale_x_discrete(expand = c(0, 0), breaks = seq(1995, 2019, by = 2)) +
  scale_y_continuous(limits = c(0, 5), expand = c(0, 0), 
                     breaks = seq(0, 5, by = 1)) +
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
