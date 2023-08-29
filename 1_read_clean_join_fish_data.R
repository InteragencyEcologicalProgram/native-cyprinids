###########################################################
### read_join_data.R ######################################
### Created 8/30/2022 
### Script for reading DJFMP and YBFMP fish data from EDI 
###########################################################

# Filtered condition code <3
# Noticed FL = 0 for plus counts
# No volumes on about 10% of data


library(contentid)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

# Read data ------------------------------
# https://portal.edirepository.org/nis/mapbrowse?packageid=edi.244.9 #djfmp
# https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=233&revision=3

# store cached version
(djfmp_catch_url <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.9&entityid=147fd5e2c7db15913b2ffa44410dc7f9"))
(djfmp_site_url <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.9&entityid=99a038d691f27cd306ff93fdcbc03b77"))
(ybfmp_length_url <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.3&entityid=402732c0e6c782db8b8229c3b9310afa"))
(ybfmp_wq_url <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.3&entityid=4488201fee45953b001f70acf30f7734"))
(ybfmp_sites_url <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.3&entityid=89146f1382d7dfa3bbf3e4b1554eb5cc"))
(ybfmp_catch_url <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.3&entityid=b2b92d9dbfb78cfb1a5716174dfceab1"))
(ybfmp_tax_url <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.3&entityid=405122cb55c6996661c0dee20ab77a6c"))
(ybfmp_sample_url <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.3&entityid=1ca343f66d18c8cde0ec58ad893aad10"))

# cached files (shouldn't have to run the above once cached)
djfmp_catch_file <- contentid::resolve("hash://sha256/b4971a5ca21dc96c8635774d17880ecd552d26cf6eacb77231f659df618a68d1")
djfmp_site_file <- contentid::resolve("hash://sha256/f0f9e66da7415df90a7c0ea4c01938ecadd71ad412af1ccc940e861e63e96ba7")
ybfmp_length_file <- contentid::resolve("hash://sha256/71c3a4667bd3a00859902018ea257c9914b63f1111d276d2cdd960025247c980")
ybfmp_wq_file <- contentid::resolve("hash://sha256/17fc26c768fa24c65016cfe3d4e8d579b909180a7e349386efa7e24e1772def2")
ybfmp_sites_file <- contentid::resolve("hash://sha256/acc9940abf5662e81ee594553e7dc46a05c4cace9c924dbf5352c0544bc7a481")
ybfmp_catch_file <- contentid::resolve("hash://sha256/110b7e3a5cac91b6a2619e2421c7c19f22fb4cc84335b1cc2e5d749080dc9d92")
ybfmp_tax_file <- contentid::resolve("hash://sha256/2d8de80b2549d9f4c5222f54f8f25bb518966899d27119be77cb2bca8786f645")
ybfmp_sample_file <- contentid::resolve("hash://sha256/114456300e69d211865193888a4dc2fc10bfa8121ca0fc78013022173fc062fe")

# read files
  ## see column type designations: https://readr.tidyverse.org/reference/read_delim.html
  ## .default = the default column type, if not specified. I let R guess the rest to save time.
djfmp_catch0 <- read_csv(djfmp_catch_file,
                         col_types = cols(.default = "?",
                                          SampleDate = "D", 
                                          SampleTime = "t", 
                                          Secchi = "d", 
                                          TowNumber = "i", 
                                          SamplingDirection = "i", 
                                          TowDuration = "d",
                                          FlowDebris = "i",
                                          FlowmeterStart = "d",
                                          FlowmeterEnd = "d",
                                          FlowmeterDifference = "d",
                                          Volume = "d",
                                          StageCode = "c",
                                          Expression = "c",
                                          ArchivalID = "c",
                                          GeneticID = "c",
                                          Probability1 = "d",
                                          GeneticID2 = "c",
                                          Probability2 = "d",
                                          SexGeneID = "c",
                                          Ots28 = "c",
                                          Lab = "c",
                                          GeneticTest = "c",
                                          GeneticModel = "c"))
djfmp_sites0 <- read_csv(djfmp_site_file)
ybfmp_length0 <- read_csv(ybfmp_length_file)
ybfmp_wq0 <- read_csv(ybfmp_wq_file)
ybfmp_sites0 <- read_csv(ybfmp_sites_file)
ybfmp_catch0 <- read_csv(ybfmp_catch_file)
ybfmp_tax0 <- read_csv(ybfmp_tax_file)
ybfmp_sample0 <- read_csv(ybfmp_sample_file)
ybfmp_sample <- ybfmp_sample0 %>%
  filter(!is.na(SeineVolume))

# Join data ------------------------------------
## DJFMP -------

# Filter from USFWS list
stations_notincl <- c("SP000E","SP000W","SP001W","SP003E",
                      "SP008E","SA001M","SA004W","SA007E","SA008W","SA009E", "SA010W")

djfmp_catch <- djfmp_catch0 %>% 
  filter(!(StationCode %in% stations_notincl))

djfmp_sample <- djfmp_catch %>%
  mutate(Datetime = paste(SampleDate, SampleTime),
         Datetime = ymd_hms(Datetime)) %>%
 select(Datetime, StationCode) %>%
  distinct() %>%
  mutate(n = 1:nrow(.))
  
# Select columns to keep, create EventID
djfmp_catch2 <- djfmp_catch %>%
  mutate(Datetime = paste(SampleDate, SampleTime),
         Datetime = ymd_hms(Datetime),
         Month = month(Datetime),
         Jday = yday(Datetime)) %>% 
  left_join(djfmp_sample) %>%
  mutate(EventID = paste0("DJFMP_", n)) %>%
  select(Location, RegionCode, EventID, StationCode, Datetime, SampleDate, Month, Jday, 
         MethodCode, GearConditionCode,
         WeatherCode, DO, WaterTemp, Turbidity, Secchi, SpecificConductance, 
         FlowDebris, SiteDisturbance, AlternateSite, 
         Volume, IEPFishCode, ForkLength, Count) 

### length frequency ---------------------

# filter to table of fish with length data and calculate sum caught at each event/length
djfmp_length_sum <- djfmp_catch2 %>% 
  filter(ForkLength>0) %>%
  group_by(EventID, Volume, IEPFishCode, ForkLength)  %>%
  summarise(LengthFrequency = sum(Count)) %>%
  ungroup()

# calculate total count of those measured per event
djfmp_catchtotal <- djfmp_catch2 %>%
  group_by(Location, RegionCode, EventID, StationCode, Datetime, SampleDate, Month, 
           Jday, MethodCode, GearConditionCode,
           WeatherCode, DO, WaterTemp, Turbidity, Secchi, SpecificConductance, 
           FlowDebris, SiteDisturbance, AlternateSite, 
           Volume, IEPFishCode) %>%
  summarise(TotalCount = sum(Count))

# Merge the rest of the catch, and apply below to get CountAdj
# CountAdj = n at FL/n measured * total catch
djfmp_catchlength <- djfmp_length_sum %>%
  group_by(EventID, Volume, IEPFishCode) %>%
  mutate(TotalMeasured = sum(LengthFrequency, na.rm = TRUE)) %>%
  ungroup() %>%
  right_join(djfmp_catchtotal) %>%
  mutate(CountAdj = round((LengthFrequency/TotalMeasured)* TotalCount,2)) %>%
            select(Location, RegionCode, EventID, StationCode, Datetime, SampleDate, 
                   Month, Jday, MethodCode, GearConditionCode,
         WeatherCode, DO, WaterTemp, Turbidity, Secchi, SpecificConductance, 
         FlowDebris, SiteDisturbance, AlternateSite, 
         Volume, IEPFishCode, ForkLength, CountAdj) 
  

# fill in zeroes for all instances of fish not caught
djfmp_all <- djfmp_catchlength %>% 
  complete(nesting(EventID, Location, RegionCode, StationCode, Datetime, SampleDate, 
                   Month, Jday, MethodCode, GearConditionCode,
                   WeatherCode, DO, WaterTemp, Turbidity, Secchi, SpecificConductance, 
                   FlowDebris, SiteDisturbance, AlternateSite, 
                   Volume),
          IEPFishCode, 
          fill = list(CountAdj = 0, ForkLength = 0))

# filter to cyprinids of interest only
djfmp_cyprinids <- filter(djfmp_all, IEPFishCode %in% c("SACSUC", "SACPIK", "SPLITT",
                                                        "COMCAR", "REDSHI", "GOLDSHI"))

# original study filtered to gear condition 1 and removed all seines that did not match current SOP, we chose not to do that here

## YBFMP -------

### length frequency ---------------------

#combine ybfmp length and wq tables
ybfmp_lenwq <- left_join(ybfmp_length0, ybfmp_wq0, by="EventID")
ybfmp_lenwqeffort <- left_join(ybfmp_lenwq, ybfmp_sample0, by = "EventID") 

# calculate total counts by event/organism/forklength in length table
ybfmp_length_sum <- ybfmp_lenwqeffort %>%
  mutate(CountL = 1) %>%
  group_by(EventID, SeineVolume, OrganismCode, ForkLength) %>%
  summarise(LengthFrequency = sum(CountL)) %>%
  ungroup()

# Now calculate total by event/organism, join to larger catch total, and apply proportional fork length catch to total catch
# CountAdj = n at FL/n measured * total catch
# Fix instances where there was nothing measured (either because no catch at all or just none measured)
# Make FL = 0 if currently NA to match DJFMP notation
ybfmp_catchlength <- ybfmp_length_sum %>%
  group_by(EventID, OrganismCode) %>%
  mutate(TotalMeasured = sum(LengthFrequency, na.rm = TRUE)) %>%
  ungroup() %>%
  right_join(ybfmp_catch0) %>%
  mutate(CountAdj = round((LengthFrequency/TotalMeasured)* Count,2)) %>%
  mutate(CountAdj = case_when(Count == 0 ~ 0,
                              is.na(ForkLength) & Count>0~ Count,
                              TRUE~CountAdj),
         ForkLength = case_when(is.na(ForkLength) ~ 0,
                                TRUE ~ ForkLength))


### filter and clean --------------
# beach seine only, add IEPFishCode to standardize with USFWS, add variables in DJFMP, select columns
ybfmp_seine0 <- left_join(ybfmp_catchlength, ybfmp_wq0, by = "EventID") %>%
  filter(MethodCode == "BSEIN") %>%
  left_join(ybfmp_tax0 %>% select(OrganismCode, IEPFishCode)) %>%
  left_join(ybfmp_sample %>% select(EventID, Volume=SeineVolume)) %>%
  mutate(Datetime = mdy_hm(Datetime),
        SampleDate = mdy(SampleDate),
        Month = month(SampleDate),
        Jday = yday(SampleDate),
         Location = "Yolo Bypass",
         RegionCode = NA,
         FlowDebris = NA, 
         SiteDisturbance = NA, 
         AlternateSite = NA,
        EventID = paste0("YBFMP_", EventID)) %>%
  select(Location, RegionCode, EventID, StationCode, Datetime, SampleDate, Month, 
         Jday, MethodCode, GearCode, GearConditionCode,
         WeatherCode, DO, WaterTemp, Turbidity, Secchi, SpecificConductance, 
         FlowDebris, SiteDisturbance, AlternateSite, 
         Volume, IEPFishCode, ForkLength, CountAdj) 

# Filter out alternate gears used only in early couple years of the program
gear_notincl_yb <- c("SEINENCL", "SEINCOVE", "SEIN30", "SEIN100")

ybfmp_seine <- ybfmp_seine0 %>% 
  filter(!(GearCode %in% gear_notincl_yb))

# Filter sites 
  ## LIS - total of 137 samples, discontinued after 2012 - keep
  ## YB - total of 275 samples, fairly consistent until 2017 but not connected to system unless flooded - remove
  ## CCS - Cache Slough - remove
  ## PCS - only 9 total samples, possibly BL1 recorded incorrectly - remove
  ## FW1, SB1, LIHF, RD22, LIHFS, YBI80, SB2 - all inundation sites not regularly sampled - remove
  ## BL 1, 2, 4 and AL 1 sampled since 1998 - keep
  ## BL 3 & 5 sampled since 2007 - keep
  ## AL 3 sampled since 2005 - keep
  ## AL 2 & 4 sampled since 2010, AL 2 discontinued after 2017 - keep
  ## BL6 sampled during 2015 & 2016 to help fill gap while unable to sample BL5 - keep

stations_notincl_yb <- c("YB", "CCS1", "CCS2", "CCS3", "CCS4", "PCS", "FW1", "SB1", "LIHF", 
                         "RD22", "LIHFS", "YBI80", "SB2")

ybfmp_seine1 <- ybfmp_seine %>% 
  filter(!(StationCode %in% stations_notincl_yb))

# filter to cyprinids of interest only
ybfmp_cyprinids <- filter(ybfmp_seine1, IEPFishCode %in% 
                            c("SACSUC", "SACPIK", "SPLITT", "COMCAR", "REDSHI",
                              "GOLDSHI"))
summary(ybfmp_cyprinids)

## Combine -------

# combine and remove Condition Code <3
allfmp_catch0 <- bind_rows(djfmp_cyprinids, ybfmp_cyprinids) %>%
  filter(GearConditionCode<3) 

# filter to 1995+
allfm_catch <- allfmp_catch0 %>% 
  mutate(Year=lubridate::year(SampleDate),
         Program=sub("(.*)_(.*)","\\1",EventID)) %>%  
  filter(1995 <= Year & Year <= 2019)


## It looks like the YBFMP didn't get the "tidyr::complete" treatment that DJFMP 
## did, so golden shiner samples are lost during filtering on species in the 
## "calculate indices" file. Use complete here at the end to handle this.
allfm_catch_complete <- allfm_catch %>%
  complete(nesting(EventID, Location, RegionCode, StationCode, Datetime, SampleDate, 
                   Month, Jday, MethodCode, GearConditionCode,
                   WeatherCode, DO, WaterTemp, Turbidity, Secchi, SpecificConductance, 
                   FlowDebris, SiteDisturbance, AlternateSite, 
                   Volume, GearCode, Year, Program),
           IEPFishCode, 
           fill = list(CountAdj = 0, ForkLength = 0))


# Write --------
saveRDS(allfm_catch_complete, "data_clean/seine_djfmp_ybfmp.rds")
