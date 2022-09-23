library(lubridate)
library(dplyr)
library(psych)

### Zooplankton data
# Longest time series is from the EMP data...if I used CDFW townet/20 mm survey data I would have to cut ~ 10 years of data



### SPLT
# Used catch of fish in months 5 & 6 to create age-0 abundance indicies
# Spawn from late Feb through early May
# ~10 days after hatching they begin exogenous feeding
# Starting feeding on small rotifers and switch to small crustaceans and then dipterans as they grow larger (Splittail WPaper)
# Kurth and Nobriga 2001 (IEP Newsletter) found that larval SPLT fed on cladocerans (56% of diet by dry weight), chironomid larvae (40%) and copepods (4%)
# Rotifers comprised <1%. Other items encountered infrequently included diatoms, detritus, and terrestrial insects
# Daniels and Moyle 1983: SPLT 50-100 mm SL consume primarily detritus but also feed on harpactacoid and calanoid copepods
# Use Zooplankton data from May and June to determine how they influence age-0 abundance indicies


#### Zooplankton Data
Zoop_Raw = read.csv("data/1972-2017CBMatrix.csv", header = T)
head(Zoop_Raw)
str(Zoop_Raw)
Zoop_Raw$Month = month(as.POSIXlt(Zoop_Raw$Date, format = "%m/%d/%Y"))


### SPLT
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

Zoop_SPLT_SJ_Year_PC = principal(Zoop_SPLT_SJ_Year_Matrix, nfactors = 2, rotate = "none") # Not using a rotation because I am trying to 
# create a single index of zooplankton abuandance 
Zoop_SPLT_SJ_Year_PC


Zoop_SPLT_SJ_PC_Scores = as.data.frame(Zoop_SPLT_SJ_Year_PC$scores)


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

Zoop_SPLT_SAC_Year_PC = principal(Zoop_SPLT_SAC_Year_Matrix, nfactors = 2, rotate = "none") # Not using a rotation because I am trying to 
# create a single index of zooplankton abuandance 
Zoop_SPLT_SAC_Year_PC


Zoop_SPLT_SAC_PC_Scores = as.data.frame(Zoop_SPLT_SAC_Year_PC$scores)

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

Zoop_SPLT_DELTA_Year_PC = principal(Zoop_SPLT_DELTA_Year_Matrix, nfactors = 2, rotate = "none") # Not using a rotation because I am trying to 
# create a single index of zooplankton abuandance 
Zoop_SPLT_DELTA_Year_PC


Zoop_SPLT_DELTA_PC_Scores = as.data.frame(Zoop_SPLT_DELTA_Year_PC$scores)


######## SAPM
# Based on Nobriga et al. 2006 - For SAPM <150 mm most common prey items were Annelids, Corophiid and Gammarid amphipods. 
# SAPM <150 mm did not consume a lot of fish.
# Got benthic invert data from Betsy Wells
# Used two stations from data; D24-L: Sacramento River downstream of Rio Vista bridge (left); P8-R: San Joaquin River at Buckley Cove (right)
# D24-L doesn't line up well with stations used to make abundance indicies...may have to use other stations that are closer but
# should wait until find the station that we are going to use for discharge first. 

SAPM_Diet_SJ = read.csv("data/SAPM_Diet_SJ.csv")
head(SAPM_Diet_SJ)

numMonth <- function(x) {
  months <- list(january=1,february=2,march=3,april=4,may=5,june=6,july=7,august=8,september=9,october=10,november=11,december=12)
  x <- tolower(x)
  sapply(x,function(x) months[[x]])
}

SAPM_Diet_SJ$Month = as.factor(numMonth(SAPM_Diet_SJ$Month))
SAPM_Diet_SJ$Year = as.factor(SAPM_Diet_SJ$Year)
str(SAPM_Diet_SJ)
tail(SAPM_Diet_SJ)

# Used months 3 through 7 for SAPM index and have years 1996 - 2017
SAPM_Diet_SJ = SAPM_Diet_SJ[SAPM_Diet_SJ$Month %in% c(3:7) & SAPM_Diet_SJ$Year %in% c(1996:2017),]
head(SAPM_Diet_SJ)
tail(SAPM_Diet_SJ)
str(SAPM_Diet_SJ)
SAPM_Diet_SJ$Year = droplevels(SAPM_Diet_SJ$Year)

# Creating a mean per group per year
SAPM_Diet_SJ_Year_Agg <- 
  as.data.frame(SAPM_Diet_SJ %>% 
                  group_by(Year) %>% 
                  summarise_at(c("Annelida", "Corophiidae", "Gammaridae"), mean, na.rm = TRUE))
head(SAPM_Diet_SJ_Year_Agg)
str(SAPM_Diet_SJ_Year_Agg)
unique(SAPM_Diet_SJ_Year_Agg$Year)
SAPM_Diet_SJ_Year_Agg$Year = as.factor(SAPM_Diet_SJ_Year_Agg$Year)


Diet_SAPM_SJ_Year_Matrix = SAPM_Diet_SJ_Year_Agg[, -c(1)]
head(Diet_SAPM_SJ_Year_Matrix)

Diet_SAPM_SJ_Year_PC = principal(Diet_SAPM_SJ_Year_Matrix, nfactors = 2, rotate = "none") # Not using a rotation because I am trying to 
# create a single index of Dietlankton abuandance 
Diet_SAPM_SJ_Year_PC


Diet_SAPM_SJ_PC_Scores = as.data.frame(Diet_SAPM_SJ_Year_PC$scores)
colnames(Diet_SAPM_SJ_PC_Scores) <- c("SJ_PC1", "SJ_PC2")


SAPM_Index_Final = read.csv("SAPM_Index_Final.csv")
SAPM_Index_Final
SAPM_Index_Final_Abr = SAPM_Index_Final[SAPM_Index_Final$Year %in% c(1996:2017),]

summary(lm(SAPM_Index_Final_Abr$San_Joaquin_River_Index ~ SAPM_SJ_PC_Scores$RC3))
