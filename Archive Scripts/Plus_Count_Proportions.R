library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)
options(scipen=999)
windowsFonts(Times = windowsFont("Times New Roman"))

setwd("M:/DJFMP Not IEP/Native Cypriniform Study/Data_Analyses")


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

# Indicies
# 1) Calculate mean May and June CPUE for each station for SPLT and SASU (and March through July for SAPM)
# 2) Averaging average of May and June CPUE for each subarea
# 3) Averaging, average of average of May and June CPUE for each station within a subarea
# 4) Sum subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River to get final index



#### SPLT 

SPLT_Catch_Effort = read.csv("data/SPLT_Catch_Effort.csv")
SPLT_Catch_Effort$SampleID = as.factor(SPLT_Catch_Effort$SampleID)
SPLT_Catch_Effort$Month = as.factor(SPLT_Catch_Effort$Month)
SPLT_Catch_Effort$subarea = as.factor(SPLT_Catch_Effort$subarea)
SPLT_Catch_Effort$Year = as.factor(SPLT_Catch_Effort$Year)
SPLT_Catch_Effort$GearConditionCode = as.factor(SPLT_Catch_Effort$GearConditionCode)
str(SPLT_Catch_Effort)
head(SPLT_Catch_Effort, 10)

# Using months 5 and 6 for SPLT
SPLT_Catch_Effort = SPLT_Catch_Effort[SPLT_Catch_Effort$Month %in% c(5,6),]
SPLT_Catch_Effort_May = SPLT_Catch_Effort[SPLT_Catch_Effort$Month == 5,]
SPLT_Catch_Effort_June = SPLT_Catch_Effort[SPLT_Catch_Effort$Month == 6,]

# May
SPLT_Plus_Count_May = aggregate(SumOfSumOfCatchCount ~ SampleID, data = SPLT_Catch_Effort_May[SPLT_Catch_Effort_May$ForkLength == 0,], sum)
head(SPLT_Plus_Count_May)

SPLT_FL_Count_May = aggregate(SumOfSumOfCatchCount ~ SampleID, data = SPLT_Catch_Effort_May[SPLT_Catch_Effort_May$ForkLength > 0,], sum)
head(SPLT_FL_Count_May)

SPLT_Total_May = full_join(SPLT_FL_Count_May, SPLT_Plus_Count_May,  by = "SampleID")
colnames(SPLT_Total_May) = c("SampleID", "FL_Count", "Plus_Count")
SPLT_Total_May[is.na(SPLT_Total_May)] = 0
head(SPLT_Total_May)

SPLT_Total_May$Total_Count = SPLT_Total_May$FL_Count + SPLT_Total_May$Plus_Count
head(SPLT_Total_May)

SPLT_FL_Range_May = aggregate(SumOfSumOfCatchCount ~ SampleID , data = SPLT_Catch_Effort_May[SPLT_Catch_Effort_May$ForkLength > 24 & SPLT_Catch_Effort_May$ForkLength < 85,], sum)
head(SPLT_FL_Range_May)

SPLT_Total_May_Final = left_join(SPLT_Total_May, SPLT_FL_Range_May, by = "SampleID")
colnames(SPLT_Total_May_Final) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
head(SPLT_Total_May_Final)

                                
# June
SPLT_Plus_Count_June = aggregate(SumOfSumOfCatchCount ~ SampleID, data = SPLT_Catch_Effort_June[SPLT_Catch_Effort_June$ForkLength == 0,], sum)
head(SPLT_Plus_Count_June)

SPLT_FL_Count_June = aggregate(SumOfSumOfCatchCount ~ SampleID, data = SPLT_Catch_Effort_June[SPLT_Catch_Effort_June$ForkLength > 0,], sum)
head(SPLT_FL_Count_June)

SPLT_Total_June = full_join(SPLT_FL_Count_June, SPLT_Plus_Count_June,  by = "SampleID")
colnames(SPLT_Total_June) = c("SampleID", "FL_Count", "Plus_Count")
SPLT_Total_June[is.na(SPLT_Total_June)] = 0
head(SPLT_Total_June)

SPLT_Total_June$Total_Count = SPLT_Total_June$FL_Count + SPLT_Total_June$Plus_Count
head(SPLT_Total_June)

SPLT_FL_Range_June = aggregate(SumOfSumOfCatchCount ~ SampleID , data = SPLT_Catch_Effort_June[SPLT_Catch_Effort_June$ForkLength > 24 & SPLT_Catch_Effort_June$ForkLength < 105,], sum)
head(SPLT_FL_Range_June)

SPLT_Total_June_Final = left_join(SPLT_Total_June, SPLT_FL_Range_June, by = "SampleID")
colnames(SPLT_Total_June_Final) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
head(SPLT_Total_June_Final)                              

# Combining may and june data
SPLT_Adj_Final = rbind(SPLT_Total_May_Final, SPLT_Total_June_Final)
head(SPLT_Adj_Final)

SPLT_Adj_Final$Adjusted_Count = SPLT_Adj_Final$Total_Count * (SPLT_Adj_Final$FL_Range_Count/SPLT_Adj_Final$FL_Count)
head(SPLT_Adj_Final)
SPLT_Adj_Final[is.na(SPLT_Adj_Final)] = 0

SPLT_Index = full_join(subset(SPLT_Catch_Effort, !duplicated(SampleID)), SPLT_Adj_Final, by = "SampleID")
head(SPLT_Index)
SPLT_Index[,15:21][is.na(SPLT_Index[,15:21])] = 0
head(SPLT_Index)

SPLT_Index$CPUE = SPLT_Index$Adjusted_Count/SPLT_Index$SeineVolume
head(SPLT_Index)

# Indicies

# Calculating mean May and June CPUE for each station 
Mean.CPUE.Station.Month.SPLT = aggregate(CPUE ~ StationCode + Year + Month + subarea, data = SPLT_Index, mean, na.action = na.omit)
head(Mean.CPUE.Station.Month.SPLT, n = 50)

# Averaging average of May and June CPUE for each subarea
Mean.CPUE.Station.SPLT = aggregate(CPUE ~ Year + Month + subarea , data = Mean.CPUE.Station.Month.SPLT, mean, na.action = na.omit)
head(Mean.CPUE.Station.SPLT, n = 50)

# Averaging, average of average of May and June CPUE for each station within a subarea
Mean.Catch.Subarea.SPLT = aggregate(CPUE ~ Year +  subarea , data = Mean.CPUE.Station.SPLT, mean, na.action = na.omit)
head(Mean.Catch.Subarea.SPLT, n = 50)

SPLT_Index_Final = reshape(Mean.Catch.Subarea.SPLT, idvar = "Year", timevar = "subarea", direction = "wide")

# Summing subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River
SPLT_Index_Final$Delta_Index = rowSums(SPLT_Index_Final[,2:6])
SPLT_Index_Final$Sac_River_Index = rowSums(SPLT_Index_Final[,7:9], na.rm = T) # na.rm = T because we are no longer sampling in subarea 8
SPLT_Index_Final$San_Joaquin_River_Index = rowSums(SPLT_Index_Final[,10:11])


SPLT_Index_Final
write.csv(SPLT_Index_Final, "SPLT_Index_Final.csv")
SPLT_Index_Final = read.csv("SPLT_Index_Final.csv")

SPLT_SJ_GGPLOT = ggplot(data= SPLT_Index_Final, aes(x=Year, y=San_Joaquin_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous( expand = c(0, 0)) +
  theme_classic()
SPLT_SJ_GGPLOT

SPLT_Sac_GGPLOT = ggplot(data= SPLT_Index_Final, aes(x=Year, y=Sac_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,4), expand = c(0, 0)) +
  theme_classic()
SPLT_Sac_GGPLOT


# Output for SPLT plots
png(file = "SPLT.Indicies.png", width = 1500, height = 1850, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SPLT_Sac_GGPLOT, SPLT_SJ_GGPLOT,  ncol = 1,
                                   top =textGrob("SPLT", vjust=2,gp = gpar(fontsize=25,   fontfamily="Times")))))

dev.off()

#### SASU

SASU_Catch_Effort = read.csv("data/SASU_Catch_Effort.csv")
SASU_Catch_Effort$SampleID = as.factor(SASU_Catch_Effort$SampleID)
SASU_Catch_Effort$Month = as.factor(SASU_Catch_Effort$Month)
SASU_Catch_Effort$subarea = as.factor(SASU_Catch_Effort$subarea)
SASU_Catch_Effort$Year = as.factor(SASU_Catch_Effort$Year)
SASU_Catch_Effort$GearConditionCode = as.factor(SASU_Catch_Effort$GearConditionCode)
str(SASU_Catch_Effort)
head(SASU_Catch_Effort, 10)

# Using months 5 and 6 for SASU; SASU catch peaks in both months
SASU_Catch_Effort = SASU_Catch_Effort[SASU_Catch_Effort$Month %in% c(5,6),]
SASU_Catch_Effort_May = SASU_Catch_Effort[SASU_Catch_Effort$Month == 5,]
SASU_Catch_Effort_June = SASU_Catch_Effort[SASU_Catch_Effort$Month == 6,]

# May
SASU_Plus_Count_May = aggregate(SumOfSumOfCatchCount ~ SampleID, data = SASU_Catch_Effort_May[SASU_Catch_Effort_May$ForkLength == 0,], sum)
head(SASU_Plus_Count_May)

SASU_FL_Count_May = aggregate(SumOfSumOfCatchCount ~ SampleID, data = SASU_Catch_Effort_May[SASU_Catch_Effort_May$ForkLength > 0,], sum)
head(SASU_FL_Count_May)

SASU_Total_May = full_join(SASU_FL_Count_May, SASU_Plus_Count_May,  by = "SampleID")
colnames(SASU_Total_May) = c("SampleID", "FL_Count", "Plus_Count")
SASU_Total_May[is.na(SASU_Total_May)] = 0
head(SASU_Total_May)

SASU_Total_May$Total_Count = SASU_Total_May$FL_Count + SASU_Total_May$Plus_Count
head(SASU_Total_May)

# Using same length cutoff as SPLT for age 0 fish in May; Most fish are below cutoff
SASU_FL_Range_May = aggregate(SumOfSumOfCatchCount ~ SampleID , data = SASU_Catch_Effort_May[SASU_Catch_Effort_May$ForkLength > 24 & SASU_Catch_Effort_May$ForkLength < 85,], sum)
head(SASU_FL_Range_May)

SASU_Total_May_Final = left_join(SASU_Total_May, SASU_FL_Range_May, by = "SampleID")
colnames(SASU_Total_May_Final) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
head(SASU_Total_May_Final)


# June
SASU_Plus_Count_June = aggregate(SumOfSumOfCatchCount ~ SampleID, data = SASU_Catch_Effort_June[SASU_Catch_Effort_June$ForkLength == 0,], sum)
head(SASU_Plus_Count_June)

SASU_FL_Count_June = aggregate(SumOfSumOfCatchCount ~ SampleID, data = SASU_Catch_Effort_June[SASU_Catch_Effort_June$ForkLength > 0,], sum)
head(SASU_FL_Count_June)

SASU_Total_June = full_join(SASU_FL_Count_June, SASU_Plus_Count_June,  by = "SampleID")
colnames(SASU_Total_June) = c("SampleID", "FL_Count", "Plus_Count")
SASU_Total_June[is.na(SASU_Total_June)] = 0
head(SASU_Total_June)

SASU_Total_June$Total_Count = SASU_Total_June$FL_Count + SASU_Total_June$Plus_Count
head(SASU_Total_June)

# Using same length cutoff as SPLT for age 0 fish in June; Most of the fish are below cutoff
SASU_FL_Range_June = aggregate(SumOfSumOfCatchCount ~ SampleID , data = SASU_Catch_Effort_June[SASU_Catch_Effort_June$ForkLength > 24 & SASU_Catch_Effort_June$ForkLength < 105,], sum)
head(SASU_FL_Range_June)

SASU_Total_June_Final = left_join(SASU_Total_June, SASU_FL_Range_June, by = "SampleID")
colnames(SASU_Total_June_Final) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
head(SASU_Total_June_Final)                              


# Combining months 
SASU_Adj_Final = rbind(SASU_Total_May_Final, SASU_Total_June_Final)
head(SASU_Adj_Final)

SASU_Adj_Final$Adjusted_Count = SASU_Adj_Final$Total_Count * (SASU_Adj_Final$FL_Range_Count/SASU_Adj_Final$FL_Count)
head(SASU_Adj_Final)
SASU_Adj_Final[is.na(SASU_Adj_Final)] = 0

SASU_Index = full_join(subset(SASU_Catch_Effort, !duplicated(SampleID)), SASU_Adj_Final, by = "SampleID")
head(SASU_Index)
SASU_Index[,15:21][is.na(SASU_Index[,15:21])] = 0
head(SASU_Index)

SASU_Index$CPUE = SASU_Index$Adjusted_Count/SASU_Index$SeineVolume
head(SASU_Index)

# Indicies

# Calculating mean May and June CPUE for each station 
Mean.CPUE.Station.Month.SASU = aggregate(CPUE ~ StationCode + Year + Month + subarea, data = SASU_Index, mean, na.action = na.omit)
head(Mean.CPUE.Station.Month.SASU, n = 50)

# Averaging average of May and June CPUE for each subarea
Mean.CPUE.Station.SASU = aggregate(CPUE ~ Year + Month + subarea , data = Mean.CPUE.Station.Month.SASU, mean, na.action = na.omit)
head(Mean.CPUE.Station.SASU, n = 50)

# Averaging, average of average of May and June CPUE for each station within a subarea
Mean.Catch.Subarea.SASU = aggregate(CPUE ~ Year +  subarea , data = Mean.CPUE.Station.SASU, mean, na.action = na.omit)
head(Mean.Catch.Subarea.SASU, n = 50)

SASU_Index_Final = reshape(Mean.Catch.Subarea.SASU, idvar = "Year", timevar = "subarea", direction = "wide")

# Summing subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River
SASU_Index_Final$Delta_Index = rowSums(SASU_Index_Final[,2:6])
SASU_Index_Final$Sac_River_Index = rowSums(SASU_Index_Final[,7:9], na.rm = T) # na.rm = T because we are no longer sampling in subarea 8
SASU_Index_Final$San_Joaquin_River_Index = rowSums(SASU_Index_Final[,10:11])


SASU_Index_Final
write.csv(SASU_Index_Final, "SASU_Index_Final.csv")
SASU_Index_Final = read.csv("SASU_Index_Final.csv")

SASU_SJ_GGPLOT = ggplot(data= SASU_Index_Final, aes(x=Year, y=San_Joaquin_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,.8), expand = c(0, 0)) +
  theme_classic()
SASU_SJ_GGPLOT

SASU_Sac_GGPLOT = ggplot(data= SASU_Index_Final, aes(x=Year, y=Sac_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,10), expand = c(0, 0)) +
  theme_classic()
SASU_Sac_GGPLOT


# Output for SASU plots
png(file = "SASU.Indicies.png", width = 1500, height = 1850, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SASU_Sac_GGPLOT, SASU_SJ_GGPLOT,  ncol = 1,
                                   top =textGrob("SASU", vjust=2,gp = gpar(fontsize=25,   fontfamily="Times")))))         

dev.off()
#### SAPM

SAPM_Catch_Effort = read.csv("data/SAPM_Catch_Effort.csv")
SAPM_Catch_Effort$SampleID = as.factor(SAPM_Catch_Effort$SampleID)
SAPM_Catch_Effort$Month = as.factor(SAPM_Catch_Effort$Month)
SAPM_Catch_Effort$subarea = as.factor(SAPM_Catch_Effort$subarea)
SAPM_Catch_Effort$Year = as.factor(SAPM_Catch_Effort$Year)
SAPM_Catch_Effort$GearConditionCode = as.factor(SAPM_Catch_Effort$GearConditionCode)
str(SAPM_Catch_Effort)
head(SAPM_Catch_Effort, 10)

# Using months 3 through 7 for SAPM, catch does not peak in months 5 and 6 like SPLT and SASU
# Going to use length cutoffs (min size for age 1 fish) taken from CDFW for SPLT index calculations
SAPM_Catch_Effort = SAPM_Catch_Effort[SAPM_Catch_Effort$Month %in% c(3:7),]
SAPM_Catch_Effort_March = SAPM_Catch_Effort[SAPM_Catch_Effort$Month == 3,]
SAPM_Catch_Effort_April = SAPM_Catch_Effort[SAPM_Catch_Effort$Month == 4,]
SAPM_Catch_Effort_May = SAPM_Catch_Effort[SAPM_Catch_Effort$Month == 5,]
SAPM_Catch_Effort_June = SAPM_Catch_Effort[SAPM_Catch_Effort$Month == 6,]
SAPM_Catch_Effort_July = SAPM_Catch_Effort[SAPM_Catch_Effort$Month == 7,]

# Length Freq Analyses to determine what months will be included in the analyses

# March
SAPM_Catch_March = SAPM_Catch_Effort_March[SAPM_Catch_Effort_March$OrganismCode == "SAPM",]
head(SAPM_Catch_March)

SAPM_March_FL = ggplot(SAPM_Catch_March[SAPM_Catch_March$Year == 1995,], aes(x =ForkLength, y = X2016.SacM)) + 
  geom_bar(stat = "identity", fill = "black") +
  ggtitle("(A) Sacramento Trawl Site (MWTR) 2016") +
  labs(x = "", y = "") +
  scale_y_continuous(limits = c(0,50), expand = c(0,0), breaks = scales::pretty_breaks(n = 5)) +
  theme_classic(base_size = 15) + 
  theme(axis.text = element_text(size = 12, colour = "black", family = "Times")) +
  theme(axis.line = element_line(colour = "black", size = .65)) +
  theme(axis.ticks.y = element_line(size = .65)) +
  theme(plot.title = element_text(face = "bold", hjust = .5, family = "Times")) +
  theme(legend.text = element_text(family = "Times"), legend.title = element_text(family = "Times")) +
  theme(axis.title = element_text(family = "Times"))





# March
SAPM_Plus_Count_March = aggregate(SumOfSumOfCatchCount ~ SampleID, data = SAPM_Catch_Effort_March[SAPM_Catch_Effort_March$ForkLength == 0,], sum)
head(SAPM_Plus_Count_March)

SAPM_FL_Count_March = aggregate(SumOfSumOfCatchCount ~ SampleID, data = SAPM_Catch_Effort_March[SAPM_Catch_Effort_March$ForkLength > 0,], sum)
head(SAPM_FL_Count_March)

SAPM_Total_March = full_join(SAPM_FL_Count_March, SAPM_Plus_Count_March,  by = "SampleID")
colnames(SAPM_Total_March) = c("SampleID", "FL_Count", "Plus_Count")
SAPM_Total_March[is.na(SAPM_Total_March)] = 0
head(SAPM_Total_March)

SAPM_Total_March$Total_Count = SAPM_Total_March$FL_Count + SAPM_Total_March$Plus_Count
head(SAPM_Total_March)

SAPM_FL_Range_March = aggregate(SumOfSumOfCatchCount ~ SampleID , data = SAPM_Catch_Effort_March[SAPM_Catch_Effort_March$ForkLength > 24 & SAPM_Catch_Effort_March$ForkLength < 50,], sum)
head(SAPM_FL_Range_March)

SAPM_Total_March_Final = left_join(SAPM_Total_March, SAPM_FL_Range_March, by = "SampleID")
colnames(SAPM_Total_March_Final) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
head(SAPM_Total_March_Final)

# April
SAPM_Plus_Count_April = aggregate(SumOfSumOfCatchCount ~ SampleID, data = SAPM_Catch_Effort_April[SAPM_Catch_Effort_April$ForkLength == 0,], sum)
head(SAPM_Plus_Count_April)

SAPM_FL_Count_April = aggregate(SumOfSumOfCatchCount ~ SampleID, data = SAPM_Catch_Effort_April[SAPM_Catch_Effort_April$ForkLength > 0,], sum)
head(SAPM_FL_Count_April)

SAPM_Total_April = full_join(SAPM_FL_Count_April, SAPM_Plus_Count_April,  by = "SampleID")
colnames(SAPM_Total_April) = c("SampleID", "FL_Count", "Plus_Count")
SAPM_Total_April[is.na(SAPM_Total_April)] = 0
head(SAPM_Total_April)

SAPM_Total_April$Total_Count = SAPM_Total_April$FL_Count + SAPM_Total_April$Plus_Count
head(SAPM_Total_April)

SAPM_FL_Range_April = aggregate(SumOfSumOfCatchCount ~ SampleID , data = SAPM_Catch_Effort_April[SAPM_Catch_Effort_April$ForkLength > 24 & SAPM_Catch_Effort_April$ForkLength < 70,], sum)
head(SAPM_FL_Range_April)

SAPM_Total_April_Final = left_join(SAPM_Total_April, SAPM_FL_Range_April, by = "SampleID")
colnames(SAPM_Total_April_Final) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
head(SAPM_Total_April_Final)

# May
SAPM_Plus_Count_May = aggregate(SumOfSumOfCatchCount ~ SampleID, data = SAPM_Catch_Effort_May[SAPM_Catch_Effort_May$ForkLength == 0,], sum)
head(SAPM_Plus_Count_May)

SAPM_FL_Count_May = aggregate(SumOfSumOfCatchCount ~ SampleID, data = SAPM_Catch_Effort_May[SAPM_Catch_Effort_May$ForkLength > 0,], sum)
head(SAPM_FL_Count_May)

SAPM_Total_May = full_join(SAPM_FL_Count_May, SAPM_Plus_Count_May,  by = "SampleID")
colnames(SAPM_Total_May) = c("SampleID", "FL_Count", "Plus_Count")
SAPM_Total_May[is.na(SAPM_Total_May)] = 0
head(SAPM_Total_May)

SAPM_Total_May$Total_Count = SAPM_Total_May$FL_Count + SAPM_Total_May$Plus_Count
head(SAPM_Total_May)

SAPM_FL_Range_May = aggregate(SumOfSumOfCatchCount ~ SampleID , data = SAPM_Catch_Effort_May[SAPM_Catch_Effort_May$ForkLength > 24 & SAPM_Catch_Effort_May$ForkLength < 85,], sum)
head(SAPM_FL_Range_May)

SAPM_Total_May_Final = left_join(SAPM_Total_May, SAPM_FL_Range_May, by = "SampleID")
colnames(SAPM_Total_May_Final) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
head(SAPM_Total_May_Final)


# June
SAPM_Plus_Count_June = aggregate(SumOfSumOfCatchCount ~ SampleID, data = SAPM_Catch_Effort_June[SAPM_Catch_Effort_June$ForkLength == 0,], sum)
sum(SAPM_Plus_Count_June$SumOfSumOfCatchCount)

SAPM_FL_Count_June = aggregate(SumOfSumOfCatchCount ~ SampleID, data = SAPM_Catch_Effort_June[SAPM_Catch_Effort_June$ForkLength > 0,], sum)
sum(SAPM_FL_Count_June$SumOfSumOfCatchCount)

SAPM_Total_June = full_join(SAPM_FL_Count_June, SAPM_Plus_Count_June,  by = "SampleID")
colnames(SAPM_Total_June) = c("SampleID", "FL_Count", "Plus_Count")
SAPM_Total_June[is.na(SAPM_Total_June)] = 0
head(SAPM_Total_June)

SAPM_Total_June$Total_Count = SAPM_Total_June$FL_Count + SAPM_Total_June$Plus_Count
head(SAPM_Total_June)
sum(SAPM_Total_June$Total_Count)


SAPM_FL_Range_June = aggregate(SumOfSumOfCatchCount ~ SampleID , data = SAPM_Catch_Effort_June[SAPM_Catch_Effort_June$ForkLength > 24 & SAPM_Catch_Effort_June$ForkLength < 105,], sum)
head(SAPM_FL_Range_June)
sum(SAPM_FL_Range_June$SumOfSumOfCatchCount)

SAPM_Total_June_Final = left_join(SAPM_Total_June, SAPM_FL_Range_June, by = "SampleID")
colnames(SAPM_Total_June_Final) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
head(SAPM_Total_June_Final)

# July
SAPM_Plus_Count_July = aggregate(SumOfSumOfCatchCount ~ SampleID, data = SAPM_Catch_Effort_July[SAPM_Catch_Effort_July$ForkLength == 0,], sum)
head(SAPM_Plus_Count_July)

SAPM_FL_Count_July = aggregate(SumOfSumOfCatchCount ~ SampleID, data = SAPM_Catch_Effort_July[SAPM_Catch_Effort_July$ForkLength > 0,], sum)
head(SAPM_FL_Count_July)

SAPM_Total_July = full_join(SAPM_FL_Count_July, SAPM_Plus_Count_July,  by = "SampleID")
colnames(SAPM_Total_July) = c("SampleID", "FL_Count", "Plus_Count")
SAPM_Total_July[is.na(SAPM_Total_July)] = 0
head(SAPM_Total_July)

SAPM_Total_July$Total_Count = SAPM_Total_July$FL_Count + SAPM_Total_July$Plus_Count
head(SAPM_Total_July)

SAPM_FL_Range_July = aggregate(SumOfSumOfCatchCount ~ SampleID , data = SAPM_Catch_Effort_July[SAPM_Catch_Effort_July$ForkLength > 24 & SAPM_Catch_Effort_July$ForkLength < 125,], sum)
head(SAPM_FL_Range_July)

SAPM_Total_July_Final = left_join(SAPM_Total_July, SAPM_FL_Range_July, by = "SampleID")
colnames(SAPM_Total_July_Final) = c("SampleID", "FL_Count", "Plus_Count", "Total_Count", "FL_Range_Count")
head(SAPM_Total_July_Final)


# Combining all months
SAPM_Adj_Final = rbind(SAPM_Total_June_Final, SAPM_Total_July_Final)
head(SAPM_Adj_Final)

SAPM_Adj_Final$Adjusted_Count = SAPM_Adj_Final$Total_Count * (SAPM_Adj_Final$FL_Range_Count/SAPM_Adj_Final$FL_Count)
head(SAPM_Adj_Final)
SAPM_Adj_Final[is.na(SAPM_Adj_Final)] = 0

SAPM_Index = full_join(subset(SAPM_Catch_Effort, !duplicated(SampleID)), SAPM_Adj_Final, by = "SampleID")
head(SAPM_Index)
SAPM_Index[,15:21][is.na(SAPM_Index[,15:21])] = 0
head(SAPM_Index)

SAPM_Index$CPUE = SAPM_Index$Adjusted_Count/SAPM_Index$SeineVolume
head(SAPM_Index)

# Indicies

# Calculating mean May and June CPUE for each station 
Mean.CPUE.Station.Month.SAPM = aggregate(CPUE ~ StationCode + Year + Month + subarea, data = SAPM_Index, mean, na.action = na.omit)
head(Mean.CPUE.Station.Month.SAPM, n = 25)

# Averaging average of March through July CPUE for each subarea
Mean.CPUE.Station.SAPM = aggregate(CPUE ~ Year + Month + subarea , data = Mean.CPUE.Station.Month.SAPM, mean, na.action = na.omit)
head(Mean.CPUE.Station.SAPM, n = 25)

# Averaging, average of average of May and June CPUE for each station within a subarea
Mean.Catch.Subarea.SAPM = aggregate(CPUE ~ Year +  subarea , data = Mean.CPUE.Station.SAPM, mean, na.action = na.omit)
head(Mean.Catch.Subarea.SAPM, n = 50)

SAPM_Index_Final_Temp = reshape(Mean.Catch.Subarea.SAPM, idvar = "Year", timevar = "subarea", direction = "wide")

# Summing subareas 1-5 for Delta Index, 6-8 for sac and 9-10 for SJ River
SAPM_Index_Final$Delta_Index = rowSums(SAPM_Index_Final[,2:6])
SAPM_Index_Final$Sac_River_Index = rowSums(SAPM_Index_Final[,7:9], na.rm = T) # na.rm = T because we are no longer sampling in subarea 8
SAPM_Index_Final$San_Joaquin_River_Index = rowSums(SAPM_Index_Final[,10:11])


SAPM_Index_Final
write.csv(SAPM_Index_Final, "SAPM_Index_Final.csv")
str(SAPM_Index_Final)
plot(SAPM_Index_Final$San_Joaquin_River_Index ~ SAPM_Index_Final$Year, type = "l", lwd = 2)
plot(SAPM_Index_Final$Sac_River_Index ~ SAPM_Index_Final$Year, type = "l", lwd = 2)

SAPM_SJ_GGPLOT = ggplot(data= SAPM_Index_Final, aes(x=Year, y=San_Joaquin_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,.025), expand = c(0, 0)) +
  theme_classic()
SAPM_SJ_GGPLOT

SAPM_Sac_GGPLOT = ggplot(data= SAPM_Index_Final, aes(x=Year, y=Sac_River_Index)) +
  geom_bar(stat = "identity", fill = "black") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1.25), expand = c(0, 0)) +
  theme_classic()
SAPM_Sac_GGPLOT

# Output for SAPM plots
png(file = "SAPM.Indicies.png", width = 1500, height = 1850, bg = "transparent", res = 160)
grid.draw(grid.arrange(arrangeGrob(SAPM_Sac_GGPLOT, SAPM_SJ_GGPLOT,  ncol = 1,
                                   top =textGrob("SAPM", vjust=2,gp = gpar(fontsize=25,   fontfamily="Times")))))         
                                   
dev.off()
