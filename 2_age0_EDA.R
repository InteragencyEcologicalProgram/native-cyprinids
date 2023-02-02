

EDA_data_SACPIK <- data %>%
  filter(IEPFishCode == "SACPIK") %>%
  dplyr::select(SampleDate, Year, Month, IEPFishCode, CountAdj, ForkLength) %>%
  dplyr::group_by(IEPFishCode, Month, ForkLength) %>%
  dplyr::summarize(Count=sum(CountAdj), .groups="drop")

p_SACPIK <- ggplot(EDA_data_SACPIK) +
  geom_histogram(aes(x=ForkLength, weight=Count), binwidth=5) + 
  geom_vline(xintercept=c(91), col="red") + 
  facet_wrap(~ Month) + 
  ggtitle("Pikeminnow; current focal months: 6 & 7")
p_SACPIK


##############################################

EDA_data_SPLITT <- data %>%
  filter(IEPFishCode == "SPLITT") %>%
  dplyr::select(SampleDate, Year, Month, IEPFishCode, CountAdj, ForkLength) %>%
  dplyr::group_by(IEPFishCode, Month, ForkLength) %>%
  dplyr::summarize(Count=sum(CountAdj), .groups="drop")

p_SPLIT <- ggplot(EDA_data_SPLITT) +
  geom_histogram(aes(x=ForkLength, weight=Count), binwidth=5) + 
  geom_vline(xintercept=c(118), col="red") + 
  facet_wrap(~ Month) + 
  ggtitle("Splittail; current focal months: 5 & 6")
p_SPLIT


##############################################

EDA_data_SACSUC <- data %>%
  filter(IEPFishCode == "SACSUC") %>%
  dplyr::select(SampleDate, Year, Month, IEPFishCode, CountAdj, ForkLength) %>%
  dplyr::group_by(IEPFishCode, Month, ForkLength) %>%
  dplyr::summarize(Count=sum(CountAdj), .groups="drop")

p_SACSUC <- ggplot(EDA_data_SACSUC) +
  geom_histogram(aes(x=ForkLength, weight=Count), binwidth=5) + 
  geom_vline(xintercept=c(86), col="red") + 
  facet_wrap(~ Month) + 
  ggtitle("Sucker; current focal months: 5 & 6")
p_SACSUC


##############################################

EDA_data_COMCAR <- data %>%
  filter(IEPFishCode == "COMCAR") %>%
  dplyr::select(SampleDate, Year, Month, IEPFishCode, CountAdj, ForkLength) %>%
  dplyr::group_by(IEPFishCode, Month, ForkLength) %>%
  dplyr::summarize(Count=sum(CountAdj), .groups="drop")

p_COMCAR <- ggplot(EDA_data_COMCAR) +
  geom_histogram(aes(x=ForkLength, weight=Count), binwidth=5) + 
  geom_vline(xintercept=c(164), col="red") + 
  facet_wrap(~ Month) + 
  ggtitle("Carp; current focal months: 5 & 6")
p_COMCAR

##############################################

EDA_data_REDSHI <- data %>%
  filter(IEPFishCode == "REDSHI") %>%
  dplyr::select(SampleDate, Year, Month, IEPFishCode, CountAdj, ForkLength) %>%
  dplyr::group_by(IEPFishCode, Month, ForkLength) %>%
  dplyr::summarize(Count=sum(CountAdj), .groups="drop")

p_REDSHI <- ggplot(EDA_data_REDSHI) +
  geom_histogram(aes(x=ForkLength, weight=Count), binwidth=5) + 
  geom_vline(xintercept=c(49), col="red") + 
  facet_wrap(~ Month) + 
  ggtitle("Red shiner; current focal months: 3 & 4")
p_REDSHI

##############################################

EDA_data_GOLDSHI <- data %>%
  filter(IEPFishCode == "GOLDSHI") %>%
  dplyr::select(SampleDate, Year, Month, IEPFishCode, CountAdj, ForkLength) %>%
  dplyr::group_by(IEPFishCode, Month, ForkLength) %>%
  dplyr::summarize(Count=sum(CountAdj), .groups="drop")

p_GOLDSHI <- ggplot(EDA_data_GOLDSHI) +
  geom_histogram(aes(x=ForkLength, weight=Count), binwidth=5) + 
  geom_vline(xintercept=c(65), col="red") + 
  facet_wrap(~ Month) + 
  ggtitle("Golden shiner; current focal months: 6 & 7")
p_GOLDSHI


##############################################


pdf("Figures/length_figures.pdf", onefile=TRUE, width=9)
  p_SACPIK
  p_SPLIT
  p_SACSUC
  p_COMCAR
  p_REDSHI
  p_GOLDSHI
dev.off()



pdf("Figures/length_figures_0_to_80mm.pdf", onefile=TRUE, width=9)
  p_SACPIK + xlim(0, 80)
  p_SPLIT + xlim(0, 80)
  p_SACSUC + xlim(0, 80)
  p_COMCAR + xlim(0, 80)
  p_REDSHI + xlim(0, 80)
  p_GOLDSHI + xlim(0, 80)
dev.off()



