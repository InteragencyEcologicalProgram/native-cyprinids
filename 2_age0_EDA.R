


EDA_data <- data %>%
  dplyr::select(Program, SampleDate, Year, Month, IEPFishCode, CountAdj, ForkLength) %>%
  dplyr::group_by(Program, IEPFishCode, Month, ForkLength) %>%
  dplyr::summarize(Count=sum(CountAdj), 
                   .groups="drop") %>%
  dplyr::filter(ForkLength != 0) %>%
  dplyr::filter(Program == "DJFMP")  # go with DJ for now



p_SACPIK <- subset(EDA_data, IEPFishCode == "SACPIK") %>%
  ggplot() +
  geom_histogram(aes(x=ForkLength, weight=Count, fill=Program), binwidth=5) + 
  facet_wrap(~ Month) + 
  geom_vline(xintercept=c(91), col="red") + 
  ggtitle("Pikeminnow; current focal months: 6 & 7")  
p_SACPIK


p_SPLIT <- subset(EDA_data, IEPFishCode == "SPLITT") %>%
  ggplot() +
  geom_histogram(aes(x=ForkLength, weight=Count, fill=Program), binwidth=5) + 
  facet_wrap(~ Month) + 
  geom_vline(xintercept=c(118), col="red") + 
  ggtitle("Splittail; current focal months: 5 & 6")
p_SPLIT


p_SACSUC <- subset(EDA_data, IEPFishCode == "SACSUC") %>%
  ggplot() +
  geom_histogram(aes(x=ForkLength, weight=Count, fill=Program), binwidth=5) + 
  facet_wrap(~ Month) + 
  geom_vline(xintercept=c(86), col="red") + 
  ggtitle("Sucker; current focal months: 5 & 6")
p_SACSUC


p_COMCAR <- subset(EDA_data, IEPFishCode == "COMCAR") %>%
  ggplot() +
  geom_histogram(aes(x=ForkLength, weight=Count, fill=Program), binwidth=5) + 
  facet_wrap(~ Month) + 
  geom_vline(xintercept=c(164), col="red") + 
  ggtitle("Carp; current focal months: 5 & 6")
p_COMCAR


p_REDSHI <- subset(EDA_data, IEPFishCode == "REDSHI") %>%
  ggplot() +
  geom_histogram(aes(x=ForkLength, weight=Count, fill=Program), binwidth=5) + 
  facet_wrap(~ Month) + 
  geom_vline(xintercept=c(49), col="red") + 
  ggtitle("Red shiner; current focal months: 3 & 4")
p_REDSHI


p_GOLDSHI <- subset(EDA_data, IEPFishCode == "GOLDSHI") %>%
  ggplot() +
  geom_histogram(aes(x=ForkLength, weight=Count, fill=Program), binwidth=5) + 
  facet_wrap(~ Month) + 
  geom_vline(xintercept=c(65), col="red") + 
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




