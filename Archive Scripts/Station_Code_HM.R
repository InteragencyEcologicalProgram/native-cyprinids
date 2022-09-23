# Station Code Heatmaps

Sample_Data <- 
  Sample_Table_Final %>% 
  filter(StationCode %in% Station_Codes_Final$StationCode,
         Month %in% c(5:7)) %>% 
  mutate(DV = 1)
head(Sample_Data)

Sample_Data_HM <- 
  Sample_Data %>% 
  filter(GearConditionCode   != 4,
         !(StationCode %in% c("SJ079E", "SJ076W", "SJ074A", "SJ058E"))) %>% 
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

Stations_Abridged <- 
  unique(Sample_Data_HM$StationCode)

write.csv(Stations_Abridged, "Output/Stations_Abridged.csv", row.names = FALSE)
