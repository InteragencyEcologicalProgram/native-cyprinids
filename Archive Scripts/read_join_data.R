###########################################################
### read_join_data.R ######################################
### Created 8/30/2022 
### Script for reading DJFMP and YBFMP fish data from EDI 
###########################################################

library(contentid)
library(readr)
library(dplyr)
library(tidyr)

# Read data ------------------------------
# https://portal.edirepository.org/nis/mapbrowse?packageid=edi.244.9
# https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=233&revision=3

# store cached version
(djfmp_catch_url <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.9&entityid=147fd5e2c7db15913b2ffa44410dc7f9"))
(djfmp_site_url <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.9&entityid=99a038d691f27cd306ff93fdcbc03b77"))
(ybfmp_length_url <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.3&entityid=402732c0e6c782db8b8229c3b9310afa"))
(ybfmp_wq_url <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.3&entityid=4488201fee45953b001f70acf30f7734"))
(ybfmp_sites_url <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.3&entityid=89146f1382d7dfa3bbf3e4b1554eb5cc"))
(ybfmp_catch_url <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.3&entityid=b2b92d9dbfb78cfb1a5716174dfceab1"))

# cached files (shouldn't have to run the above once cached)
djfmp_catch_file <- contentid::resolve("hash://sha256/b4971a5ca21dc96c8635774d17880ecd552d26cf6eacb77231f659df618a68d1")
djfmp_site_file <- contentid::resolve("hash://sha256/f0f9e66da7415df90a7c0ea4c01938ecadd71ad412af1ccc940e861e63e96ba7")
ybfmp_length_file <- contentid::resolve("hash://sha256/71c3a4667bd3a00859902018ea257c9914b63f1111d276d2cdd960025247c980")
ybfmp_wq_file <- contentid::resolve("hash://sha256/17fc26c768fa24c65016cfe3d4e8d579b909180a7e349386efa7e24e1772def2")
ybfmp_sites_file <- contentid::resolve("hash://sha256/acc9940abf5662e81ee594553e7dc46a05c4cace9c924dbf5352c0544bc7a481")
ybfmp_catch_file <- contentid::resolve("hash://sha256/110b7e3a5cac91b6a2619e2421c7c19f22fb4cc84335b1cc2e5d749080dc9d92")

# read files
djfmp_catch0 <- read_csv(djfmp_catch_file)
djfmp_sites <- read_csv(djfmp_site_file)
ybfmp_length <- read_csv(ybfmp_length_file)
ybfmp_wq0 <- read_csv(ybfmp_wq_file)
ybfmp_sites <- read_csv(ybfmp_sites_file)
ybfmp_catch0 <- read_csv(ybfmp_catch_file)

# Join data ------------------------------------
# Are we adjusting for length frequency?


# Filter data

# From USFWS list
stations_notincl <- c("SP000E","SP000W","SP001W","SP003E",
                      "SP008E","SA001M","SA004W","SA007E","SA008W","SA009E", "SA010W")
djfmp_catch <- djfmp_catch0 %>% 
  filter(!(StationCode %in% stations_notincl))

# beach seine only 
ybfmp_wq <- ybfmp_wq0 %>%
  filter(MethodCode == "BSEIN")
