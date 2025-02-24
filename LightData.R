
# Packages ----------------------------------------------------------------

install.packages("remotes")
install.packages("EDIutils")
install.packages("SciViews")

library(tidyverse)
library(visreg)
library(ggplot2)
library(readxl)
library(lubridate)
library(reshape2)
library(rstatix)
library(scales)
library(vegan)
library(FSA)
library(ggrepel)
library(RColorBrewer)
library(remotes)
library(EDIutils)
library(emmeans)
library(ggpubr)
library(readr)
library(viridis)
library(readxl)
library(readr)
library(SciViews)

# Data Wrangling and Orginization -----------------------------------------

YBFMP_WQ_Data_Copy <- read_excel("X:/Manuscripts, Reports, Newsletters (AEU)/YoloBypass_AnnualReport_LowerTrophic/Lower Trophic Report_2020-2022/Physical WQ/YBFMP_WQ_Data_Copy.xlsx", 
                                 col_types = c("text", "text", "date", 
                                               "text", "text", "text", "text", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "text", "text", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric"))

Light_IRR <- read_excel("Light Attenuation/Light IRR.xlsx")

Light_IRR <- Light_IRR %>% 
  select(-TblLight.PhysicalDataID,-TblPhysicalData.PhysicalDataID) %>% 
  rename(StationName = 'Station Code', SurfaceIrr = SurfaceIRR, Depth1Irr = Depth1,
         Depth2Irr = Depth2, Depth3Irr = Depth3, Depth4Irr = Depth4,
         CollectionDate = Date, SubIrr1 = SubSurfaceIRR1, SubIrr2 = SubSurfaceIRR2,
         SubIrr3 = SubSurfaceIRR3, SubIrr4 = SubSurfaceIRR4) %>% 
  mutate(CollectionDate = as.Date(CollectionDate, format = "%m/%d/%Y"))

Light_IRR2 <- YBFMP_WQ_Data_Copy %>% 
  select(StationName, CollectionDate, SurfaceIrr:SubIrr4) %>% 
  filter(StationName %in% c("SHR", "STTD")) %>% 
  na.omit()

LightIRRData <- bind_rows(Light_IRR,Light_IRR2) %>% 
  select(-SurfaceIrr)

LightIRRData <- LightIRRData %>% 
  mutate(WaterYear = case_when(
    between(LightIRRData$CollectionDate, as.Date("2011-10-01"), as.Date("2012-09-30")) ~ "2012",
    between(LightIRRData$CollectionDate, as.Date("2012-10-01"), as.Date("2013-09-30")) ~ "2013",
    between(LightIRRData$CollectionDate, as.Date("2013-10-01"), as.Date("2014-09-30")) ~ "2014",
    between(LightIRRData$CollectionDate, as.Date("2014-10-01"), as.Date("2015-09-30")) ~ "2015",
    between(LightIRRData$CollectionDate, as.Date("2015-10-01"), as.Date("2016-09-30")) ~ "2016",
    between(LightIRRData$CollectionDate, as.Date("2016-10-01"), as.Date("2017-09-30")) ~ "2017",
    between(LightIRRData$CollectionDate, as.Date("2017-10-01"), as.Date("2018-09-30")) ~ "2018",
    between(LightIRRData$CollectionDate, as.Date("2018-10-01"), as.Date("2019-09-30")) ~ "2019",
    between(LightIRRData$CollectionDate, as.Date("2019-10-01"), as.Date("2020-09-30")) ~ "2020",
    between(LightIRRData$CollectionDate, as.Date("2020-10-01"), as.Date("2021-09-30")) ~ "2021",
    between(LightIRRData$CollectionDate, as.Date("2021-10-01"), as.Date("2022-09-30")) ~ "2022",
    between(LightIRRData$CollectionDate, as.Date("2022-10-01"), as.Date("2023-09-30")) ~ "2023")) %>% 
  group_by(CollectionDate) 

LightIRR99STTD <- LightIRRData %>% 
  filter(StationName == "STTD") %>% 
  select(CollectionDate, StationName, WaterYear, Depth4Irr, SubIrr4) %>% 
  na.omit() %>% 
  filter(CollectionDate != as.Date("2017-04-13"))

LightIRR99SHR <- LightIRRData %>%
  filter(StationName == "SHR") %>% 
  select(CollectionDate, StationName, WaterYear, Depth4Irr, SubIrr4) %>% 
  na.omit() %>% 
  filter(CollectionDate != as.Date("2017-04-13"))

LightIRRDiff <- LightIRRData %>% 
  select(StationName, CollectionDate, WaterYear, Depth1Irr,Depth2Irr, Depth3Irr, Depth4Irr) %>% 
  mutate(DepthDiff1 = Depth2Irr - Depth1Irr) %>% 
  mutate(DepthDiff2 = Depth3Irr - Depth2Irr) %>% 
  mutate(DepthDiff3 = Depth4Irr - Depth3Irr)

mean(LightIRRDiff$DepthDiff1)
mean(LightIRRDiff$DepthDiff2)
mean(LightIRRDiff$DepthDiff3)

Depth_Long <- LightIRRData %>% 
  select(-contains("Sub")) %>% 
  pivot_longer(cols = c("Depth1Irr","Depth2Irr","Depth3Irr","Depth4Irr"),
               names_to = "DepthType",
               values_to = "Depth")

Light_long <- LightIRRData %>% 
  select(-contains("Depth")) %>% 
  pivot_longer(cols = c("SubIrr1","SubIrr2","SubIrr3","SubIrr4"),
               names_to = "LightType",
               values_to = "SubIrr")

LightIRRFinal <- bind_cols(Light_long,Depth_Long)

LightIRRFinal <- LightIRRFinal %>% 
  select(-c(StationName...1,CollectionDate...2,WaterYear...3)) %>% 
  rename(WaterYear = WaterYear...8, StationName = StationName...6, CollectionDate = CollectionDate...7) %>% 
  filter(between(CollectionDate, as.Date("2013-10-30"), as.Date("2022-12-23")))

LightIRRFinal<-LightIRRFinal %>% 
  filter(Depth!=2.19) %>%
  filter(Depth!=2.48) %>% 
  filter(Depth!=100) %>% 
  filter(Depth!=17)
  

#fixing typo 

LightIRRFinal[802,7]<-0.08



# Filtering Station Code --------------------------------------------------

LightSTTD <- LightIRRFinal %>% 
  filter(StationName == "STTD") %>% 
  filter(CollectionDate != as.Date("2017-04-13")) %>% 
  filter(CollectionDate != as.Date("2018-06-26")) %>% 
  na.omit()

LightSTTD <- LightSTTD[order(LightSTTD$SubIrr),]

LightSTTD2017 <- LightIRRFinal %>% 
  filter(WaterYear == "2017") %>%
  filter(StationName == "STTD") %>% 
  filter(Depth != 100.00) %>% 
  distinct()

LightSTTD2017 <- LightSTTD2017[order(LightSTTD2017$SubIrr), ]

LightSTTD2017$DepthType <- factor(LightSTTD2017$DepthType, levels = c("Depth1Irr", "Depth2Irr", "Depth3Irr", "Depth4Irr"))

LightSHR <- LightIRRData %>% 
  filter(StationName == "SHR")

(ggplot(data = LightSTTD2017, aes(x = CollectionDate, y = Depth, fill = SubIrr, color = DepthType)) +
    geom_bar(stat = 'identity', position = 'identity') +
    scale_y_reverse() +
    geom_hline(yintercept = 2.19)
)

(ggplot(data = LightIRR99STTD, aes(x = CollectionDate, y = Depth4Irr, fill = SubIrr4)) +
    geom_bar(stat = 'identity') +
    facet_wrap(~WaterYear, scales = 'free_x')+
    scale_y_reverse() +
    theme(axis.text.x = element_text(angle = 45)) +
    geom_hline(yintercept = 2.19)
)

(ggplot(data = LightSTTD, aes(x = CollectionDate, y = Depth, fill = SubIrr)) +
    geom_bar(stat = 'identity', position = 'identity') +
    facet_wrap(~WaterYear, scales = 'free_x') +
    scale_y_reverse() +
    geom_hline(yintercept = 2.19)
)


# Chla Wrangling ------------------------------------------------

#Chla_Query <- read_excel("Chla Query.xlsx")

#Chla1 <- Chla_Query %>% 
#  select(Concentration:Date) %>% 
#  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% 
#  rename(CollectionDate = Date, StationName = 'Station Code') %>% 
#  filter(between(CollectionDate, as.Date("2013-10-30"), as.Date("2019-11-14")))

remotes::install_github("ropensci/EDIutils", ref = "development")

# Package ID from EDI Data Portal https://dex.edirepository.org/dex/profile/1351
packageId <- "edi.1391.1"

res <- data.frame(read_data_entity_names(packageId))
entityid <- res$entityId[res$entityId == "8c457392256ff1e88ef4b757d8ffa3a6"]

# Calling on the EDI API to bring the data into R

raw_nut <- read_data_entity(packageId, entityid)

# Reading the raw data into a legible csv format. Unzipping the file, essentially.

nut_data <- readr::read_csv(file = raw_nut)

#Combining the Chla data frames

Chla <- nut_data %>% 
  select(station_code, sample_date, chlorophyll, secchi, turbidity) %>% 
  rename(StationName = station_code, CollectionDate = sample_date, Concentration = chlorophyll) %>% 
  filter(between(CollectionDate, as.Date("2013-10-30"), as.Date("2022-12-13"))) %>% 
  filter(StationName %in% c("SHR", "STTD"))

LightIRRFinal<- merge(Chla, LightIRRFinal, by=c("CollectionDate", "StationName"), no.dups=TRUE)

#LightIRRFinal2<- unite(LightIRRFinal1, secchi, c(secchi.x, secchi.y))

# Retrieving integrated_wq_totalcatch.csv via entityId
res <- data.frame(read_data_entity_names(packageId)) # Locating entityId
entityId <- res$entityId[res$entityId == "30251306380f615705a7fdd678faba05"] # entityId for Integrated Water Quality and Fish Catch

# Calling on the EDI API to bring the data into R

raw_phyto <- read_data_entity(packageId, entityId)

# Reading the raw data into a legible csv format. Unzipping the file, essentially.

phyto_data <- readr::read_csv(file = raw_phyto)

phyto_data_clean <- phyto_data %>% 
  select(sample_date, station_code, organisims_per_ml, taxon, genus, biovolume_1, sample_time) %>% 
  filter(station_code %in% c("STTD", "SHR")) %>% 
  rename(StationName = station_code, CollectionDate = sample_date, Biovolume = biovolume_1)

phyto_biovolume_total <- phyto_data_clean %>% 
  group_by(StationName, CollectionDate) %>% 
  mutate(sumBiovolume = sum(Biovolume)) %>% 
  mutate(sumOrganismsml = sum(organisims_per_ml)) %>% 
  select(CollectionDate, StationName, sumOrganismsml, sumBiovolume) %>% 
  distinct() %>% 
  ungroup() %>% 
  na.omit()




# Light, Chl a, and Phyto Data Frame --------------------------------------

# LCData <- merge(Light_IRR, Chla, by = c("CollectionDate", "StationName"))
# 
# LCData <- LCData %>% 
#   mutate(CollectionDate = as.Date(CollectionDate, format = "%m/%d/%Y")) %>% 
#   na.omit()
# 
# LPCData <- merge(phyto_biovolume_total, LCData, by = c("CollectionDate", "StationName"))
# 
# LPCData <- LPCData %>% 
#   mutate(WaterYear = case_when(
#     between(LPCData$CollectionDate, as.Date("2011-10-01"), as.Date("2012-09-30")) ~ "2012",
#     between(LPCData$CollectionDate, as.Date("2012-10-01"), as.Date("2013-09-30")) ~ "2013",
#     between(LPCData$CollectionDate, as.Date("2013-10-01"), as.Date("2014-09-30")) ~ "2014",
#     between(LPCData$CollectionDate, as.Date("2014-10-01"), as.Date("2015-09-30")) ~ "2015",
#     between(LPCData$CollectionDate, as.Date("2015-10-01"), as.Date("2016-09-30")) ~ "2016",
#     between(LPCData$CollectionDate, as.Date("2016-10-01"), as.Date("2017-09-30")) ~ "2017",
#     between(LPCData$CollectionDate, as.Date("2017-10-01"), as.Date("2018-09-30")) ~ "2018",
#     between(LPCData$CollectionDate, as.Date("2018-10-01"), as.Date("2019-09-30")) ~ "2019",
#     between(LPCData$CollectionDate, as.Date("2019-10-01"), as.Date("2020-09-30")) ~ "2020",
#     between(LPCData$CollectionDate, as.Date("2020-10-01"), as.Date("2021-09-30")) ~ "2021",
#     between(LPCData$CollectionDate, as.Date("2021-10-01"), as.Date("2022-09-30")) ~ "2022",
#     between(LPCData$CollectionDate, as.Date("2022-10-01"), as.Date("2023-09-30")) ~ "2023")) %>% 
#   mutate(WaterYear = as.factor(WaterYear))

#Any correspondence with Chl-a and 75% light depth
#Extinction Coefficient equation that goes along with light attenuation
#Gives estimate of photic zone to calculate extinction coefficient with turbidity
#Compare total suspended solids and light attenuation
#Figure if phytoplankton are light limited
#Extinction Coefficient - Gives idea of depth of photic zone and intesnisty of light that they need to be productive
#Find continuance stations with irradiance to determine cloud cover.
#Kd = 1/z*ln (E0/Ez) z = distance in meters being recorded; E0 is surface irradiance
#Try doing more connections when analysing the data with LA, Chl-a, phytoplankton, and secchi depth.
#Biovolume better to use
#

# Statistical Analysis ----------------------------------------------------

# SecchTurbReg <- lm(secchi ~ turbidity, data = LPCData)
# 
# summary(SecchTurbReg)
# 
# ggplot(data = LPCData) +
#   geom_line(aes(x = CollectionDate, y = secchi), color = "black") +
#   geom_point(aes(x = CollectionDate, y = secchi), color = "black") +
#   geom_line(aes(x = CollectionDate, y = turbidity / 100), color = "blue") +
#   geom_point(aes(x = CollectionDate, y = turbidity / 100), color = "blue") +
#   scale_y_continuous(name = "Secchi", sec.axis = sec_axis(~ . *50, name = "Turbidity")) 
# 
# ChlaLightReg <- lm(SubIrr3 ~ Concentration, data = LPCData)
# 
# summary(ChlaLightReg)
# 
# PhytoturbidityReg <- lm(turbidity ~ sumBiovolume, data = LPCData)
# 
# summary(PhytoturbidityReg)
# 
# CHLaPhytoLightMReg <- lm(SubIrr3 ~ sumBiovolume * Concentration, data = LPCData)
# 
# summary(CHLaPhytoLightMReg)
# 
# LPCData15STTD <- LPCData %>% 
#   filter(WaterYear == "2015") %>% 
#   filter(StationName == "STTD") %>% 
#   mutate(CollectionDate = floor_date(CollectionDate, unit = "day")) %>% 
#   filter(CollectionDate != "2014-12-30")
# 
# (Conc15 <- ggplot(data = LPCData15STTD) +
#     geom_line(aes(x = CollectionDate, y = Concentration / 70)) +
#     geom_point(aes(x = CollectionDate, y = Concentration / 70)) +
#     geom_bar(aes(x = CollectionDate, y = Depth3Irr, fill = SubIrr3),stat = "identity", alpha = .5) +
#     scale_y_continuous(name = "Depth", sec.axis = sec_axis(~ . *70, name = "Concentration"))
# )
# 
# (Sum15 <- ggplot(data = LPCData15STTD) +
#     geom_line(aes(x = CollectionDate, y = sumBiovolume / 20000)) +
#     geom_point(aes(x = CollectionDate, y = sumBiovolume / 20000)) +
#     geom_bar(aes(x = CollectionDate, y = Depth3Irr, fill = SubIrr3),stat = "identity", width =4, alpha = .5) +
#     scale_y_continuous(name = "Depth", sec.axis = sec_axis(~ . *20000, name = "Biovolume"))
# )
# 
# (Graph2015 <- ggarrange(Conc15, Sum15,
#                         ncol = 1,
#                         common.legend = TRUE)
# )
# 
# KWPhytoWY <- kruskal.test(sumOrganismsml ~ WaterYear, data = LPCData)
# 
# summary(KWPhytoWY)
# 
# KWChlaWY <- kruskal.test(Concentration ~ WaterYear, data = LPCData)
# 
# summary(KWChlaWY)

  # # Phytoplankton Wrangling -------------------------------------------------
# 
# remotes::install_github("ropensci/EDIutils", ref = "development", force = TRUE)
# 
# # Package ID from EDI Data Portal https://dex.edirepository.org/dex/profile/1351
# packageId <- "edi.1391.1"
# 
# # Retrieving integrated_wq_totalcatch.csv via entityId
# res <- data.frame(read_data_entity_names(packageId)) # Locating entityId
# entityId <- res$entityId[res$entityId == "30251306380f615705a7fdd678faba05"] # entityId for Integrated Water Quality and Fish Catch
# 
# # Calling on the EDI API to bring the data into R
# 
# raw_phyto <- read_data_entity(packageId, entityId)
# 
# # Reading the raw data into a legible csv format. Unzipping the file, essentially.
# 
# phyto_data <- readr::read_csv(file = raw_phyto)
# 
# phyto_data_clean <- phyto_data %>% 
#   select(sample_date, station_code, organisims_per_ml, taxon, genus, biovolume_1, sample_time) %>% 
#   filter(station_code %in% c("STTD", "SHR")) %>% 
#   rename(StationName = station_code, CollectionDate = sample_date, Biovolume = biovolume_1)
# 
# phyto_biovolume_total <- phyto_data_clean %>% 
#   group_by(StationName, CollectionDate) %>% 
#   mutate(sumBiovolume = sum(Biovolume)) %>% 
#   mutate(sumOrganismsml = sum(organisims_per_ml)) %>% 
#   select(CollectionDate, StationName, sumOrganismsml, sumBiovolume) %>% 
#   distinct() %>% 
#   ungroup() %>% 
#   na.omit()
# 
# # Light, Chl a, and Phyto Data Frame --------------------------------------
# 
# LCData <- merge(Light_IRR, Chla, by = c("CollectionDate", "StationName"))
# 
# LCData <- LCData %>% 
#   mutate(CollectionDate = as.Date(CollectionDate, format = "%m/%d/%Y")) %>% 
#   na.omit()
# 
# LPCData <- merge(phyto_biovolume_total, LCData, by = c("CollectionDate", "StationName"))
# 
# LPCData <- LPCData %>% 
#   mutate(WaterYear = case_when(
#     between(LPCData$CollectionDate, as.Date("2011-10-01"), as.Date("2012-09-30")) ~ "2012",
#     between(LPCData$CollectionDate, as.Date("2012-10-01"), as.Date("2013-09-30")) ~ "2013",
#     between(LPCData$CollectionDate, as.Date("2013-10-01"), as.Date("2014-09-30")) ~ "2014",
#     between(LPCData$CollectionDate, as.Date("2014-10-01"), as.Date("2015-09-30")) ~ "2015",
#     between(LPCData$CollectionDate, as.Date("2015-10-01"), as.Date("2016-09-30")) ~ "2016",
#     between(LPCData$CollectionDate, as.Date("2016-10-01"), as.Date("2017-09-30")) ~ "2017",
#     between(LPCData$CollectionDate, as.Date("2017-10-01"), as.Date("2018-09-30")) ~ "2018",
#     between(LPCData$CollectionDate, as.Date("2018-10-01"), as.Date("2019-09-30")) ~ "2019",
#     between(LPCData$CollectionDate, as.Date("2019-10-01"), as.Date("2020-09-30")) ~ "2020",
#     between(LPCData$CollectionDate, as.Date("2020-10-01"), as.Date("2021-09-30")) ~ "2021",
#     between(LPCData$CollectionDate, as.Date("2021-10-01"), as.Date("2022-09-30")) ~ "2022",
#     between(LPCData$CollectionDate, as.Date("2022-10-01"), as.Date("2023-09-30")) ~ "2023")) %>% 
#   mutate(WaterYear = as.factor(WaterYear))
# 
# #Any correspondence with Chl-a and 75% light depth
# #Extinction Coefficient equation that goes along with light attenuation
# #Gives estimate of photic zone to calculate extinction coefficient with turbidity
# #Compare total suspended solids and light attenuation
# #Figure if phytoplankton are light limited
# #Extinction Coefficient - Gives idea of depth of photic zone and intesnisty of light that they need to be productive
# #Find continuance stations with irradiance to determine cloud cover.
# #Kd = 1/z*ln (E0/Ez) z = distance in meters being recorded; E0 is surface irradiance
# #Try doing more connections when analysing the data with LA, Chl-a, phytoplankton, and secchi depth.
# #Biovolume better to use
# #
# 
# # Statistical Analysis ----------------------------------------------------
# 
# SecchTurbReg <- lm(secchi ~ turbidity, data = LPCData)
# 
# summary(SecchTurbReg)
# 
# ggplot(data = LPCData) +
#   geom_line(aes(x = CollectionDate, y = secchi), color = "black") +
#   geom_point(aes(x = CollectionDate, y = secchi), color = "black") +
#   geom_line(aes(x = CollectionDate, y = turbidity / 100), color = "blue") +
#   geom_point(aes(x = CollectionDate, y = turbidity / 100), color = "blue") +
#   scale_y_continuous(name = "Secchi", sec.axis = sec_axis(~ . *50, name = "Turbidity")) 
#   
# ChlaLightReg <- lm(SubIrr3 ~ Concentration, data = LPCData)
# 
# summary(ChlaLightReg)
# 
# PhytoturbidityReg <- lm(turbidity ~ sumBiovolume, data = LPCData)
# 
# summary(PhytoturbidityReg)
# 
# CHLaPhytoLightMReg <- lm(SubIrr3 ~ sumBiovolume * Concentration, data = LPCData)
# 
# summary(CHLaPhytoLightMReg)
# 
# LPCData15STTD <- LPCData %>% 
#   filter(WaterYear == "2015") %>% 
#   filter(StationName == "STTD") %>% 
#   mutate(CollectionDate = floor_date(CollectionDate, unit = "day")) %>% 
#   filter(CollectionDate != "2014-12-30")
# 
# (Conc15 <- ggplot(data = LPCData15STTD) +
#   geom_line(aes(x = CollectionDate, y = Concentration / 70)) +
#   geom_point(aes(x = CollectionDate, y = Concentration / 70)) +
#   geom_bar(aes(x = CollectionDate, y = Depth3Irr, fill = SubIrr3),stat = "identity", alpha = .5) +
#   scale_y_continuous(name = "Depth", sec.axis = sec_axis(~ . *70, name = "Concentration"))
# )
#   
# (Sum15 <- ggplot(data = LPCData15STTD) +
#   geom_line(aes(x = CollectionDate, y = sumBiovolume / 20000)) +
#   geom_point(aes(x = CollectionDate, y = sumBiovolume / 20000)) +
#   geom_bar(aes(x = CollectionDate, y = Depth3Irr, fill = SubIrr3),stat = "identity", width =4, alpha = .5) +
#   scale_y_continuous(name = "Depth", sec.axis = sec_axis(~ . *20000, name = "Biovolume"))
# )
# 
# (Graph2015 <- ggarrange(Conc15, Sum15,
#           ncol = 1,
#           common.legend = TRUE)
# )
# 
# KWPhytoWY <- kruskal.test(sumOrganismsml ~ WaterYear, data = LPCData)
# 
# summary(KWPhytoWY)
# 
# KWChlaWY <- kruskal.test(Concentration ~ WaterYear, data = LPCData)
# 
# summary(KWChlaWY)



# Data Findings -----------------------------------------------------------

#No strong linear relationship between Depth:Organismsml or chlorophyll and SubIrr3:Organismsml or chlorophyll.
#Possibly not light limited?

#calculating kdPAR ----------------------------------------------------------------

LightIRRFinal$lnPAR<-log(LightIRRFinal$SubIrr)



LightIRR_kdPAR <- LightIRRFinal %>%
  group_by(CollectionDate) %>%
  do(model = lm(lnPAR ~ Depth, data = .)) %>%  
  mutate(kdPAR = coef(model)[2]) %>%  
  select(CollectionDate, kdPAR) 



#calculate photic zone (Zeu) with Light IRR-----------------------------------------------------------

zeu_lightIRR_func <- function(kdPAR) {
  abs_kdPAR <- abs(kdPAR)
  zeu_lightIRR <- -4.605 / abs_kdPAR
  return(zeu_lightIRR)
  
}


LightIRR_kdPAR$zeu_lightIRR <- sapply(LightIRR_kdPAR$kdPAR, zeu_lightIRR_func)

LightIRRFinal <- LightIRRFinal %>%
  left_join(LightIRR_kdPAR, by = "CollectionDate") 


#calculate zeu with Secchi-----------------------------------------------------------------------

LightIRRFinal<-LightIRRFinal %>%
  group_by(CollectionDate) %>% 
  mutate(zeu_secchi=secchi*3) %>% 
  ungroup()


#secchi_zeu <- secchi_df %>%
#group_by(CollectionDate) %>% 
#mutate(zeu_secchi=secchi*3) %>% 
#select(CollectionDate, zeu_secchi)



#calculating zeu with turbidity------------------------------------------------------------------------------

#turb_df<-YBFMP_WQ_Data_Copy %>% 
  #select(StationName, CollectionDate,turb, WaterYear) %>% 
  #na.omit(turb) %>% 
  #filter(StationName %in% c("SHR", "STTD"))

#select(turb, zeu_turb, CollectionDate)

kdpar_turb_func <- function(turbidity) {
  kdpar_turb <-0.52+(turbidity*0.11)
  return(kdpar_turb)
  
}

LightIRRFinal$kdpar_turb <- sapply(LightIRRFinal$turbidity, kdpar_turb_func)

LightIRRFinal<-LightIRRFinal %>% 
  mutate(zeu_turb=ln(0.01)/-kdpar_turb)
  

#testing light IRR zeu accurancy against STTD 4th Depth-----------------------------------

LightSTTD_test<- LightIRRFinal %>% 
  filter(StationName=="STTD") %>% 
  filter(DepthType=='Depth4Irr') %>% 
  mutate(kdPAR=abs(kdPAR))

LightSHR_test<- LightIRRFinal %>% 
  filter(StationName=="SHR") %>% 
  filter(DepthType=='Depth4Irr') %>% 
  mutate(kdPAR=abs(kdPAR))

ggplot(LightSTTD_test, aes(x = Depth, y = zeu_lightIRR)) +
  geom_line()+ geom_point() + geom_smooth()+ 
  labs(title="Light attenuation-Predicted Photic Zone Depth vs Observed at STTD",
       x= "Observed Depth",
       y= "Predicted Depth ")

ggsave("C:/Users/acamille/OneDrive - California Department of Water Resources/Documents/LightAttenuationPoster/LightAttenuationSTTD.png",
       width = 6, height =  4, units = "in")



ggplot(LightSHR_test, aes(x = Depth, y = zeu_lightIRR)) +
  geom_line()+ geom_point() + geom_smooth()+ 
  labs(title="Light attenuation-Predicted Photic Zone Depth vs Observed at SHR",
       x= "Observed Depth",
       y= "Predicted Depth")

getwd()

ggsave("C:/Users/acamille/OneDrive - California Department of Water Resources/Documents/LightAttenuationPoster/LightAttenuationSHR.png",
       width = 6, height =  4, units = "in")


#testing zecchi kdPAR ----------------------------------------------------------------------------------

ggplot(LightSTTD_test, aes(x= kdPAR, y= kdpar_secchi))+geom_line()+geom_point()
       +geom_smooth()


# compare secchi to actual---------------------------------------------------------------

ggplot(STTD_secchi, aes(x = Depth, y = zeu_secchi)) +
  geom_line()+ geom_point() + geom_smooth()+ 
  labs(title="Predicted Photic Zone Depth vs Observed at STTD- Secchi",
       x= "Observed Depth",
       y= "Predicted Depth")

zeu_diff <- LightIRRFinal %>%
  mutate(difference = abs(zeu_lightIRR- zeu_secchi)) %>% 
  na.omit(zeu_diff)


summary_stats <- zeu_diff %>%
  summarise(
    mean_difference = mean(difference),
    sd_difference = sd(difference),
    max_difference = max(difference),
    min_difference = min(difference))

print(summary_stats)

ggplot(LightSTTD_test, aes(x = Depth, y = zeu_secchi)) +
  geom_line()+ geom_point() + geom_smooth()+ 
  labs(title="Predicted Photic Zone Depth vs Observed at STTD- Secchi",
       x= "Observed Depth",
       y= "Predicted Depth")

ggplot(LightSHR_test, aes(x = Depth, y = zeu_secchi)) +
  geom_line()+ geom_point() + geom_smooth()+ 
  labs(title="Predicted Photic Zone Depth vs Observed at SHR- Secchi",
       x= "Observed Depth",
       y= "Predicted Depth")

#testing turbidity----------------------------------------------------------

# ggplot(LightSTTD_test, aes(x = Depth, y = zeu_turb)) +
#   geom_line()+ geom_point() + geom_smooth()+ 
#   labs(title="Predicted Photic Zone Depth vs Observed at STTD- Secchi",
#        x= "Observed Depth",
#        y= "Predicted Depth")
# 
# zeu_diff <- LightIRRFinal %>%
#   mutate(difference = abs(zeu_lightIRR- zeu_secchi)) %>% 
#   na.omit(zeu_diff)
# 
# 
# summary_stats <- zeu_diff %>%
#   summarise(
#     mean_difference = mean(difference),
#     sd_difference = sd(difference),
#     max_difference = max(difference),
#     min_difference = min(difference))
# 
# print(summary_stats)
# 
# ggplot(LightSTTD_test, aes(x = Depth, y = zeu_secchi)) +
#   geom_line()+ geom_point() + geom_smooth()+ 
#   labs(title="Predicted Photic Zone Depth vs Observed at STTD- Secchi",
#        x= "Observed Depth",
#        y= "Predicted Depth")
# 
# ggplot(LightSHR_test, aes(x = Depth, y = zeu_secchi)) +
#   geom_line()+ geom_point() + geom_smooth()+ 
#   labs(title="Predicted Photic Zone Depth vs Observed at SHR- Secchi",
#        x= "Observed Depth",
#        y= "Predicted Depth")


#This is a test code line for Luke

  
#retesting turbidity
LightSTTD_test <- LightSTTD_test %>% 
  filter(kdpar_turb <30)

LightSHR_test <- LightSHR_test %>% 
  filter(Concentration<11)

ggplot(LightSTTD_test, aes(x = turbidity, y = kdPAR)) +
  geom_line()+ geom_point() + geom_smooth() + xlim(0,100)+ ylim(0,13)  # scale_x_continuous(trans = "log10") +scale_y_continuous(trans = "log10") 

ggplot(LightSHR_test, aes(x = turbidity, y = kdPAR)) +
  geom_line()+ geom_point() + geom_smooth()+xlim(0,30) #+ scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") 

ggplot(LightSTTD_test, aes(x = kdPAR, y = kdpar_turb)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0) +
  theme_classic()+
  theme(axis.title = element_text(size = 14), title =  element_text(size = 17), axis.text = element_text(size = 12)) +
  labs(x = "Observed KdPAR", y = "Predicted KdPAR", title = "KdPAR Predicted vs Observed at Yolo Bypass")

ggplot(LightSHR_test, aes(x = kdPAR, y = kdpar_turb)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0) +
  theme_classic()+
  theme(axis.title = element_text(size = 14), title =  element_text(size = 17), axis.text = element_text(size = 12)) +
  labs(x = "Observed KdPAR", y = "Predicted KdPAR", title = "KdPAR Predicted vs. Observed at Sacramento River")

?geom_smooth()
?geom_abline

ggplot(LightSTTD_test, aes(x = turbidity, y = kdPAR)) +
  geom_line()+ geom_point() + geom_smooth() + xlim(0,100)+ ylim(0,13)

ggplot(LightSTTD_test,aes(x = zeu_turb, y = Depth, color = "Depths")) + 
  geom_line(aes(y = Concentration*.05, color = "Chla Concentration (mg/L)")) +
  geom_point() +
  scale_y_continuous(sec.axis = sec_axis(~./.05, name = "Concentration (mg/L)")) +
  labs(x = "Predicted Depth", y = "Observed Depth", color = "Values")+
  scale_color_manual(values = c("green3", "red4"))

ggsave("STTDChlavsDepth.png", width = 8, height = 4, units = "in")
  
ggplot(LightSTTD_test, aes(x = Depth, y = Concentration)) +
  geom_point() +
  theme_classic() +
  theme(axis.title = element_text(size = 14), title =  element_text(size = 16), axis.text = element_text(size = 12)) +
  geom_smooth(method = "lm") +
  labs(x = "Observed Photic Zone Depth (m)", y = "Chla Concentration (mg/L)", title = "Photic Zone Depth vs. Chla Concentration at Yolo Bypass")

ggplot(LightSHR_test, aes(x = Depth, y = Concentration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  theme(axis.title = element_text(size = 14), title =  element_text(size = 16), axis.text = element_text(size = 12)) +
  labs(x = "Observed Photic Zone Depth (m)", y = "Chla Concentration (mg/L)", title = "Photic Zone Depth vs. Chla Concentration at Sacramento River")

lm(formula = Concentration ~ Depth, data = LightSTTD_test) %>% 
  summary

