path <- "C:\\Users\\CarinaMueller\\OneDrive - SEI\\RStudio_from_PhD\\GHA_Cocoa" ##
setwd(path)

library(rgee)
library(aws.signature)
library(aws.s3)

library(tidyverse)
library(ggplot2)
library(tidyr)

## loading GEE assets 
######################## Load Territorial ...
#deforestation as outputted from GEE
td <-read.delim2("GHA_cocoa_territorial_deforestation_degradation_q4_2024_25Oct.csv", header=TRUE, sep = "," , dec=".")
tdefS  <- td[,c('LVL_3_CODE','terdef_2000', 'terdef_2001', 'terdef_2002', 'terdef_2003',
                'terdef_2004','terdef_2005','terdef_2006','terdef_2007','terdef_2008','terdef_2009','terdef_2010','terdef_2011','terdef_2012','terdef_2013','terdef_2014','terdef_2015',
                'terdef_2016','terdef_2017','terdef_2018','terdef_2019','terdef_2020','terdef_2021','terdef_2022','terdef_2023')]

#Load Territorial Degradatation
tdegS  <- td[,c('LVL_3_CODE', 'terdeg_2000', 'terdeg_2001', 'terdeg_2002', 'terdeg_2003',
                'terdeg_2004','terdeg_2005','terdeg_2006','terdeg_2007','terdeg_2008','terdeg_2009','terdeg_2010','terdeg_2011','terdeg_2012','terdeg_2013','terdeg_2014','terdeg_2015',
                'terdeg_2016','terdeg_2017','terdeg_2018','terdeg_2019','terdeg_2020','terdeg_2021','terdeg_2022','terdeg_2023')]

#Load Territorial uTMF remaining
tuTMFS  <- td[,c('LVL_3_CODE', 'uTMF_2000', 'uTMF_2001','uTMF_2002','uTMF_2003',
                 'uTMF_2004','uTMF_2005','uTMF_2006','uTMF_2007','uTMF_2008','uTMF_2009','uTMF_2010','uTMF_2011','uTMF_2012','uTMF_2013','uTMF_2014','uTMF_2015',
                 'uTMF_2016','uTMF_2017','uTMF_2018','uTMF_2019','uTMF_2020','uTMF_2021','uTMF_2022','uTMF_2023')]

##Load Gross Territorial Deforestation emissions
tghg <-read.delim2("GHA_cocoa_territorial_deforestation_gross_ghg_q4_2024_25Oct.csv", header=TRUE, sep = "," , dec=".")
tghgS  <- tghg [,c('LVL_3_CODE',  'terdef_2010',
                   'terdef_2011' , 'terdef_2012', 'terdef_2013', 'terdef_2014', 'terdef_2015' , 'terdef_2016', 'terdef_2017', 'terdef_2018',
                   'terdef_2019' , 'terdef_2020', 'terdef_2021', 'terdef_2022', 'terdef_2023')]

###################### Load Cocoa-specific assets
##Load cocoa area
ca <-read.delim2("cocoa_area_ha_29OCt.csv", header=TRUE, sep = "," , dec=".") 
caS  <- ca[,c('LVL_3_CODE', 'sum')]

#Change column name to indicator name
names(caS)[names(caS) == "sum"] <- "cocoa_area_ha_2020"

#Add year as column
caS$YEAR <- c('2020')

##Load 15-year cocoa deforestation:  Only cocoa uTMF loss (defined as deforestation) required for now
cd15 <-read.delim2("GHA_Cocoa_uTMF_loss_15yr_ha_24Oct.csv", header=TRUE, sep = "," , dec=".") 
cd15S  <- cd15[,c('LVL_3_CODE', 'cd_15yr_tmf_loss_2015', 'cd_15yr_tmf_loss_2016', 'cd_15yr_tmf_loss_2017')]
head(cd15S)

#Change years to export years to account for lag period
names(cd15S)[names(cd15S) == "cd_15yr_tmf_loss_2015"] <- "cd_15yr_tmf_loss_2019"
names(cd15S)[names(cd15S) == "cd_15yr_tmf_loss_2016"] <- "cd_15yr_tmf_loss_2020"
names(cd15S)[names(cd15S) == "cd_15yr_tmf_loss_2017"] <- "cd_15yr_tmf_loss_2021"

##Load cocoa deforestation (annual)
cd1 <-read.delim2("GHA_Cocoa_uTMF_loss_ha_1yr_24Oct.csv", header=TRUE, sep = "," , dec=".") 
cd1S  <- cd1[,c('LVL_3_CODE', 'cd_1yr_tmf_loss_2019', 'cd_1yr_tmf_loss_2020', 'cd_1yr_tmf_loss_2021')]
head(cd1S)

#Load Gross emissions
ge <-read.delim2("GHA_cocoa_GROSS_and_NET_ghg_24Oct.csv", header=TRUE, sep = "," , dec=".") 
geS_1yr  <- ge[,c('LVL_3_CODE', 'ghg_cd_1yr_gross_2019','ghg_cd_1yr_gross_2020','ghg_cd_1yr_gross_2021')]
geS_15yr  <- ge[,c('LVL_3_CODE', 'ghg_cd_15yr_gross_2019','ghg_cd_15yr_gross_2020', 'ghg_cd_15yr_gross_2021')]
head(geS_15yr)

#Get Net emissions
neS_1yr  <- ge[,c('LVL_3_CODE',  'ghg_cd_1yr_net_2019','ghg_cd_1yr_net_2020','ghg_cd_1yr_net_2021')]
neS_15yr  <- ge[,c('LVL_3_CODE', 'ghg_cd_15yr_net_2019','ghg_cd_15yr_net_2020', 'ghg_cd_15yr_net_2021')]
head(geS_1yr)

########################### TERRITORIAL METRICS FIRST #########################################
##Pivot longer territorial deforestation data
sub <- tdefS %>%     
  pivot_longer(!LVL_3_CODE, names_to = "YEAR", values_to = "TerritorialDeforestation_ha")
terdef.longer_2019_2022 <- as.data.frame(sub)
head(terdef.longer_2019_2022)

##Delete X in column YEAR
terdef.longer_2019_2022$YEAR <- gsub("terdef_","",as.character(terdef.longer_2019_2022$YEAR))
head(terdef.longer_2019_2022)

##Pivot longer territorial degradation data
subi <- tdegS %>%     
  pivot_longer(!LVL_3_CODE, names_to = "YEAR", values_to = "TerritorialDegradation_ha")
terdeg.longer_2019_2022 <- as.data.frame(subi)
head(terdeg.longer_2019_2022)

terdeg.longer_2019_2022$YEAR <- gsub("terdeg_","",as.character(terdeg.longer_2019_2022$YEAR)) ##Delete X in column YEAR
head(terdeg.longer_2019_2022)

##Pivot longer territorial uTMF data
suba <- tuTMFS %>%     
  pivot_longer(!LVL_3_CODE, names_to = "YEAR", values_to = "undisturbedTropicalMoistForest_ha")
teruTMF.longer_2019_2022 <- as.data.frame(suba)
head(teruTMF.longer_2019_2022)

teruTMF.longer_2019_2022$YEAR <- gsub("uTMF_","",as.character(teruTMF.longer_2019_2022$YEAR)) ##Delete X in column YEAR
head(teruTMF.longer_2019_2022)

##Pivot longer territorial ghg
subg <- tghgS %>%     
  pivot_longer(!LVL_3_CODE, names_to = "YEAR", values_to = "TerritorialGrossGHG_CO2eq")
terghg.longer_2019_2022 <- as.data.frame(subg)
head(terghg.longer_2019_2022)

terghg.longer_2019_2022$YEAR <- gsub("terdef_","",as.character(terghg.longer_2019_2022$YEAR)) ##Delete X in column YEAR
head(terghg.longer_2019_2022)

##Merge Territorial metrics together
Terr_2Metrics <- merge(terdef.longer_2019_2022, terdeg.longer_2019_2022,  by=c("LVL_3_CODE", "YEAR"),  all.x = TRUE, all.y = TRUE)
Terr_3Metrics <- merge(Terr_2Metrics, teruTMF.longer_2019_2022,  by=c("LVL_3_CODE", "YEAR"),  all.x = TRUE, all.y = TRUE)
Terr_4Metrics <- merge(Terr_3Metrics, terghg.longer_2019_2022,  by=c("LVL_3_CODE", "YEAR"),  all.x = TRUE, all.y = TRUE)
head(Terr_4Metrics)

############Cocoa-deforestation and ghg
######################Pivot longer cocoa area
ari1 <- caS %>%     
  pivot_longer(!LVL_3_CODE, names_to = "YEAR", values_to = "Cocoa_area_ha")
ca2020 <- as.data.frame(ari1)
head(ca2020)

ca2020$YEAR <- gsub("cocoa_area_ha_","",as.character(ca2020$YEAR))
head(ca2020)

######################Pivot longer annual cocoa deforestation data
par1 <- cd1S %>%     
  pivot_longer(!LVL_3_CODE, names_to = "YEAR", values_to = "Annual_Cocoa_Deforestation_ha")
cd1.longer_2019_2021 <- as.data.frame(par1)
head(cd1.longer_2019_2021)

cd1.longer_2019_2021$YEAR <- gsub("cd_1yr_tmf_loss_","",as.character(cd1.longer_2019_2021$YEAR))
head(cd1.longer_2019_2021)

##Pivot longer 15-year cocoa deforestation data
par15 <- cd15S %>%     
  pivot_longer(!LVL_3_CODE, names_to = "YEAR", values_to = "15year_Cocoa_Deforestation_ha")
cd15.longer_2019_2021 <- as.data.frame(par15)
head(cd15.longer_2019_2021)

cd15.longer_2019_2021$YEAR <- gsub("cd_15yr_tmf_loss_","",as.character(cd15.longer_2019_2021$YEAR))
cd15.longer_2019_2021

##Merge cocoa deforestation metrics together
Cdef_2Metrics <- merge(cd1.longer_2019_2021, cd15.longer_2019_2021,  by=c("LVL_3_CODE", "YEAR"),  all.x = TRUE, all.y = TRUE)

################################Gross GHG#####################
##Pivot longer Gross annual deforestation
subr <- geS_1yr %>%     
  pivot_longer(!LVL_3_CODE, names_to = "YEAR", values_to = "Cocoa_Gross_Emissions_1yr_CO2eq")
gross_ghg_1yr_longer_2019_2021 <- as.data.frame(subr)
head(gross_ghg_1yr_longer_2019_2021)

gross_ghg_1yr_longer_2019_2021$YEAR <- gsub("ghg_cd_1yr_gross_","",as.character(gross_ghg_1yr_longer_2019_2021$YEAR))
gross_ghg_1yr_longer_2019_2021

##Pivot longer gross 15-year deforestation
suba <- geS_15yr %>%     
  pivot_longer(!LVL_3_CODE, names_to = "YEAR", values_to = "Cocoa_Gross_Emissions_15yr_CO2eq")
gross_ghg_15yr_longer_2019_2021 <- as.data.frame(suba)
head(gross_ghg_15yr_longer_2019_2021)

gross_ghg_15yr_longer_2019_2021$YEAR <- gsub("ghg_cd_15yr_gross_","",as.character(gross_ghg_15yr_longer_2019_2021$YEAR))
head(gross_ghg_15yr_longer_2019_2021)

################################Net GHG#####################
##Pivot longer Net annual deforestation
subrn <- neS_1yr %>%     
  pivot_longer(!LVL_3_CODE, names_to = "YEAR", values_to = "Cocoa_Net_Emissions_1yr_CO2eq")
net_ghg_1yr_longer_2019_2021 <- as.data.frame(subrn)
head(net_ghg_1yr_longer_2019_2021)

net_ghg_1yr_longer_2019_2021$YEAR <- gsub("ghg_cd_1yr_net_","",as.character(net_ghg_1yr_longer_2019_2021$YEAR))
head(net_ghg_1yr_longer_2019_2021)

suban <- neS_15yr %>%     
  pivot_longer(!LVL_3_CODE, names_to = "YEAR", values_to = "Cocoa_Net_Emissions_15yr_CO2eq")
net_ghg_15yr_longer_2019_2021 <- as.data.frame(suban)
head(net_ghg_15yr_longer_2019_2021)

net_ghg_15yr_longer_2019_2021$YEAR <- gsub("ghg_cd_15yr_net_","",as.character(net_ghg_15yr_longer_2019_2021$YEAR))
head(net_ghg_15yr_longer_2019_2021)

##Merge all Cocoa-metrics together
Cdef_3Metrics <- merge(Cdef_2Metrics, gross_ghg_1yr_longer_2019_2021,  by=c("LVL_3_CODE", "YEAR"),  all.x = TRUE, all.y = TRUE)
Cdef_4Metrics <- merge(Cdef_3Metrics, gross_ghg_15yr_longer_2019_2021,  by=c("LVL_3_CODE", "YEAR"),  all.x = TRUE, all.y = TRUE)
Cdef_5Metrics <- merge(Cdef_4Metrics, net_ghg_1yr_longer_2019_2021,  by=c("LVL_3_CODE", "YEAR"),  all.x = TRUE, all.y = TRUE)
Cdef_6Metrics <- merge(Cdef_5Metrics, net_ghg_15yr_longer_2019_2021,  by=c("LVL_3_CODE", "YEAR"),  all.x = TRUE, all.y = TRUE)
head(Cdef_6Metrics)

# save territorial metrics to S3
s3write_using(Terr_4Metrics, object = "ghana/cocoa/indicators/in/q4_2024/Territorial_deforestation_degradation_uTMF_ghg_2004-2023_q4_2024_29Oct.csv",
              bucket = "trase-storage", FUN  = write_delim, delim = ";", opts = c("check_region" = T))

# save territorial metrics to S3
s3write_using(Cdef_6Metrics, object = "ghana/cocoa/indicators/in/q4_2024/Cocoa_Deforestation_and_GrossNetGHG_Annual_and_15yr_each_q4_202429Oct.csv",
              bucket = "trase-storage", FUN  = write_delim, delim = ";", opts = c("check_region" = T))

# save territorial metrics to S3
s3write_using(ca2020, object = "ghana/cocoa/indicators/in/q4_2024/Cocoa_area_ha_q4_2024_29Oct.csv",
              bucket = "trase-storage", FUN  = write_delim, delim = ";", opts = c("check_region" = T))

#Export to own drive
write.csv(Terr_4Metrics, "C:\\Users\\CarinaMueller\\OneDrive - SEI\\RStudio_from_PhD\\GHA_Cocoa\\CIV_Cococoa_TerDef_toS3Exported.csv", row.names=FALSE)

#Export to own drive
write.csv(Cdef_6Metrics, "C:\\Users\\CarinaMueller\\OneDrive - SEI\\RStudio_from_PhD\\GHA_Cocoa\\Cdef_6Metrics_ExportedToS3.csv", row.names=FALSE)
