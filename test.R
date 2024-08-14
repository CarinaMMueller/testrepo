## author: florian.gollnow@sei.org
## this script calculates soy deforestation 2013-2021 using deforestation combined from Prodes (Amazon and Cerrado) and Mapbiomas (note Mapbiomas only maps deforestation until 2021)
## and song soy data 2001 - 2022 (20ha minimal mapping unit [clump and eliminate])
## we only use deforestation starting in 2008 (because of prodes), which limits us to soy-deforestation starting in 2013 (5 year allocation window)

path <- "C:\\Users\\CarinaMueller\\OneDrive - SEI\\RStudio_from_PhD\\Ind_Palm" ##
setwd(path)

library(rgee)
library(aws.signature)
library(aws.s3)

library(tidyverse)
library(ggplot2)

## loading GEE assets (only years of 2021 and 2022 needed!)
Subsi <-read.delim2("palm_peat_subsidence_conc_22July_2001TO2022.csv", header=TRUE, sep = "," , dec=".") 
SubsiS  <- Subsi[,c('palm_SubsiGHG_tCO2_2001','palm_SubsiGHG_tCO2_2002','palm_SubsiGHG_tCO2_2003','palm_SubsiGHG_tCO2_2004','palm_SubsiGHG_tCO2_2005'
                    ,'palm_SubsiGHG_tCO2_2006','palm_SubsiGHG_tCO2_2007','palm_SubsiGHG_tCO2_2008','palm_SubsiGHG_tCO2_2009','palm_SubsiGHG_tCO2_2010','palm_SubsiGHG_tCO2_2011','palm_SubsiGHG_tCO2_2012'
                    ,'palm_SubsiGHG_tCO2_2013','palm_SubsiGHG_tCO2_2014','palm_SubsiGHG_tCO2_2015','palm_SubsiGHG_tCO2_2016','palm_SubsiGHG_tCO2_2017','palm_SubsiGHG_tCO2_2018','palm_SubsiGHG_tCO2_2019'
                    ,'palm_SubsiGHG_tCO2_2020','palm_SubsiGHG_tCO2_2021','palm_SubsiGHG_tCO2_2022','ffb_code')]

Gross <-read.delim2("palm_defor_gross_GHG_3JulyFINAL.csv", header=TRUE, sep = "," , dec=".") 
GrossS <- Gross[,c('pd_2001','pd_2002','pd_2003','pd_2004','pd_2005','pd_2006','pd_2007','pd_2008','pd_2009','pd_2010','pd_2011','pd_2012','pd_2013','pd_2014','pd_2015','pd_2016','pd_2017','pd_2018','pd_2019','pd_2020','pd_2021','pd_2022'
                   ,'pnfl_2001','pnfl_2002','pnfl_2003','pnfl_2004','pnfl_2005','pnfl_2006','pnfl_2007','pnfl_2008','pnfl_2009','pnfl_2010','pnfl_2011','pnfl_2012','pnfl_2013','pnfl_2014','pnfl_2015','pnfl_2016','pnfl_2017'
                   ,'pnfl_2018','pnfl_2019','pnfl_2020','pnfl_2021','pnfl_2022','ffb_code')]

CSeq <-read.delim2("CSeq_DefandNonFor_Palm_3JulyFINAL.csv", header=TRUE, sep = "," , dec=".") 
CSeqS <- CSeq[,c('CSeq_2001','CSeq_2002','CSeq_2003','CSeq_2004','CSeq_2005','CSeq_2006','CSeq_2007','CSeq_2008','CSeq_2009','CSeq_2010','CSeq_2011','CSeq_2012','CSeq_2013','CSeq_2014','CSeq_2015','CSeq_2016','CSeq_2017'
                 ,'CSeq_2018','CSeq_2019','CSeq_2020','CSeq_2021','CSeq_2022'
                 ,'ffb_code')]

Fire_Ghg <-read.delim2("LandCoverSharesOnPeat_andEmissions4JulyFINAL.csv", header=TRUE, sep = "," , dec=".") 

##Territorial Deforestation
TerDef <-read.delim2("INDter_defor_gross_GHG_11July.csv", header=TRUE, sep = "," , dec=".") 
TerDefS <- TerDef[,c('td_2001','td_2002','td_2003','td_2004','td_2005','td_2006','td_2007','td_2008','td_2009','td_2010','td_2011','td_2012','td_2013','td_2014','td_2015','td_2016','td_2017','td_2018','td_2019','td_2020','td_2021','td_2022'
                     ,'tnfl_2001','tnfl_2002','tnfl_2003','tnfl_2004','tnfl_2005','tnfl_2006','tnfl_2007','tnfl_2008','tnfl_2009','tnfl_2010','tnfl_2011','tnfl_2012','tnfl_2013','tnfl_2014','tnfl_2015','tnfl_2016','tnfl_2017'
                     ,'tnfl_2018','tnfl_2019','tnfl_2020','tnfl_2021','tnfl_2022','ffb_code')]

head(TerDefS)

##Carbon Sequestration in Territorial Deforestation to caluclate Net if needed
C_TerDef <-read.delim2("CSeq_TerDef_DefandNonFor_Palm_CO2eq_8July_B.csv", header=TRUE, sep = "," , dec=".") 
C_TerDefS <- C_TerDef[,c('ctd_2001','ctd_2002','ctd_2003','ctd_2004','ctd_2005','ctd_2006','ctd_2007','ctd_2008','ctd_2009','ctd_2010','ctd_2011','ctd_2012','ctd_2013','ctd_2014','ctd_2015','ctd_2016','ctd_2017','ctd_2018','ctd_2019','ctd_2020','ctd_2021','ctd_2022'
                         ,'ffb_code')]

##Pivot longer subsidence data
sub <- SubsiS %>%      #######CHANGE HERE
  pivot_longer(!ffb_code, names_to = "YEAR", values_to = "Subsidence_GHG_tCO2eq")
subsidence.longer_2001to2022 <- as.data.frame(sub)
head(subsidence.longer_2001to2022)

##Delete X in column YEAR
subsidence.longer_2001to2022$YEAR <- gsub("palm_SubsiGHG_tCO2_","",as.character(subsidence.longer_2001to2022$YEAR))
####################################################

############### Calculate Net LUC emissions
#Merge pdf and pnfl 
GrossS$pd_nfl_2001 <- GrossS$pd_2001 + GrossS$pnfl_2001
GrossS$pd_nfl_2002 <- GrossS$pd_2002 + GrossS$pnfl_2002
GrossS$pd_nfl_2003 <- GrossS$pd_2003 + GrossS$pnfl_2003
GrossS$pd_nfl_2004 <- GrossS$pd_2004 + GrossS$pnfl_2004
GrossS$pd_nfl_2005 <- GrossS$pd_2005 + GrossS$pnfl_2005
GrossS$pd_nfl_2006 <- GrossS$pd_2006 + GrossS$pnfl_2006
GrossS$pd_nfl_2007 <- GrossS$pd_2007 + GrossS$pnfl_2007
GrossS$pd_nfl_2008 <- GrossS$pd_2008 + GrossS$pnfl_2008
GrossS$pd_nfl_2009 <- GrossS$pd_2009 + GrossS$pnfl_2009
GrossS$pd_nfl_2010 <- GrossS$pd_2010 + GrossS$pnfl_2010
GrossS$pd_nfl_2011 <- GrossS$pd_2011 + GrossS$pnfl_2011
GrossS$pd_nfl_2012 <- GrossS$pd_2012 + GrossS$pnfl_2012
GrossS$pd_nfl_2013 <- GrossS$pd_2013 + GrossS$pnfl_2013
GrossS$pd_nfl_2014 <- GrossS$pd_2014 + GrossS$pnfl_2014
GrossS$pd_nfl_2015 <- GrossS$pd_2015 + GrossS$pnfl_2015
GrossS$pd_nfl_2016 <- GrossS$pd_2016 + GrossS$pnfl_2016
GrossS$pd_nfl_2017 <- GrossS$pd_2017 + GrossS$pnfl_2017
GrossS$pd_nfl_2018 <- GrossS$pd_2018 + GrossS$pnfl_2018
GrossS$pd_nfl_2019 <- GrossS$pd_2019 + GrossS$pnfl_2019
GrossS$pd_nfl_2020 <- GrossS$pd_2020 + GrossS$pnfl_2020
GrossS$pd_nfl_2021 <- GrossS$pd_2021 + GrossS$pnfl_2021
GrossS$pd_nfl_2022 <- GrossS$pd_2022 + GrossS$pnfl_2022

GrossT <- GrossS[,c('pd_nfl_2001','pd_nfl_2002','pd_nfl_2003','pd_nfl_2004','pd_nfl_2005','pd_nfl_2006','pd_nfl_2007','pd_nfl_2008','pd_nfl_2009','pd_nfl_2010','pd_nfl_2011','pd_nfl_2012','pd_nfl_2013','pd_nfl_2014',
                    'pd_nfl_2015','pd_nfl_2016','pd_nfl_2017','pd_nfl_2018','pd_nfl_2019','pd_nfl_2020','pd_nfl_2021','pd_nfl_2022', 'ffb_code')]

#Pivot longer Gross LUC GHG
gross <- GrossT %>%      #######CHANGE HERE
  pivot_longer(!ffb_code, names_to = "YEAR", values_to = "GrossLUC_GHG_tCO2eq")
gross.longer <- as.data.frame(gross)

gross.longer$YEAR <- gsub("pd_nfl_","",as.character(gross.longer$YEAR))

#Pivot longer C Seq
seq <- CSeqS %>%      #######CHANGE HERE
  pivot_longer(!ffb_code, names_to = "YEAR", values_to = "CSeq_GHG_tCO2eq")
CSeq.longer <- as.data.frame(seq)

CSeq.longer$YEAR <- gsub("CSeq_","",as.character(CSeq.longer$YEAR))

##Merge Gross and CSeq to calculate Net afterwards
Gross_CSeq <- merge(gross.longer, CSeq.longer,  by=c("ffb_code", "YEAR"),  all.x = TRUE, all.y = TRUE)
Gross_CSeq$NetLUC_GHG_tCO2eq <- Gross_CSeq$GrossLUC_GHG_tCO2eq - Gross_CSeq$CSeq_GHG_tCO2eq
###############

#### Fire on Peat burn emissions #####
head(Fire_Ghg)

Fire_Ghg$X2015_TotalAgriLC <- Fire_Ghg$aquac_peat_2015 + Fire_Ghg$other_peat_2015 + Fire_Ghg$palm_peat_2015 + Fire_Ghg$pulp_peat_2015 + Fire_Ghg$rice_peat_2015
Fire_Ghg$X2015_PalmShare <- Fire_Ghg$palm_peat_2015/Fire_Ghg$X2015_TotalAgriLC

Fire_Ghg$X2016_TotalAgriLC <- Fire_Ghg$aquac_peat_2016 + Fire_Ghg$other_peat_2016 + Fire_Ghg$palm_peat_2016 + Fire_Ghg$pulp_peat_2016 + Fire_Ghg$rice_peat_2016
Fire_Ghg$X2016_PalmShare <- Fire_Ghg$palm_peat_2016/Fire_Ghg$X2016_TotalAgriLC

Fire_Ghg$X2017_TotalAgriLC <- Fire_Ghg$aquac_peat_2017 + Fire_Ghg$other_peat_2017 + Fire_Ghg$palm_peat_2017 + Fire_Ghg$pulp_peat_2017 + Fire_Ghg$rice_peat_2017
Fire_Ghg$X2017_PalmShare <- Fire_Ghg$palm_peat_2017/Fire_Ghg$X2017_TotalAgriLC

Fire_Ghg$X2018_TotalAgriLC <- Fire_Ghg$aquac_peat_2018 + Fire_Ghg$other_peat_2018 + Fire_Ghg$palm_peat_2018 + Fire_Ghg$pulp_peat_2018 + Fire_Ghg$rice_peat_2018
Fire_Ghg$X2018_PalmShare <- Fire_Ghg$palm_peat_2018/Fire_Ghg$X2018_TotalAgriLC

Fire_Ghg$X2019_TotalAgriLC <- Fire_Ghg$aquac_peat_2019 + Fire_Ghg$other_peat_2019 + Fire_Ghg$palm_peat_2019 + Fire_Ghg$pulp_peat_2019 + Fire_Ghg$rice_peat_2019
Fire_Ghg$X2019_PalmShare <- Fire_Ghg$palm_peat_2019/Fire_Ghg$X2019_TotalAgriLC

Fire_Ghg$X2020_TotalAgriLC <- Fire_Ghg$aquac_peat_2020 + Fire_Ghg$other_peat_2020 + Fire_Ghg$palm_peat_2020 + Fire_Ghg$pulp_peat_2020 + Fire_Ghg$rice_peat_2020
Fire_Ghg$X2020_PalmShare <- Fire_Ghg$palm_peat_2020/Fire_Ghg$X2020_TotalAgriLC

Fire_Ghg$X2021_TotalAgriLC <- Fire_Ghg$aquac_peat_2021 + Fire_Ghg$other_peat_2021 + Fire_Ghg$palm_peat_2021 + Fire_Ghg$pulp_peat_2021 + Fire_Ghg$rice_peat_2021
Fire_Ghg$X2021_PalmShare <- Fire_Ghg$palm_peat_2021/Fire_Ghg$X2021_TotalAgriLC

Fire_Ghg$X2022_TotalAgriLC <- Fire_Ghg$aquac_peat_2022 + Fire_Ghg$other_peat_2022 + Fire_Ghg$palm_peat_2022 + Fire_Ghg$pulp_peat_2022 + Fire_Ghg$rice_peat_2022
Fire_Ghg$X2022_PalmShare <- Fire_Ghg$palm_peat_2022/Fire_Ghg$X2022_TotalAgriLC

##IF column NA, replace with zero
Fire_Ghg$X2015_PalmShare[is.na(Fire_Ghg$X2015_PalmShare)] <- 0 
Fire_Ghg$X2016_PalmShare[is.na(Fire_Ghg$X2016_PalmShare)] <- 0 
Fire_Ghg$X2017_PalmShare[is.na(Fire_Ghg$X2017_PalmShare)] <- 0 
Fire_Ghg$X2018_PalmShare[is.na(Fire_Ghg$X2018_PalmShare)] <- 0 
Fire_Ghg$X2019_PalmShare[is.na(Fire_Ghg$X2019_PalmShare)] <- 0 
Fire_Ghg$X2020_PalmShare[is.na(Fire_Ghg$X2020_PalmShare)] <- 0 
Fire_Ghg$X2021_PalmShare[is.na(Fire_Ghg$X2021_PalmShare)] <- 0 
Fire_Ghg$X2022_PalmShare[is.na(Fire_Ghg$X2022_PalmShare)] <- 0 

##Multiply Palm oil shares with each FFB´s peat burn emissions per year
Fire_Ghg$Fire_GHG_Palm2015 <- Fire_Ghg$peat_burn_2015 * Fire_Ghg$X2015_PalmShare
Fire_Ghg$Fire_GHG_Palm2016 <- Fire_Ghg$peat_burn_2016 * Fire_Ghg$X2016_PalmShare
Fire_Ghg$Fire_GHG_Palm2017 <- Fire_Ghg$peat_burn_2017 * Fire_Ghg$X2017_PalmShare
Fire_Ghg$Fire_GHG_Palm2018 <- Fire_Ghg$peat_burn_2018 * Fire_Ghg$X2018_PalmShare
Fire_Ghg$Fire_GHG_Palm2019 <- Fire_Ghg$peat_burn_2019 * Fire_Ghg$X2019_PalmShare
Fire_Ghg$Fire_GHG_Palm2020 <- Fire_Ghg$peat_burn_2020 * Fire_Ghg$X2020_PalmShare
Fire_Ghg$Fire_GHG_Palm2021 <- Fire_Ghg$peat_burn_2021 * Fire_Ghg$X2021_PalmShare
Fire_Ghg$Fire_GHG_Palm2022 <- Fire_Ghg$peat_burn_2022 * Fire_Ghg$X2022_PalmShare

sum(Fire_Ghg$Fire_GHG_Palm2015)/1000000
sum(Fire_Ghg$Fire_GHG_Palm2016)/1000000
sum(Fire_Ghg$Fire_GHG_Palm2017)/1000000
sum(Fire_Ghg$Fire_GHG_Palm2018)/1000000
sum(Fire_Ghg$Fire_GHG_Palm2019)/1000000
sum(Fire_Ghg$Fire_GHG_Palm2020)/1000000
sum(Fire_Ghg$Fire_GHG_Palm2021)/1000000
sum(Fire_Ghg$Fire_GHG_Palm2022)/1000000

head(Fire_Ghg)

##Pivot-longer the Ghg fire data
Fire_GhgS  <- Fire_Ghg[,c('Fire_GHG_Palm2015','Fire_GHG_Palm2016','Fire_GHG_Palm2017','Fire_GHG_Palm2018','Fire_GHG_Palm2019','Fire_GHG_Palm2020','Fire_GHG_Palm2021','Fire_GHG_Palm2022','ffb_code')]
fire <- Fire_GhgS %>%      #######CHANGE HERE
  pivot_longer(!ffb_code, names_to = "YEAR", values_to = "Fire_on_peat_GHG_tCO2eq")
fires.longer_2015to2022 <- as.data.frame(fire)
head(fires.longer_2015to2022)

##Delete X in column YEAR
fires.longer_2015to2022$YEAR <- gsub("Fire_GHG_Palm","",as.character(fires.longer_2015to2022$YEAR))

##Quality-check: Sum across Indonesia related to Palm oil?
sum(Fire_Ghg$peat_burn_2015)/1000000
sum(Fire_Ghg$peat_burn_2016)/1000000
sum(Fire_Ghg$peat_burn_2017)/1000000
sum(Fire_Ghg$peat_burn_2018)/1000000
sum(Fire_Ghg$peat_burn_2019)/1000000
sum(Fire_Ghg$peat_burn_2020)/1000000
sum(Fire_Ghg$peat_burn_2021)/1000000
sum(Fire_Ghg$peat_burn_2022)/1000000

head(Gross_CSeq)

GrossandNet_FINAL <- Gross_CSeq[,c('YEAR', 'ffb_code',  'NetLUC_GHG_tCO2eq')] #'GrossLUC_GHG_tCO2eq',

# save to S3
s3write_using(GrossandNet_FINAL,
              write.table,
              sep = ";",
              object = "indonesia/palm_oil/indicators/out/q1_2024/GHG_Net_palm_LUC2001To2022_q1_2024_8July.csv",
              opts = c("check_region" = TRUE),
              bucket = "trase-storage"
)

s3write_using(subsidence.longer_2001to2022,
              write.table,
              sep = ";",
              object = "indonesia/palm_oil/indicators/out/q1_2024/GHG_palm_Subsidence_2001To2022_q1_2024_23July.csv",
              opts = c("check_region" = TRUE),
              bucket = "trase-storage"
)

s3write_using(fires.longer_2015to2022,
              write.table,
              sep = ";",
              object = "indonesia/palm_oil/indicators/out/q1_2024/GHG_palm_FiresOnPeat_2015To2022_q1_2024_5July.csv",
              opts = c("check_region" = TRUE),
              bucket = "trase-storage"
)

############### Gross Territorial Deforestation
#Merge conversion of non-forest and forest together 
TerDefS$td_nfl_2001 <- TerDefS$td_2001 + TerDefS$tnfl_2001
TerDefS$td_nfl_2002 <- TerDefS$td_2002 + TerDefS$tnfl_2002
TerDefS$td_nfl_2003 <- TerDefS$td_2003 + TerDefS$tnfl_2003
TerDefS$td_nfl_2004 <- TerDefS$td_2004 + TerDefS$tnfl_2004
TerDefS$td_nfl_2005 <- TerDefS$td_2005 + TerDefS$tnfl_2005
TerDefS$td_nfl_2006 <- TerDefS$td_2006 + TerDefS$tnfl_2006
TerDefS$td_nfl_2007 <- TerDefS$td_2007 + TerDefS$tnfl_2007
TerDefS$td_nfl_2008 <- TerDefS$td_2008 + TerDefS$tnfl_2008
TerDefS$td_nfl_2009 <- TerDefS$td_2009 + TerDefS$tnfl_2009
TerDefS$td_nfl_2010 <- TerDefS$td_2010 + TerDefS$tnfl_2010
TerDefS$td_nfl_2011 <- TerDefS$td_2011 + TerDefS$tnfl_2011
TerDefS$td_nfl_2012 <- TerDefS$td_2012 + TerDefS$tnfl_2012
TerDefS$td_nfl_2013 <- TerDefS$td_2013 + TerDefS$tnfl_2013
TerDefS$td_nfl_2014 <- TerDefS$td_2014 + TerDefS$tnfl_2014
TerDefS$td_nfl_2015 <- TerDefS$td_2015 + TerDefS$tnfl_2015
TerDefS$td_nfl_2016 <- TerDefS$td_2016 + TerDefS$tnfl_2016
TerDefS$td_nfl_2017 <- TerDefS$td_2017 + TerDefS$tnfl_2017
TerDefS$td_nfl_2018 <- TerDefS$td_2018 + TerDefS$tnfl_2018
TerDefS$td_nfl_2019 <- TerDefS$td_2019 + TerDefS$tnfl_2019
TerDefS$td_nfl_2020 <- TerDefS$td_2020 + TerDefS$tnfl_2020
TerDefS$td_nfl_2021 <- TerDefS$td_2021 + TerDefS$tnfl_2021
TerDefS$td_nfl_2022 <- TerDefS$td_2022 + TerDefS$tnfl_2022
head(TerDefS)

GrossTerDefFINAL <- TerDefS[,c('td_nfl_2001','td_nfl_2002','td_nfl_2003','td_nfl_2004','td_nfl_2005','td_nfl_2006','td_nfl_2007','td_nfl_2008','td_nfl_2009','td_nfl_2010','td_nfl_2011','td_nfl_2012','td_nfl_2013','td_nfl_2014',
                               'td_nfl_2015','td_nfl_2016','td_nfl_2017','td_nfl_2018','td_nfl_2019','td_nfl_2020','td_nfl_2021','td_nfl_2022', 'ffb_code')]

#Pivot longer Gross LUC GHG
gross <- GrossTerDefFINAL %>%      #######CHANGE HERE
  pivot_longer(!ffb_code, names_to = "YEAR", values_to = "TerDef_and_nonforestloss_Gross_GHG_tCO2eq")
TerDef_gross.longer <- as.data.frame(gross)

head(TerDef_gross.longer)

TerDef_gross.longer$YEAR <- gsub("td_nfl_","",as.character(TerDef_gross.longer$YEAR))

s3write_using(TerDef_gross.longer,
              write.table,
              sep = ";",
              object = "indonesia/palm_oil/indicators/out/q1_2024/GHG_GrossTerDef_forestandnonforest_2001To2022_q1_2024_11July.csv",
              opts = c("check_region" = TRUE),
              bucket = "trase-storage"
)


## NET Territorial Deforestation? 
#Pivot longer CSeq  LUC GHG
CSeq <- C_TerDefS %>%      #######CHANGE HERE
  pivot_longer(!ffb_code, names_to = "YEAR", values_to = "TerDef_and_nonforestloss_CSeq_GHG_tCO2eq")
CSeq_gross.longer <- as.data.frame(CSeq)

head(CSeq_gross.longer)

CSeq_gross.longer$YEAR <- gsub("ctd_","",as.character(CSeq_gross.longer$YEAR))

##Merge Gross and CSeq to calculate Net afterwards
TD_Gross_CSeq <- merge(TerDef_gross.longer, CSeq_gross.longer,  by=c("ffb_code", "YEAR"),  all.x = TRUE, all.y = TRUE)
head(TD_Gross_CSeq)

TD_Gross_CSeq$NetLUC_GHG_tCO2eq <- TD_Gross_CSeq$TerDef_and_nonforestloss_Gross_GHG_tCO2eq - TD_Gross_CSeq$TerDef_and_nonforestloss_CSeq_GHG_tCO2eq

TD_Gross_CSeq_FINAL <- TD_Gross_CSeq[,c('NetLUC_GHG_tCO2eq','YEAR', 'ffb_code')]

s3write_using(TD_Gross_CSeq_FINAL,
              write.table,
              sep = ";",
              object = "indonesia/palm_oil/indicators/out/q1_2024/GHG_NetTerDef_forestandnonforest_2001To2022_q1_2024_8July.csv",
              opts = c("check_region" = TRUE),
              bucket = "trase-storage"
)




sum(TerDefFINAL$td_nfl_2018)/1000000
sum(TerDefFINAL$td_nfl_2019)/1000000
sum(TerDefFINAL$td_nfl_2020)/1000000
sum(TerDefFINAL$td_nfl_2021)/1000000
sum(TerDefFINAL$td_nfl_2022)/1000000

###############Subsidence ##########################
sum(SubsiS$palm_SubsiGHG_tCO2_2013)/1000000
sum(SubsiS$palm_SubsiGHG_tCO2_2014)/1000000
sum(SubsiS$palm_SubsiGHG_tCO2_2015)/1000000
sum(SubsiS$palm_SubsiGHG_tCO2_2016)/1000000
sum(SubsiS$palm_SubsiGHG_tCO2_2017)/1000000
sum(SubsiS$palm_SubsiGHG_tCO2_2018)/1000000
sum(SubsiS$palm_SubsiGHG_tCO2_2019)/1000000
sum(SubsiS$palm_SubsiGHG_tCO2_2020)/1000000
sum(SubsiS$palm_SubsiGHG_tCO2_2021)/1000000
sum(SubsiS$palm_SubsiGHG_tCO2_2022)/1000000

GrossT
sum(GrossT$pd_nfl_2020)/1000000
sum(GrossT$pd_nfl_2021)/1000000
sum(GrossT$pd_nfl_2022)/1000000


sum(CSeqS$CSeq_2022)/1000000
sum(CSeqS$CSeq_2021)/1000000
sum(CSeqS$CSeq_2020)/1000000