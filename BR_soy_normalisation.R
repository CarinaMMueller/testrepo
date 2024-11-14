## author: florian.gollnow@sei.org
## this script calculates soy deforestation 2013-2021 using deforestation combined from Prodes (Amazon and Cerrado) and Mapbiomas (note Mapbiomas only maps deforestation until 2021)
## and song soy data 2001 - 2022 (20ha minimal mapping unit [clump and eliminate])
## we only use deforestation starting in 2008 (because of prodes), which limits us to soy-deforestation starting in 2013 (5 year allocation window)

path <- "C:\\Users\\CarinaMueller\\OneDrive - SEI\\RStudio_from_PhD\\BR_Soy" ##
setwd(path)

library(rgee)
library(sf)
library(aws.signature)
library(aws.s3)

library(tidyverse)
library(ggplot2)
library(dplyr)


## loading GEE assets
GrossGhg <-read.delim2("Soy_Gross_GHG_20xx_CO2eq_5yr_5AprNEW.csv", header=TRUE, sep = "," , dec=".") #XYear is the column of Soy Deforestation Emissions per municipality 
GrossGhgS  <- GrossGhg[,c('X2022','X2021','X2020','X2019','X2018','X2017','X2016','X2015','X2014','X2013', 'TRASE_ID')]

NetGhg <-read.delim2("Soy_Net_GHG_20xx_CO2eq_soy_br5Apr.csv", header=TRUE, sep = "," , dec=".") #XYear is the column of Net Soy Deforestation Emissions per municipality 
NetGhgS  <- NetGhg[,c('X2022','X2021','X2020','X2019','X2018','X2017','X2016','X2015','X2014','X2013', 'TRASE_ID')]

##Load Terrestrial Deforestation GRoss
TerDefGrossGhg <-read.delim2("BRSoy_TerDef_GrossGHG_20xx_CO2eqNewNAmingX.csv", header=TRUE, sep = "," , dec=".") 
TerDefGrossGhgS  <- TerDefGrossGhg[,c('XG2003', 'XG2004','XG2005','XG2006','XG2007','XG2008','XG2009','XG2010','XG2011','XG2012','XG2013','XG2014','XG2015','XG2016','XG2017','XG2018'
                      ,'XG2019','XG2020','XG2021','XG2022', 'TRASE_ID')]

head(TerDefGrossGhgS)

##Many separate files as GEE did not manage more than 2 years at a time
CseqTD_21and22 <-read.delim2("BRSoy_TerDef_CSeq_20xx_CO2eq_19_13June_2021and2022.csv", header=TRUE, sep = "," , dec=".") #XYear is the column of Net Soy Deforestation Emissions per municipality 
CseqTD_21and22S  <- CseqTD_21and22[,c('X2021','X2022', 'TRASE_ID')]
##sum(CseqTD_21and22S$X2021)/1000000

CseqTD_19and20 <-read.delim2("BRSoy_TerDef_CSeq_20xx_CO2eq_19_13June_2019and2020.csv", header=TRUE, sep = "," , dec=".")
CseqTD_19and20S  <- CseqTD_19and20[,c('X2019','X2020', 'TRASE_ID')]
sum(CseqTD_19and20S$X2020)/1000000

CseqTD_17and18 <-read.delim2("BRSoy_TerDef_CSeq_20xx_CO2eq_19_13June_2017and2018.csv", header=TRUE, sep = "," , dec=".")
CseqTD_17and18S  <- CseqTD_17and18[,c('X2017','X2018', 'TRASE_ID')]

CseqTD_15and16 <-read.delim2("BRSoy_TerDef_CSeq_20xx_CO2eq_19_13June_2015and2016.csv", header=TRUE, sep = "," , dec=".")
CseqTD_15and16S  <- CseqTD_15and16[,c('X2015','X2016', 'TRASE_ID')]

CseqTD_13and14 <-read.delim2("BRSoy_TerDef_CSeq_20xx_CO2eq_19_13June_2013and2014.csv", header=TRUE, sep = "," , dec=".")
CseqTD_13and14S  <- CseqTD_13and14[,c('X2013','X2014', 'TRASE_ID')]

CseqTD_11and12 <-read.delim2("BRSoy_TerDef_CSeq_20xx_CO2eq_19_13June_2011and2012.csv", header=TRUE, sep = "," , dec=".")
CseqTD_11and12S  <- CseqTD_11and12[,c('X2011','X2012', 'TRASE_ID')]

CseqTD_09and10 <-read.delim2("BRSoy_TerDef_CSeq_20xx_CO2eq_19_13June_2009and2010.csv", header=TRUE, sep = "," , dec=".")
CseqTD_09and10S  <- CseqTD_09and10[,c('X2009','X2010', 'TRASE_ID')]

CseqTD_07and08 <-read.delim2("BRSoy_TerDef_CSeq_20xx_CO2eq_19_13June_2007and2008.csv", header=TRUE, sep = "," , dec=".")
CseqTD_07and08S  <- CseqTD_07and08[,c('X2007','X2008', 'TRASE_ID')]

CseqTD_05and06 <-read.delim2("BRSoy_TerDef_CSeq_20xx_CO2eq_19_13June_2005and2006.csv", header=TRUE, sep = "," , dec=".")
CseqTD_05and06S  <- CseqTD_05and06[,c('X2005','X2006', 'TRASE_ID')]

CseqTD_03and04 <-read.delim2("BRSoy_TerDef_CSeq_20xx_CO2eq_19_13June_2003and2004.csv", header=TRUE, sep = "," , dec=".")
CseqTD_03and04S  <- CseqTD_03and04[,c('X2003','X2004', 'TRASE_ID')]

##BS <-read.delim2("BRAZIL_SOY_2.6.0_pc.csv", header=TRUE, sep = "," , dec=".") #
##BSs <- BS[,c('MUNICIPALITY.OF.PRODUCTION', 'TRASE_GEOCODE',  'STATE')]## 'YEAR', BACK
##BSs20 <-BSs[BSs$YEAR=="2020",] ## Select only years I need

## loading IBGE soy production data from S3
soy_prod <-read.delim2("soy_production_IBGE_2003_2022_readable.csv", header=TRUE, sep = "," , dec=".") #From S3
soy_prod  <- soy_prod[,c('HA','YEAR','TN','YIELD',  'TRASE_ID')]
soy_prod$YEAR <- sub("^", "X", soy_prod$YEAR)

## loading IBGE soy production data from S3
soyarea_song <-read.delim2("Soy_glad_20xx_area_soy_br5Apr.csv", header=TRUE, sep = "," , dec=".") #From GEE
soyarea_songS  <- soyarea_song[,c('X2022','X2021','X2020','X2019','X2018','X2017','X2016','X2015','X2014','X2013', 'TRASE_ID')]

##Pivot-longer the soy Hansen area data 
dm.longer <- soyarea_songS %>%      #######CHANGE HERE
  pivot_longer(!TRASE_ID, names_to = "YEAR", values_to = "Soy_AreaSong_HA")
soyarea_songS.longer <- as.data.frame(dm.longer)

##Pivot-longer the Ghg data Gross
dn.longer <- GrossGhgS %>%      #######CHANGE HERE
  pivot_longer(!TRASE_ID, names_to = "YEAR", values_to = "Soy_Def_GrossGHG_CO2eq")
GrossGhg.longer <- as.data.frame(dn.longer)

##Pivot-longer the Ghg data Net
di.longer <- NetGhgS %>%      #######CHANGE HERE
  pivot_longer(!TRASE_ID, names_to = "YEAR", values_to = "Soy_Def_NetGHG_CO2eq")
NetGhg.longer <- as.data.frame(di.longer)

## add IBGE data
soy_soyGhg_IBGE <- merge(soy_prod, GrossGhg.longer,  by=c("TRASE_ID", "YEAR"),  all.x = FALSE, all.y = TRUE)
soy_soyNetGhg_IBGE <- merge(soy_soyGhg_IBGE, NetGhg.longer,  by=c("TRASE_ID", "YEAR"),  all.x = FALSE, all.y = TRUE)
soy_soy2Ghg_IBGESong <- merge(soy_soyNetGhg_IBGE, soyarea_songS.longer,  by=c("TRASE_ID", "YEAR"),  all.x = TRUE, all.y = TRUE)

##Here starts Florians script

## normalize soy deforestation data with IBGE soy (harvested) area data
soy_soyGhg_IBGE <- soy_soy2Ghg_IBGESong %>% mutate(
  SOY_GrossDEF_NORM = (HA / Soy_AreaSong_HA) * Soy_Def_GrossGHG_CO2eq,
  SOY_NetDEF_NORM = (HA / Soy_AreaSong_HA) * Soy_Def_NetGHG_CO2eq
)

soy_soyGhg_IBGE <- soy_soyGhg_IBGE %>% replace_na(list(SOY_GrossDEF_NORM = 0))
soy_soyGhg_IBGE <- soy_soyGhg_IBGE %>% replace_na(list(SOY_NetDEF_NORM = 0))

soy_soyGhg_IBGE <- soy_soyGhg_IBGE %>% replace_na(list(TN = 0))

##Add column with soy deforestation emissions 
soy_soyGhg_IBGE$SOY_GrossDEF_NORM_perSoyTon <- soy_soyGhg_IBGE$SOY_GrossDEF_NORM/soy_soyGhg_IBGE$TN
soy_soyGhg_IBGE$SOY_NetDEF_NORM_perSoyTon <- soy_soyGhg_IBGE$SOY_NetDEF_NORM/soy_soyGhg_IBGE$TN

soy_soyGhg_IBGE <- soy_soyGhg_IBGE %>% replace_na(list(SOY_GrossDEF_NORM_perSoyTon = 0))
soy_soyGhg_IBGE <- soy_soyGhg_IBGE %>% replace_na(list(SOY_NetDEF_NORM_perSoyTon = 0))

head(soy_soyGhg_IBGE)

##Clean to avoid confusion of others using data
soy_Ghg_FINAL_Clean  <- soy_soyGhg_IBGE[,c('TRASE_ID','YEAR','SOY_GrossDEF_NORM','SOY_NetDEF_NORM','SOY_GrossDEF_NORM_perSoyTon','SOY_NetDEF_NORM_perSoyTon')]

##Re-name to clarify units
names(soy_Ghg_FINAL_Clean )[names(soy_Ghg_FINAL_Clean ) == "SOY_GrossDEF_NORM"] <- "SOY_GrossDefGHG_NORM_tCO2eq"
names(soy_Ghg_FINAL_Clean )[names(soy_Ghg_FINAL_Clean ) == "SOY_NetDEF_NORM"] <- "SOY_NetDefGHG_NORM_tCO2eq"
names(soy_Ghg_FINAL_Clean )[names(soy_Ghg_FINAL_Clean ) == "SOY_GrossDEF_NORM_perSoyTon"] <- "SOY_GrossDefGHG_NORM_tCO2eq_perSoyTon"
names(soy_Ghg_FINAL_Clean )[names(soy_Ghg_FINAL_Clean ) == "SOY_NetDEF_NORM_perSoyTon"] <- "SOY_NetDefGHG_NORM_tCO2eq_perSoyTon"

##Delete X in column YEAR
soy_Ghg_FINAL_Clean$YEAR <- gsub("X","",as.character(soy_Ghg_FINAL_Clean$YEAR))

head(soy_Ghg_FINAL_Clean)

# save to S3
s3write_using(soy_Ghg_FINAL_Clean,
              write.table,
              sep = ";",
             object = "brazil/soy/indicators/out/q4_2023/GHG_soy_deforestation_2013_2022_q4_2023_17June.csv",
             opts = c("check_region" = TRUE),
             bucket = "trase-storage"
)

##Adding Territorial Deforestation
##Merge all years together of C Sequestration
Cseq4Yrs <-  merge(CseqTD_19and20S, CseqTD_21and22S, by=c("TRASE_ID"),  all.x = TRUE, all.y = TRUE)
Cseq6Yrs <-  merge(CseqTD_17and18S, Cseq4Yrs,  by=c("TRASE_ID"),  all.x = TRUE, all.y = TRUE)
Cseq8Yrs <-  merge(CseqTD_15and16S, Cseq6Yrs,  by=c("TRASE_ID"),  all.x = TRUE, all.y = TRUE)
Cseq10Yrs <- merge(CseqTD_13and14S, Cseq8Yrs,  by=c("TRASE_ID"),  all.x = TRUE, all.y = TRUE)
Cseq12Yrs <- merge(CseqTD_11and12S, Cseq10Yrs,  by=c("TRASE_ID"),  all.x = TRUE, all.y = TRUE)
Cseq14Yrs <- merge(CseqTD_09and10S, Cseq12Yrs,  by=c("TRASE_ID"),  all.x = TRUE, all.y = TRUE)
Cseq16Yrs <- merge(CseqTD_07and08S, Cseq14Yrs,  by=c("TRASE_ID"),  all.x = TRUE, all.y = TRUE)
Cseq18Yrs <- merge(CseqTD_05and06S, Cseq16Yrs,  by=c("TRASE_ID"),  all.x = TRUE, all.y = TRUE)
Cseq20Yrs <- merge(CseqTD_03and04S, Cseq18Yrs,  by=c("TRASE_ID"),  all.x = TRUE, all.y = TRUE)

TerDefGrossGhgSMerged <- merge(TerDefGrossGhgS, Cseq20Yrs,  by=c("TRASE_ID"),  all.x = TRUE, all.y = TRUE)

##Pivot-longer the Gross Ghg data 
GROSS.longer <- TerDefGrossGhgS %>%      #######CHANGE HERE
  pivot_longer(!TRASE_ID, names_to = "YEAR", values_to = "Ter_Def_GrossGHG_tCO2eq")
GROSSforFinallonger <- as.data.frame(GROSS.longer)
head(GROSSforFinallonger)

##Pivot-longer the Net Ghg data 
Cseq.longer <- Cseq20Yrs %>%      #######CHANGE HERE
  pivot_longer(!TRASE_ID, names_to = "YEAR", values_to = "Ter_Def_CseqGHG_tCO2eq")
Cseq.longer2 <- as.data.frame(Cseq.longer)
head(Cseq.longer2)

##Delete X in column YEAR to be able to merge later
Cseq.longer2$YEAR <- gsub("X","",as.character(Cseq.longer2$YEAR ))

GROSSforFinallonger$YEAR <- gsub("XG","",as.character(GROSSforFinallonger$YEAR))

CSeqandGROSS <- merge(GROSSforFinallonger, Cseq.longer2,  by=c("TRASE_ID", "YEAR"),  all.x = TRUE, all.y = TRUE)
CSeqandGROSS2 <- merge(CSeqandGROSS, BSs,  by.x=c("TRASE_ID"), by.y = ("TRASE_GEOCODE"),  all.x = TRUE, all.y = TRUE)
head(CSeqandGROSS)

##Estimate Net emissions by subtracting CSequ from Gross emissions
CSeqandGROSS$Ter_Def_NetGHG_tCO2eq <- CSeqandGROSS$Ter_Def_GrossGHG_tCO2eq - CSeqandGROSS$Ter_Def_CseqGHG_tCO2eq
head(CSeqandGROSS)

##Convert negative net Gross emissions to zero 
CSeqandGROSS[CSeqandGROSS$Ter_Def_NetGHG_tCO2eq < 0, "Ter_Def_NetGHG_tCO2eq"] <- 0

NetandGROSS_Clean  <- CSeqandGROSS[,c('TRASE_ID','YEAR','Ter_Def_GrossGHG_tCO2eq')] ##,'Ter_Def_NetGHG_tCO2eq'
head(NetandGROSS_Clean)

# save to S3
s3write_using(NetandGROSS_Clean,
              write.table,
              sep = ";",
              object = "brazil/soy/indicators/out/q4_2023/GHG_Gross_territorial_deforestation_2003_2022_q4_2023.csv",
              opts = c("check_region" = TRUE),
              bucket = "trase-storage"
)




## Quality Check
## Select only years I need

soy_soy2Ghg_IBGESong2$SoyAreaDiff_Percent <- ((soy_soy2Ghg_IBGESong5$HA_Sums-soy_soy2Ghg_IBGESong5$Soy_AreaSong_HA_Sums)/soy_soy2Ghg_IBGESong5$HA_Sums)*100

mausP <- soy_soy2Ghg_IBGESong2%>% #ASMerger2 ASs
  dplyr::group_by(YEAR) %>% # 
  dplyr::summarise(HA_Sums = (sum(HA, na.rm=TRUE)),
                   Soy_AreaSong_HA_Sums = (sum(Soy_AreaSong_HA, na.rm=TRUE) )) ##
soy_soy2Ghg_IBGESong3 <- as.data.frame(mausP)
soy_soy2Ghg_IBGESong3

soy_soy2Ghg_IBGESong3$SoyAreaDiff_Percent <- ((soy_soy2Ghg_IBGESong3$HA_Sums-soy_soy2Ghg_IBGESong3$Soy_AreaSong_HA_Sums)/soy_soy2Ghg_IBGESong3$HA_Sums)*100

soy_soyGhg_IBGE$GHG_Diff_Percent <- ((soy_soyGhg_IBGE$SOY_GrossDEF_NORM - soy_soyGhg_IBGE$Soy_Def_GrossGHG_CO2eq)/soy_soyGhg_IBGE$Soy_Def_GrossGHG_CO2eq)*100

mausP <- soy_soyGhg_IBGE %>% #ASMerger2 ASs
  dplyr::group_by(YEAR) %>% # 
  dplyr::summarise(HA_Sums = (sum(soy_soyGhg_IBGE$SOY_GrossDEF_NORM, na.rm=TRUE)),
                   Soy_AreaSong_HA_Sums = (sum(soy_soyGhg_IBGE$Soy_Def_GrossGHG_CO2eq, na.rm=TRUE) )) ##
soy_soy2Ghg_IBGESong3 <- as.data.frame(mausP)
soy_soy2Ghg_IBGESong3

b <- ggplot(soy_soyGhg_IBGE, aes(x = YEAR, y =GHG_Diff_Percent )) + #
  geom_bar (position="stack", stat="identity",color="grey" )+ #,width=0.7, fill="steel blue", color="white"+ , color="black"
  # scale_fill_manual(values=getPalette(colourCount))+
  theme_minimal()+
  theme(legend.position="top") +
  ylab("Soy GHG Difference in Percent")  
b  

plot(soy_soyGhg_IBGE$SOY_GrossDEF_NORM~soy_soyGhg_IBGE$Soy_Def_GrossGHG_CO2eq)
abline(lm(HA~Soy_AreaSong_HA, data = soy_soy2Ghg_IBGESong2), col = "blue")
### ENd of Quality Check

##Plotting everything
#load provinces shape file here and project to same crs
br_mun <- read_sf("https://cdn.jsdelivr.net/npm/@trase/trase-atlas@1/files/brazil.json") %>% 
  mutate(trase_id = paste0("BR-", id)) 
plot(br_mun[3])

names(soy_soyGhg_IBGE)[names(soy_soyGhg_IBGE) == "TRASE_ID"] <- "trase_id"
#soy_soyGhg_IBGE13 <-soy_soyGhg_IBGE[soy_soyGhg_IBGE$YEAR=="X2013",] ## Select only years I need
soy_soyGhg_IBGE20 <-soy_soyGhg_IBGE[soy_soyGhg_IBGE$YEAR=="X2020",] ## Select only years I need
soy_soyGhg_IBGE21 <-soy_soyGhg_IBGE[soy_soyGhg_IBGE$YEAR=="X2021",] ## Select only years I need

#mean(soy_soyGhg_IBGE13$Soy_Def_GrossGHG_CO2eq)

DFd <-merge(br_mun,soy_soyGhg_IBGE20, by=c("trase_id"))
plot(DFb["Soy_Def_GrossGHG_CO2eq"])

head(DFb)

DFd[is.na(DFd)] <- 0 # Replace NA with 0 to be able to sum-up

DFd$GrossRelDiff20 <- (DFd$SOY_NetDEF_NORM - DFd$Soy_Def_GrossGHG_CO2eq )/DFd$SOY_NetDEF_NORM


##This works
c <- ggplot(DFd) + 
  geom_sf(aes(fill =GrossRelDiff20), color="transparent") + ##
  scale_fill_stepsn(colors = RColorBrewer::brewer.pal(7, "RdYlGn"),
                    breaks = c(-4, -2,  0,  0.5,     1), 
                    labels = c("-4 to -2" , "-2 to 0", "Zero", "0-0.5","0.5-1"),    
                    limits=c(-17,1),
                    values = scales::rescale(c(-4, -2,  0,  0.5,     1)))
c+ coord_sf(xlim = c(-73, -35), ylim = c(-32, 5))

##Absolute GRoss Def Emissions
b <- ggplot(DFc) + 
  geom_sf(aes(fill =Soy_Def_GrossGHG_CO2eq), color="transparent") + ##
  scale_fill_stepsn(colors = RColorBrewer::brewer.pal(5, "YlOrRd"),
                    breaks = c(0,       5000,      50000,       500000,        5000000), 
                    labels = c("Zero" , "0-5000", "5000-50000", "50000-500000","500000+"),    
                    limits=c(0,6000000),
                    values = scales::rescale(c(0,5000, 50000,500000,5000000)))
b+ coord_sf(xlim = c(-73, -35), ylim = c(-32, 5))

b <- ggplot(DFc) + 
  geom_sf(aes(fill =SOY_NetDEF_NORM), color="transparent") + ##
  scale_fill_stepsn(colors = RColorBrewer::brewer.pal(5, "YlOrRd"),
                    breaks = c(0,       5000,      50000,       500000,        5000000), 
                    labels = c("Zero" , "0-5000", "5000-50000", "50000-500000","500000+"),    
                    limits=c(0,6000000),
                    values = scales::rescale(c(0,5000, 50000,500000,5000000)))
b+ coord_sf(xlim = c(-73, -35), ylim = c(-32, 5))

##
soy_soyGhg_IBGE2 <- soy_soyGhg_IBGE[!soy_soyGhg_IBGE$Soy_Def_GrossGHG_CO2eq == 0, ]
soy_soyGhg_IBGE2 <- soy_soyGhg_IBGE[!soy_soyGhg_IBGE$SOY_GrossDEF_NORM == 0, ]
 
a <- ggplot(soy_soyGhg_IBGE2, aes(x=YEAR, y=Soy_Def_GrossGHG_CO2eq)) + #x=reorder(STATE_ID, +Ranking),
  geom_boxplot() +
    ylim(0,10000)  +
   #ylim(0,8000000)  +
 # coord_cartesian(ylim=c(0,10000))  +
  theme_minimal()+ #white background few grey lines
  theme(legend.position="top") +
  labs(y=expression(paste("Deforestation emissions (t ", CO^2, "-eq. )")))
a 

b <- ggplot(soy_soyGhg_IBGE2, aes(x=YEAR, y=SOY_GrossDEF_NORM)) + #x=reorder(STATE_ID, +Ranking),
  geom_boxplot() +
  ylim(0,10000)  +
  #ylim(0,8000000)  +
  # coord_cartesian(ylim=c(0,10000))  +
  theme_minimal()+ #white background few grey lines
  theme(legend.position="top") +
  labs(y=expression(paste("Normalised Deforestation emissions (t ", CO^2, "-eq. )")))
b

soy_soyGhg_IBGE13 <-soy_soyGhg_IBGE2[soy_soyGhg_IBGE2$YEAR=="X2013",] ## Select only years I need

soy_soyGhg_IBGE13 <- soy_soyGhg_IBGE13 %>% replace_na(list(SOY_GrossDEF_NORM = 0))
soy_soyGhg_IBGE13 <- soy_soyGhg_IBGE13 %>% replace_na(list(SOY_NetDEF_NORM = 0))

BS <-read.delim2("BRAZIL_SOY_2.6.0_pc.csv", header=TRUE, sep = "," , dec=".") #
BSs <- BS[,c('TRASE_GEOCODE','MUNICIPALITY.OF.PRODUCTION', 'STATE')]
BSsU <- unique(BSs)

BSMerger0b <- merge(soy_soyGhg_IBGE13, BSsU,  by.x=c("TRASE_ID"), by.y=c("TRASE_GEOCODE"), all.x = TRUE, all.y = FALSE)

write.csv(BSMerger0b, "C:\\Users\\CarinaMueller\\OneDrive - SEI\\RStudio_from_PhD\\BR_Soy\\soy_soyGhg_IBGE22JanB.csv", row.names=FALSE)


boxplot.stats(BSMerger0b$Soy_Def_GrossGHG_CO2eq)
boxplot.stats(BSMerger0b$SOY_GrossDEF_NORM)

boxplot(YEAR~Soy_Def_GrossGHG_CO2eq,data=soy_soyGhg_IBGE2)


