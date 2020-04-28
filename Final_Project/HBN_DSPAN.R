library(tidyverse)
library(skimr)
library(naniar)

setwd("/Users/Debbie/Documents/DOCUMENTS/CLASSES/DSPAN/HBN_DSPAN")
hbn.d = read_csv("HBN_DATA_EXPLORATORY.csv")
apq.d = read_csv("APQ_ONLY.csv")
cpic.d = read_csv("HBN_CPIC.csv")
#preintdemosfam.d = read_csv("HBN_PreInt_Demos_Fam.csv")
hbnhippo.d = read_csv("hippo_vols.csv")%>%
  separate(site_wave, into = c("site", "wave"), sep = "_")
hbn.d = hbn.d %>% 
  merge(hbnhippo.d,by="id")%>% 
  merge(apq.d,by="id") #%>%
  #merge(cpic.d,by="id") #%>%
  #merge(preintdemosfam.d,by="id")
#Selecting variables of interest. Removing irrelevant variables#
  hbn.d = hbn.d %>% 
  select(id, Age, Sex, site:hippo_vol_rh, #Child_CountryOrigin:SocialService, 
         Barratt_P1_Edu:Barratt_financialsupport, 
       FSQ_01:FSQ_08, #PSI_DC:PSI_Total_T, 
       APQ_P_CP:APQ_P_Total, APQ_SR_CP:APQ_SR_Total, 
       CBCL_AB:CBCL_WD_T, YSR_AB:YSR_WD_T, SDQ_Conduct_Problems:SDQ_Prosocial)%>%
    filter(Age <= 18)%>%
    filter(Age >= 7)%>%
    replace_with_na(replace = list(FSQ_04 = 12))
  
  attach(hbn.d)
  summary(lm(CBCL_Ext_T ~ Age + Sex + FSQ_04 + whole_brain_vol + APQ_P_ID + hippo_vol_lh))
  summary(lm(CBCL_Ext_T ~ Age + Sex + FSQ_04 + whole_brain_vol + APQ_P_ID*hippo_vol_rh))
  summary(lm(CBCL_Ext_T ~ Age + Sex + FSQ_04 + APQ_P_ID))
  summary(lm(CBCL_Ext_T ~ Age + Sex + FSQ_04 + whole_brain_vol + hippo_vol_rh))
  summary(lm(CBCL_Ext_T ~ Age + Sex + FSQ_04 + whole_brain_vol + hippo_vol_lh))
  
  %>%
    replace_with_na(replace = list(Child_Ethnicity = 2 | 3)
  
#Recoding "declines to answer" as missing#
skim(hbn.d)
