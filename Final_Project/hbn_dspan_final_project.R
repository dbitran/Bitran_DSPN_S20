#Loading libraries
library(easypackages)
libraries("tidyverse","skimr", "naniar", "stringi", "psych")

#Setting wd
setwd("/Users/Debbie/Documents/Pitt/Classes/DSPAN/HBN_DSPAN")
#Loading HBN data file and Hippo volumes data file



#load data
# set  periods & blanks as NaNs
hbn.d = read_csv("hbn_data_dspan.csv", na = c("", ".")) 
#cleansing data
hbn.d = hbn.d %>%
  #rename identifiers to sub_id 
  rename(sub_id = Identifiers)%>% 
  #remove 2nd row 
  subset(sub_id != ",assessment")%>%
  #remove ,assessment from end of sub_id 
  mutate(sub_id = str_remove(sub_id, ",assessment"))%>%
  #rename all MRI_Track variables: MRI_Track,* = MRI_Track_
  select_all(~gsub("MRI_Track,*", "MRI_Track_",.))%>%
  #rename all Basic_Demos variables: Basic_Demos,* = Basic_Demos_
  select_all(~gsub("Basic_Demos,*", "Basic_Demos_",.))%>%
  # ,Administration
  # ,Comment_ID
  # ,Data_entry
  # ,Days_Baseline
  # ,EID
  # ,PSCID	
  # ,START_DATE
  # ,Season
  # ,Site
  # ,Study
  # ,Visit_label
  # ,Year
  select(-ends_with(",Administration"), -ends_with(",Comment_ID"), 
           -ends_with(",Data_entry"),  -ends_with(",Days_Baseline"),
         -ends_with(",EID"), -ends_with(",PSCID"),-ends_with(",Year"), 
         -ends_with(",Patient_ID"),-ends_with(",START_DATE"),
         -ends_with(",Season"), -ends_with(",Site"),-ends_with(",Study"), 
         -ends_with(",Visit_Label"),-ends_with(",Days_Baseline"))%>%
  #remove everything before comma in colnames
  select_all(~gsub(".*,", "",.))%>%
  select_all(tolower)%>% 
  rename(age = basic_demos_age, sex = basic_demos_sex)%>%
  rename(fsq_healthins = fsq_01, fsq_p1_employed = fsq_02, fsq_p2_employed = fsq_03, fsq_income = fsq_04, 
           fsq_ssi = fsq_05a, fsq_ssd = fsq_05b, fsq_ssretire = fsq_05c, fsq_sssurvivor = fsq_05d, 
         fsq_civilpension = fsq_05e, fsq_sbp = fsq_05f, fsq_meddisability = fsq_05g,fsq_wic = fsq_05h, 
         fsq_snap = fsq_05i, fsq_nyccash = fsq_05j,
           fsq_housetenure = fsq_06, fsq_housetype = fsq_07, fsq_houseppl= fsq_08)%>%
         rename(cares_05 = ocare1, 
         cares_510 = ocare2,
         cares_1015 = ocare3,
         moves_05 = omoves1, 
         moves_510 = omoves2,
         moves_1015 = omoves3)%>%
  mutate(waiswasi_fsqi = ifelse(is.na(wasi_fsiq), wais_fsiq_scale, wasi_fsiq),
         iq = ifelse(!is.na(wisc_fsiq), wisc_fsiq,waiswasi_fsqi))%>%
  select(sub_id, mri_track_eid, iq, age, sex, child_ethnicity, child_race, 
         child_race_other, barratt_p1_edu:financialsupport, 
         cares_05:moves_1015,
         fsq_healthins:fsq_houseppl,
         apq_p_cp:apq_sr_total,
         cpic_content_total:cpic_triangulation_total,
         psi_dc:psi_total_t,
         sdq_conduct_problems:sdq_prosocial, 
         cbcl_ab:cbclpre_wd_t,
         ysr_ab:ysr_wd_t,
         asr_ab:asr_wd_t)%>%
  replace_with_na(replace = list(fsq_income = 12))%>%
  filter(age >= 10)%>%
  filter(iq>=70)%>% 
  mutate(sex = as.factor(sex))%>%
  merge(hbnhippo.d,by="sub_id")

hbncor.d = hbn.d %>%
  select(age, sex, apq_p_cp, apq_p_id, fsq_income, psi_pd_t, 
         cpic_frequency_total, cpic_intensity_total, cpic_perceived_threat_total, cpic_resolution_total, 
         sdq_conduct_problems,sdq_externalizing, sdq_hyperactivity, cbcl_ap_t, cbcl_rbb_t, cbcl_ext_t, 
           ysr_ap_t, ysr_rbb_t, ysr_ext_t)

skim(hbncor.d)

library(Hmisc)
library(psych)
rcorr(as.matrix(hbncor.d))

rcorr(hbncor.d)


corPlot(hbncor.d)
cor.plot(hbncor.d)
cor.ci(hbncor.d)
hbn.d %>%
  cor.ci(APQ_P_ID, APQ_SR_ID)

ggplot(data = hbn.d, aes(x=age))+ geom_freqpoly(color = sex)

hbn


#ages 5-10 
#05 

#ages 10-15
#05 & 5-10
#ages 15 and older
#05 & 5-10 & 1015

x <- c(2,5,3,9,8,11,6)
count <- 0
for (val in x) {
  if(val %% 2 == 0)  count = count+1
}
print(count)

#make sure MRI_Track_EID = sub_id

#loading hippo volume data
hbnhippo.d = read_csv("hippo_vols.csv")%>%
  separate(site_wave, into = c("site", "wave"), sep = "_")%>%
mutate(sub_id = str_remove(sub_id, "sub-"))%>%
  view()




#reset variable classes/structures 
