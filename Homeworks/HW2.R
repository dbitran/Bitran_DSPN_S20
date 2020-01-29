library(tidyverse)
setwd("/Users/Debbie/Documents/DOCUMENTS/CLASSES/DSPAN/GitHub/Bitran_DSPN_S20")
hcpdata <- read_csv("unrestricted_trimmed_1_7_2020_10_50_44.csv")

d1 <- hcpdata %>% 
      filter(Age == "22-25") %>%
      select(Subject, Gender, Age, FS_Total_GM_Vol, FS_Tot_WM_Vol)%>%
      na.omit()%>%
      print()

dlong_ <- d1 %>% 
          gather(BrainMeasures, Volume, 4:5)
view(dlong_)
