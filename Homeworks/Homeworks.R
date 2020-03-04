#Homework 4

library(tidyverse)
library(MASS)
library(ISLR)
setwd("/Users/Debbie/Documents/DOCUMENTS/CLASSES/DSPAN/GitHub/Bitran_DSPN_S20/Homeworks")
hcp.d <- read_csv("unrestricted_trimmed_1_7_2020_10_50_44.csv")
hcp.d <- select(hcp.d, Subject, Gender, Flanker_Unadj, FS_Tot_WM_Vol, FS_Total_GM_Vol)

model1 <- lm(Flanker_Unadj ~ FS_Total_GM_Vol, data=hcp.d)
cbind(coef(model1), confint(model1))

hcp.d %>% 
  ggplot( aes(x = FS_Total_GM_Vol, y = Flanker_Unadj)) +
  geom_smooth(method = "lm", se = TRUE, color = "green") +  # Plot regression slope
  geom_segment(aes(xend = FS_Total_GM_Vol, yend = predict(model1)), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = predicted), shape = 1)

GMVol_input <- data.frame(FS_Total_GM_Vol=c(612432, 702562, 596433, 695982, 649230))
predicted_table <- data.frame(predict(model1, GMVol_input, interval = 'confidence'))
predicted_table <- cbind(GMVol_input,predicted_table)
colnames(predicted_table) = c('FS_Total_GM_Vol','Flanker_Unadj_pred', 'lower_CI_bound', 'upper_CI_bound')
print(predicted_table)

model2 <- lm(Flanker_Unadj ~ Gender + FS_Tot_WM_Vol + FS_Total_GM_Vol, data=hcp.d)
summary(model2)
cbind(coef(model2), confint(model2))
model2_update <- update(model2, ~.-Gender -FS_Tot_WM_Vol)
summary(model2_update)

hcp.d$Gender <- as.factor(hcp.d$Gender)
contrasts(hcp.d$Gender)
