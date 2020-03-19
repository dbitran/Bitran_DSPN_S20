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

##HW 5

install.packages("lme4")
library(lme4)
library(tidyverse)
library(boot)



cbpp %>%
  ggplot( aes(x = size, y = incidence)) +  geom_point()
  

model1 <- lm(incidence ~ size, data=cbpp)
summary(model1)

lmer1 <- lmer(incidence ~ size +  (size | herd), data=cbpp) 

setwd("/Users/Debbie/Documents/DOCUMENTS/CLASSES/DSPAN/GitHub/Bitran_DSPN_S20/Homeworks")
hcp.d <- read_csv("unrestricted_trimmed_1_7_2020_10_50_44.csv")

hcp.d <- hcp.d %>%
  select(Subject, Gender, Age, FS_IntraCranial_Vol, FS_Total_GM_Vol) %>%
  na.omit()%>%
  mutate(ZFS_IntraCranial_Vol = (FS_IntraCranial_Vol - mean(FS_IntraCranial_Vol))/sd(FS_IntraCranial_Vol),
         ZFS_Total_GM_Vol = (FS_Total_GM_Vol - mean(FS_Total_GM_Vol))/sd(FS_Total_GM_Vol),
         Gender = as.factor(Gender))%>%
  print()

log.m <- glm(Gender ~ FS_IntraCranial_Vol, data=hcp.d, family=binomial)
summary(log.m)

cv.log  <- cv.glm(hcp.d, log.m)
summary(cv.log)
cv.log  <- cv.glm(hcp.d, log.m, K=nrow(hcp.d))
summary(cv.log)

contrasts(hcp.d$Gender)
log.prob.d = data.frame(predict(log.m, type="response"))
colnames(log.prob.d) = c("predicted_prob")
log.prob.d<- log.prob.d%>%
  mutate(pred_gender = if_else(predicted_prob>.5, "M", "F"))
confusion.d <- data.frame(log.prob.d$pred_gender, hcp.d$Gender)
colnames(confusion.d) <- c("predicted", "actual")
table(confusion.d)
                         

library(MASS)
lda.m <- lda(Gender ~ FS_IntraCranial_Vol, data=hcp.d)
lda.m
plot(lda.m)

lda.pred = predict(lda.m)
lda.gender = lda.pred$class
table(lda.gender, hcp.d$Gender)
mean(lda.gender==hcp.d$Gender)

