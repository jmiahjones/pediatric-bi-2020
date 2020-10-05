
rm(list=ls())

library(caret)
library(readxl)
library(dplyr)
main_data_path <- "./main_analysis_data.csv"
model_out <- "./results/web_glm.RData"

raw <- read.csv(main_data_path,
                header=T)
# raw = raw[-1,]
colnames(raw)[1] <- "Record.ID"


raw <- raw %>% mutate(BI=ifelse(is.na(True.Bacterial.Infection), 
                                0, 
                                True.Bacterial.Infection))

#==========================================================================================#
#============================ step 1: data processing =====================================#
#==========================================================================================#
##Clean data set
library(dplyr)
features.raw <- raw %>%
  mutate(sex= factor(Sex, labels = c("male", "female")),
         insurance = factor(Insurance.Type, labels = c("private", "public", "none")),
         cm_condition = factor(Presence.of.Chronic.Medical.Condition..CMC., labels = c("no", "yes")),
         age = as.numeric(Age),
         gestationage = as.numeric(Gestational.Age),
         appearance = factor(Ill.Appearing, labels = c("well", "ill","unknown")),
         maxtemp = as.numeric(Maximum.Temperature),
         numdaysill = as.numeric(Number.of.Days.of.Illness),
         cough = factor(Cough.Present.During.Illness., labels = c("no", "yes","unknown" )),
         puti = factor(Positive.Urinary.Tract.Inflammation, labels = c("no", "yes", "unknown")),
         BI = factor(BI, labels = c("no", "yes"))
  )

features_nosocial = features.raw %>% select(Record.ID, sex, insurance, cm_condition, age, gestationage, 
                                            appearance, maxtemp, numdaysill, cough, puti,
                                            BI)


dim(features_nosocial)

##Explore data set
missingFeatures_nonsocial <- as.data.frame(apply(features_nosocial, 2, function(x) sum(is.na(x))))
# no missingness

data.features_nosocial <- data.frame(model.matrix(BI ~., data = features_nosocial)[,-1],
                                     BI=features_nosocial$BI)

data.use_nosocial <- data.features_nosocial[-1]

data.use_nosocial$BI<-as.numeric(data.use_nosocial$BI)-1

Y_nosocial <-as.numeric(data.use_nosocial$BI)
X_nosocial<-data.use_nosocial[,!names(data.use_nosocial)%in%c("BI")]


web_glm <- glm(Y_nosocial ~ ., data=X_nosocial, family=binomial())

save(web_glm, file=model_out)

## example of prediction
cut.point = 0.01
factor(predict(web_glm, X_nosocial[1,], type="response") > cut.point,
       levels = c(T, F),
       labels = c("BI+", "BI-"))

sexfemale <- c(1);
insurancepublic <- c(0);
insurancenone <- c(1);
cm_conditionyes <- c(1);
age <- c(5);
gestationage <- c(21);
appearanceill <- c(1);
appearanceunknown <- c(0);
maxtemp <- c(39.1);
numdaysill <- c(1);
coughyes <- c(0);
coughunknown <- c(0);
putiyes <- c(1);
putiunknown <- c(0);
joshtest <- data.frame(sexfemale,insurancepublic,insurancenone,cm_conditionyes,age,gestationage,appearanceill,appearanceunknown,maxtemp,numdaysill,coughyes,coughunknown,putiyes,putiunknown);
a <- unname(predict(web_glm, joshtest, type="response"))

