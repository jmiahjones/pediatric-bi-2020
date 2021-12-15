
args <- commandArgs(trailingOnly = T)
if(length(args)==0){
  message("No arguments given. Training UTI only...")
  utiB <- T
} else{
  stopifnot(length(args) == 1)
  utiB <- as.logical(args)
}


library(dplyr)
library(caret)
library(foreach)
library(SuperLearner)
library(readxl)
library(doParallel)#Load parallel library

main_data_path <- "./derivation/derivation-12-6-21.csv"

# output files
trained_file <- if(utiB){
  "./derivation/results/revised_trained_uti.RData"
} else {
  "./derivation/results/revised_trained_le.RData"
}

group_testind <- function(groups, all_idxs, k=10){
  trfolds <- caret::groupKFold(groups, k=k)
  lapply(trfolds, function(tr) setdiff(all_idxs, tr))
}


raw <- read.csv(main_data_path,
                header=T)
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
         # insurance = factor(Insurance.Type, labels = c("private", "public", "none")),
         cm_condition = factor(Presence.of.Chronic.Medical.Condition..CMC., labels = c("no", "yes")),
         age = as.numeric(Age),
         gestationage = as.numeric(Gestational.Age),
         appearance = factor(Ill.Appearing, labels = c("well", "ill","unknown")),
         maxtemp = as.numeric(Maximum.Temperature),
         numdaysill = as.numeric(Number.of.Days.of.Illness),
         cough = factor(Cough.Present.During.Illness., labels = c("yes", "no","unknown" )),
         puti = factor(Positive.Urinary.Tract.Inflammation, labels = c("no", "yes", "unknown")),
         le = factor(Leukocyte.Esterase.Present, labels = c("no", "yes", "unknown")),
         BI = factor(BI, labels = c("no", "yes"))
  )

features_nosocial = features.raw %>% 
  select(Record.ID, sex, 
         # insurance, 
         cm_condition, age, gestationage, 
         appearance, maxtemp, numdaysill, cough, 
         puti, le,
         BI)

features_nosocial <- if(utiB){
  features_nosocial %>% select(!one_of("le"))
} else {
  features_nosocial %>% select(!one_of("puti"))
}

dim(features_nosocial)

##Explore data set
missingFeatures_nonsocial <- as.data.frame(apply(features_nosocial, 2, function(x) sum(is.na(x))))
# no missingness

data.features_nosocial <- data.frame(model.matrix(BI ~., data = features_nosocial)[,-1],
                                     BI=features_nosocial$BI)
record_ids <- data.features_nosocial[,1]

data.use_nosocial <- data.features_nosocial[-1]

data.use_nosocial$BI<-as.numeric(data.use_nosocial$BI)-1



Y_nosocial <-as.numeric(data.use_nosocial$BI)
X_nosocial<-data.use_nosocial[,!names(data.use_nosocial)%in%c("BI")]

# learner=create.Learner("SL.randomForest", tune = list(mtry = seq(6,29,1),ntrees=c(500,1000)))	

rflrn=create.Learner("SL.randomForest", 
                     tune = list(mtry = seq(6,13,2),
                                 ntrees=c(1000)))

SL.lib <- c(
  "SL.earth",
  "SL.gam",
  "SL.glm",
  "SL.glmnet",
  rflrn$names
)

sl_nosocial = SuperLearner(
  Y = Y_nosocial,
  X = X_nosocial,
  family = binomial(),
  method = "method.AUC",
  SL.library = SL.lib
)

glm_nosocial = SuperLearner(
  Y = Y_nosocial,
  X = X_nosocial,
  family = binomial(),
  method = "method.AUC",
  SL.library = c("SL.glm")
)

full_data_pred_df <- data.frame(Y=Y_nosocial,
                                SL=sl_nosocial$SL.predict,
                                GLM=glm_nosocial$SL.predict)

save.image(file=trained_file)
