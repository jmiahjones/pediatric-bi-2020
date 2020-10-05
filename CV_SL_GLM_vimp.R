###################################
# Variable Importance
###################################

library(caret)
library(foreach)
library(SuperLearner)
library(readxl)
library(dplyr)

main_data_path <- "./main_analysis_data.csv"
vimp_data_file <- "./results/vimp_main.RData"
glm_result_file <- "./results/vimp_glm.csv"
sl_result_file <- "./results/vimp_sl.csv"

########################
### Re-run Data Creation
########################

raw <- read.csv(main_data_path, header=T)
colnames(raw)[1] <- "Record.ID"


raw <- raw %>% mutate(BI=ifelse(is.na(True.Bacterial.Infection), 
                                0, 
                                True.Bacterial.Infection))
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

features_nosocial = features.raw %>% select(Record.ID, sex, insurance, cm_consition, age, gestationage, 
                                            appearance, maxtemp, numdaysill, cough, puti,
                                            BI)

data.features_nosocial <- data.frame(model.matrix(BI ~., data = features_nosocial)[,-1],
                                     BI=features_nosocial$BI)
record_ids <- data.features_nosocial[,1]

data.use_nosocial <- data.features_nosocial[-1]

data.use_nosocial$BI<-as.numeric(data.use_nosocial$BI)-1



Y_nosocial <-as.numeric(data.use_nosocial$BI)
X_nosocial<-data.use_nosocial[,!names(data.use_nosocial)%in%c("BI")]


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


######################################
############ VIMP ####################
######################################
library(SuperLearner)
library(vimp)

final_glm <- glm(Y_nosocial ~ ., data=X_nosocial, family=binomial())

glm_summ <- summary(final_glm)

glm_coef <- glm_summ$coefficients[,1] %>% round(3)
glm_p <- glm_summ$coefficients[,4]
glm_ci <- confint(final_glm)
glm_ci <- round(glm_ci, 3)
glm_ci[is.na(glm_ci[,1]),1] <- -Inf
glm_ci[is.na(glm_ci[,2]),2] <- Inf
glm_pretty_ci <- paste0("(", apply(glm_ci, 1, paste, collapse=", "), ")")
glm_pretty <- paste0(glm_coef, " ", glm_pretty_ci)

glm_vimp_tab <- tibble(variable=names(glm_coef), importance=glm_pretty)
write.csv(glm_vimp_tab, glm_result_file)

set.seed(70889)

# some of the features in X come from the same variable
# the vector below maps the independent variables
col_grps <- c(1, 2,2, 3, 4, 5, 6,6, 7, 8, 9,9, 10,10)

vimp_idxs <- lapply(1:max(col_grps), function(i) which(col_grps == i))
names(vimp_idxs) <- setdiff(colnames(features_nosocial), c("Record.ID", "BI"))

vimps <- foreach(idx=vimp_idxs) %do% 
  vimp_auc(Y_nosocial, X_nosocial, indx=idx,
           SL.library=SL.lib,
           run_regression = TRUE)

save(vimps, file=vimp_data_file)

vimp_tbl <- foreach(vimp=vimps, .combine=rbind) %do% vimp$mat
vimp_tbl <- vimp_tbl[,c("est", "cil", "ciu")] %>% round(3)
vimp_tbl <- vimp_tbl %>% mutate(variable=names(vimp_idxs)) %>% 
  mutate(importance=paste0(est, " (", cil, ", ", ciu, ")")) %>% 
  dplyr::select(variable, importance)
write.csv(vimp_tbl, file=sl_result_file)
