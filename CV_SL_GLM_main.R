
rm(list=ls())

library(dplyr)
library(caret)
library(foreach)
library(SuperLearner)
library(readxl)
library(doParallel)#Load parallel library

no_cores <- 30 # Set number of cores for parallel computing
main_data_path <- "./main_analysis_data.csv"
# output files
bootstrap_file <- "./results/SL_bootstrap.RData"
false_neg_pred_file <- "./results/pred_false_negative_point01.csv"
auc_diff_file <- "./results/main_auc_diff.txt"
report_table_file <- "./results/main_report_table.csv"
lr_table_file <- "./results/main_lr_table.csv"
superiority_file <- "./results/superiority_table.csv"
auc_plot_file <- "./results/main_auc.png"


group_testind <- function(groups, all_idxs, k=10){
  trfolds <- caret::groupKFold(groups, k=k)
  lapply(trfolds, function(tr) setdiff(all_idxs, tr))
}

bs.test.charac<-function(fullDat,bs.idxs, k) {
  
  
  dat <- fullDat[bs.idxs,]
  these_records <- record_ids[bs.idxs]
  
  # set the seed to ensure the folds are created the same
  set.seed(70889)
  
  folds <- group_testind(these_records, seq_len(length(bs.idxs)), k=k)
  
  fold_record_ids <- lapply(folds, function(fold) these_records[fold])
  
  inner_folds <- lapply(seq_along(folds), function(out.idx){
    train_folds <- setdiff(1:k, out.idx)
    fold_pkey <- fold_record_ids[train_folds] %>% do.call(c, .)
    fold_idxs <- folds[train_folds] %>% do.call(c, .)
    inner_folds <- group_testind(fold_pkey, seq_len(length(fold_idxs)), k=k)
    # caret::createFolds(dat$BI[fold_idxs], k=k)
  })
  
  #=====================================================================#
  #============ step 3: TEST CHARACTERISTICS ========================#
  #=====================================================================#
  
  
  bs.data.use_nosocial <-dat
  
  pred_list <- vector("list", length=k)
  for(i in 1:k){
    
    testIndexes <- folds[[i]]
    test_nosocial <- bs.data.use_nosocial[testIndexes,]
    train_nosocial <- bs.data.use_nosocial[-testIndexes,]
    
    Y_nosocial <-as.numeric(train_nosocial$BI)
    X_nosocial<-train_nosocial[,!names(train_nosocial)%in%c("BI")]
    X_holdout_nosocial <-test_nosocial[,!names(test_nosocial)%in%c("BI")]
    Y_holdout_nosocial<-as.numeric(test_nosocial$BI)
    
    validRows <- inner_folds[[i]]
    
    sl_nosocial = SuperLearner(Y = Y_nosocial, X = X_nosocial, family = binomial(), method = "method.AUC",
                               SL.library = SL.lib,
                               cvControl = list(shuffle=FALSE, validRows=validRows))
    
    
    glm_nosocial = SuperLearner(Y = Y_nosocial, X = X_nosocial, family = binomial(), method = "method.AUC",
                                SL.library = c( "SL.glm"),
                                cvControl = list(shuffle=FALSE, validRows=validRows))
    
    ##################SL RESULTS
    sl_nosocial_pred = predict(sl_nosocial, X_holdout_nosocial, onlySL = T,type="response")$pred
    
    ##################glm RESULTS
    glm_nosocial_pred = predict(glm_nosocial, X_holdout_nosocial, onlySL = T,type="response")$pred
    
    pred_list[[i]] <- cbind(
      `Record ID`=fold_record_ids[[i]],
      SL=sl_nosocial_pred,
      GLM=glm_nosocial_pred
    )
  }
  
  pred_df <- do.call(rbind, pred_list) %>% data.frame
  
  list(pred=pred_df)
}








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
record_ids <- data.features_nosocial[,1]

data.use_nosocial <- data.features_nosocial[-1]

data.use_nosocial$BI<-as.numeric(data.use_nosocial$BI)-1




nb<-500
bs.dat <- vector("list", length = nb)

set.seed(608991)
for(i in 1:nb) {
  idxs <- sample(nrow(data.use_nosocial),replace=TRUE)
  bs.dat[[i]]<-idxs
  
}

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

sl_nosocial = CV.SuperLearner(
  Y = Y_nosocial,
  X = X_nosocial,
  family = binomial(),
  method = "method.AUC",
  SL.library = SL.lib
)

glm_nosocial = CV.SuperLearner(
  Y = Y_nosocial,
  X = X_nosocial,
  family = binomial(),
  method = "method.AUC",
  SL.library = c("SL.glm")
)

full_data_pred_df <- data.frame(Y=Y_nosocial,
                                SL=sl_nosocial$SL.predict,
                                GLM=glm_nosocial$SL.predict)

cl <- makeCluster(no_cores, type="FORK", outfile="cluster.out")
registerDoParallel(cl)

results.list <- foreach(k=1:nb, 
                        .packages = c("SuperLearner","MASS","caret",
                                      "randomForest","dplyr","glmnet")
                        ) %dopar%  
  tryCatch(
    bs.test.charac(fullDat=data.use_nosocial, 
                   bs.idxs=bs.dat[[k]],
                   k=10),
    error = function(e) {
      print(e)
      NA
    }
  )

stopCluster(cl)

save(results.list, raw, nb,
     full_data_pred_df,
     file=bootstrap_file)


################################################################
# To inspect the results, load the file and run the code below
################################################################
library(caret)
library(dplyr)
library(ggplot2)

full_data_pred_df %>% 
  mutate(id=record_ids) %>% 
  filter(Y==1 & (SL < .01 | GLM < .01)) %>% 
  rename(LR = GLM) %>%
  mutate(LR = round(LR, 3),
         SL = round(SL,3)) %>% 
  select(id, SL, LR) %>% 
  write.csv(file=false_neg_pred_file, row.names=FALSE)

# full_data_pred_df %>%
#   mutate(id=record_ids) %>%
#   filter(Y==1 & (SL < .05 | GLM < .05)) %>%
#   rename(LR = GLM) %>%
#   mutate(LR = round(LR, 3),
#          SL = round(SL,3)) %>%
#   select(id, SL, LR) %>%
#   write.csv(file="pred_false_negative_point05.csv", row.names=FALSE)
# 
# full_data_pred_df %>%
#   mutate(id=record_ids) %>%
#   filter(Y==1 & (SL < .03 | GLM < .03)) %>%
#   rename(LR = GLM) %>%
#   mutate(LR = round(LR, 3),
#          SL = round(SL,3)) %>%
#   select(id, SL, LR) %>%
#   write.csv(file="pred_false_negative_point03.csv", row.names=FALSE)

### ROC Curves
library(pROC)
sl_roc = pROC::roc(Y~SL, data=full_data_pred_df)
lr_roc = pROC::roc(Y~GLM, data=full_data_pred_df)
rrc_roc = pROC::roc(response=raw$BI, predictor=raw$RLR)

auc_legend = sapply(1:3, function(i) {
  if(i == 1){
    roc = sl_roc
    name_str = "SL"
  } else if(i == 2){
    roc = lr_roc
    name_str = "LR"
  } else {
    roc = rrc_roc
    name_str = "RLR"
  }
  ci = ci.auc(roc)
  lims = round(ci[-2],3)
  auc = round(ci[2],3)
  
  paste0(name_str, " AUC: ", auc, " CI: (", lims[1], ", ", lims[2], ")")
})
png(auc_plot_file, width=600, height=600, pointsize = 16)
plot(rrc_roc, col="black", lty=3,
     xlab="Specificity (1 - False Positive %)",
     ylab="Sensitivity (True Positive %)")
plot(sl_roc, col="blue", lty=1,
     add=T)
plot(lr_roc, col="red", lty=5,
     add=T)
legend(x=0.65, y=0.15,
       legend=auc_legend,
       col=c("blue", "red", "black"),
       lty=c(1,5,3),
       cex=0.9)
dev.off()


joined.results.list <- lapply(seq_along(results.list), function(i) {
  x <- results.list[[i]]
  pred <- x$pred
  colnames(pred) <- c("Record.ID", "SL", "GLM")
  joined_pred <- raw %>% select(BI, RLR, Record.ID) %>%
    inner_join(pred, by="Record.ID")
  stopifnot(nrow(joined_pred) == nrow(pred))
  return(joined_pred)
})

# bootstrapped confusion matrix statistics
nb <- 500
conf_stat_names <- c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value")
cut.pts <- c(.01, .03, .05)

no_cores <- 2 #Set number of cores
cl <- makeCluster(no_cores)
registerDoParallel((cl))
tbl <- foreach(cut.point=cut.pts, .export="joined.results.list",
               .packages = c("caret","dplyr")
) %dopar% {
  confusion <- sapply(1:nb, function(i){
    boot_df <- joined.results.list[[i]]
    
    sl_conf <- boot_df %>%
      select(SL, BI) %>%
      mutate(SL=1*(SL > cut.point)) %>%
      table %>%
      caret::confusionMatrix(positive="1")
    sl_conf <- sl_conf$byClass[conf_stat_names]
    
    glm_conf <- boot_df %>%
      select(GLM, BI) %>%
      mutate(GLM=1*(GLM > cut.point)) %>%
      table %>%
      caret::confusionMatrix(positive="1")
    glm_conf <- glm_conf$byClass[conf_stat_names]
    
    naive_conf <- boot_df %>%
      select(RLR, BI) %>%
      table %>% 
      caret::confusionMatrix(positive="1")
    naive_conf <- naive_conf$byClass[conf_stat_names]
    
    diff_sl_conf <- sl_conf - naive_conf
    diff_glm_conf <- glm_conf - naive_conf
    
    ret <- c(sl_conf, glm_conf,
             diff_sl_conf, diff_glm_conf)
    names(ret) <- lapply(c("SL.", "GLM.", "DIFF.SL.", "DIFF.GLM."),
                         paste0, conf_stat_names) %>% do.call(c, .)
    ret
  })
}
stopCluster(cl)

sl_glm_report_table <- foreach(j=seq_along(cut.pts), 
                               .combine=cbind
) %do% {
  this_tbl <- tbl[[j]]
  sl_glm_stats <- this_tbl[1:8,]
  
  # point estimate = bootstrap means
  point.est <- rowMeans(sl_glm_stats)
  conf.ints <- apply(sl_glm_stats, 1, quantile, probs=c(0.025, 0.975))
  
  ret <- paste0(round(point.est, 3), " (",
                round(conf.ints[1,], 3), ", ",
                round(pmin(1, conf.ints[2,]), 3), ")")
  names(ret) <- names(point.est)
  ret
}
colnames(sl_glm_report_table) <- cut.pts
write.csv(sl_glm_report_table, file=report_table_file)

### Likelihood Ratio Calculations
lr_stats <- foreach(j=seq_along(cut.pts), 
                    .combine=cbind
) %do% {
  this_tbl <- tbl[[j]]
  sl_glm_stats <- this_tbl[1:8,]
  
  sl.lrps <- sl_glm_stats["SL.Sensitivity",]/(1-sl_glm_stats["SL.Specificity",])
  glm.lrps <- sl_glm_stats["GLM.Sensitivity",]/(1-sl_glm_stats["GLM.Specificity",])
  
  sl.lrns <- (1-sl_glm_stats["SL.Sensitivity",])/sl_glm_stats["SL.Specificity",]
  glm.lrns <- (1-sl_glm_stats["GLM.Sensitivity",])/sl_glm_stats["GLM.Specificity",]
  
  boot_stats <- rbind(sl.lrps, glm.lrps, sl.lrns, glm.lrns)
  point.est <- rowMeans(boot_stats)
  conf.ints <- apply(boot_stats, 1, quantile, probs=c(0.025, 0.975))
  
  
  ret <- paste0(round(point.est, 3), " (",
                round(conf.ints[1,], 3), ", ",
                round(conf.ints[2,], 3), ")")
  names(ret) <- c("SL.LR+", "GLM.LR+",
                  "SL.LR-", "GLM.LR-")
  ret
}
colnames(lr_stats) <- cut.pts
write.csv(lr_stats, file=lr_table_file)

diff_tests <- foreach(j=seq_along(cut.pts), 
                      .combine=cbind
) %do% {
  this_tbl <- tbl[[j]]
  diff_stats <- this_tbl[c(9:10, 13:14),]
  
  # test hypothesis that diff specificity > 0
  upper_cis <- apply(diff_stats, 1, quantile, probs=0.05)
  upper_pvals <- apply(diff_stats, 1, function(x) mean(x <= 0))
  
  # test hypothesis that diff sensitivity < 0
  lower_cis <- apply(diff_stats, 1, quantile, probs=0.95)
  lower_pvals <- apply(diff_stats, 1, function(x) mean(x >= 0))
  
  round(c(upper_cis, lower_cis, upper_pvals, lower_pvals), 3)
}
colnames(diff_tests) <- cut.pts
rownames(diff_tests) <- foreach(a=c("Upper.CI.","Lower.CI.","Upper.p.","Lower.p."),
                                .combine="c") %:%
  foreach(b=c("SL.", "GLM."),.combine="c") %:%
  foreach(d=c("Diff.Sens","Diff.Spec"),.combine="c") %do% paste0(a,b,d)
write.csv(diff_tests, file=superiority_file)


#### Diff in AUC
cl <- makeCluster(no_cores)
registerDoParallel((cl))
auc_diffs <- foreach(i=1:nb, .combine=cbind, .packages="pROC") %dopar% {
  
  boot_df <- joined.results.list[[i]]
  label <- factor(boot_df$BI, levels=c(0,1), labels=c("No", "Yes"))
  score_sl <- boot_df$SL
  score_glm <- boot_df$GLM
  score_naive <- boot_df$RLR
  
  sl_auc <- pROC::auc(response=label, predictor=score_sl)
  glm_auc <- pROC::auc(response=label, predictor=score_glm)
  naive_auc <- pROC::auc(response=label, predictor=score_naive)
  
  diff_sl_auc <- sl_auc - naive_auc
  diff_glm_auc <- glm_auc - naive_auc
  
  ret <- c(diff_sl_auc, diff_glm_auc)
  names(ret) <- c("DIFF.SL.AUC", "DIFF.GLM.AUC")
  ret
}
stopCluster(cl)

auc_diff_means <- rowMeans(auc_diffs)
auc_diff_cis <- apply(auc_diffs, 1, quantile, probs=c(0.025, 0.975))

auc_diff_results <- paste0(round(auc_diff_means, 3), " (",
                           round(auc_diff_cis[1,], 3), ", ",
                           round(pmin(1, auc_diff_cis[2,]), 3), ")")
names(auc_diff_results) <- c("DIFF.SL.AUC", "DIFF.GLM.AUC")
capture.output(
  {
    print("Improvement (over RLR) in AUC")
    print(auc_diff_results)
  },
  file=auc_diff_file
)
