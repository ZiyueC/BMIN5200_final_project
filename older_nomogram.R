rm(list = ls())
library(Hmisc)
library(car)
library(rms)
library(pROC)
library(nsROC)
library(rmda)
library(dplyr)
library(readxl);library(caret);library(glmnet);library(corrplot)
library(Metrics);library(ggplot2)

library(plyr)

library(epiDisplay)
library(gtsummary)
library(ggDCA)
library("writexl")
library("pheatmap")
library("corrplot")

# set path
num_dir <- "C:/Users/Ellery/Desktop/BMIN5200/Final_Project/data/"
text_dir <- "C:/Users/Ellery/Desktop/BMIN5200/Final_Project/data/"
output_dir <- "C:/Users/Ellery/Desktop/BMIN5200/Final_Project/result/"


development_num_older <- read.csv(paste(num_dir,"training_num_older.csv",sep = ""), header=TRUE)
temporal_num_older <- read.csv(paste(num_dir,"testing_num_older.csv",sep = ""), header=TRUE)#development_num_older <- subset(development_num_older, select = -c(subject_id, hadm_id, inr_min, inr_max, braden_flag, braden_score_cat))
#temporal_num_older <- subset(temporal_num_older, select = -c(subject_id, hadm_id, inr_min, inr_max, braden_flag, braden_score_cat))
text_older <- read.csv(paste(text_dir, "text_all.csv",sep = ""), header=TRUE)
text_older <- subset(text_older, select = c(all_patient_ids, probs_1, db_type))
text_older <- plyr::rename(text_older, c("all_patient_ids"="id", "probs_1"="preICU_risk_score"))
training_dataset <- merge(
  x = subset(text_older[text_older$db_type == 'train',], select = c(id, preICU_risk_score)), 
  y = development_num_older, by = "id")
training_dataset$preICU_risk_score <- round(10*training_dataset$preICU_risk_score)  # more appropriate to expand the data by 10 times
validation_dataset<- merge(
  x = subset(text_older[text_older$db_type == 'val',], select = c(id, preICU_risk_score)), 
  y = development_num_older, by = "id")
validation_dataset$preICU_risk_score <- round(10*validation_dataset$preICU_risk_score) # more appropriate to expand the data by 10 times
test_dataset<- merge(
  x = subset(text_older[text_older$db_type == 'test',], select = c(id, preICU_risk_score)), 
  y = development_num_older, by = "id")
test_dataset$preICU_risk_score <- round(10*test_dataset$preICU_risk_score) # more appropriate to expand the data by 10 times
temp_dataset<- merge(
  x = subset(text_older[text_older$db_type == 'temp',], select = c(id, preICU_risk_score)), 
  y = temporal_num_older, by = "id")
temp_dataset$preICU_risk_score <- round(10*temp_dataset$preICU_risk_score) # more appropriate to expand the data by 10 times
training_dataset <- rbind(training_dataset, validation_dataset) # combine train and validation set
training_dataset <- subset(training_dataset, select = -c(first_careunit,ethnicity,anchor_year_group))
test_dataset <- subset(test_dataset, select = -c(first_careunit,ethnicity,anchor_year_group))
temp_dataset <- subset(temp_dataset, select = -c(first_careunit,ethnicity,anchor_year_group))
#training_dataset <- subset(training_dataset, select = -c(dobutamine,dopamine,norepinephrine,epinephrine))
#test_dataset <- subset(test_dataset, select = -c(dobutamine,dopamine,norepinephrine,epinephrine))
#temp_dataset <- subset(temp_dataset, select = -c(dobutamine,dopamine,norepinephrine,epinephrine))
cols <- c(
  "label", "code_status", "code_status_eva_flag", "activity_stand", "vent", 
  "gender", "delirium_eva_flag", "activity_bed", "delirium_flag", 
  "activity_sit", "activity_eva_flag", "pao2fio2ratio_vent_flag", 
  "pao2fio2ratio_novent_flag", "bilirubin_max_flag", "alp_max_flag", "alt_max_flag", 
  "ast_max_flag", "baseexcess_min_flag" , "fio2_max_flag", "lactate_max_flag", "lymphocytes_max_flag", 
  "lymphocytes_min_flag", "neutrophils_min_flag", "paco2_max_flag", 
  "pao2_min_flag", "nlr_flag", "vasopressor"
)
training_dataset[cols] <- lapply(training_dataset[cols], factor)  ## as.factor() could also be used
test_dataset[cols] <- lapply(test_dataset[cols], factor)  ## as.factor() could also be used
temp_dataset[cols] <- lapply(temp_dataset[cols], factor)  ## as.factor() could also be used
rm(development_num_older, temporal_num_older, validation_dataset, text_older, text_dir)

# print(paste0("training set rows",dim(training_dataset)[1]," training set columns",dim(training_dataset)[2])) # dim(x) 
# print(paste0("test set rows",dim(test_dataset)[1]," test set columns",dim(test_dataset)[2]))
# print(paste0("temp set rows",dim(temp_dataset)[1]," temp set columns",dim(temp_dataset)[2]))
# summary(training_dataset) 
# summary(test_dataset)
# summary(temp_dataset)



# # "gcs_min","cci_score","admission_type","shock_index","vent","activity_bed","code_status","vasopressor","resp_rate_mean","preICU_risk_score"
# x <- subset(training_dataset, select = c(preICU_risk_score, cci_score, gcs_min, shock_index, resp_rate_mean, lactate_max, inr_min, temperature_mean))
# # column with column co variation
# cov(x)
# pheatmap::pheatmap(cov(x)) # Heat Map
# # correlation
# cor(x)
# pheatmap::pheatmap(cor(x))  # Heat Map near to zero == less correlation
# corrplot(cor(x))
# png(filename=paste(output_dir,"corrplot.png",sep = ""), width = 1600, height = 1600, res = 300)
# corrplot(cor(x),method="color",addCoef.col="grey") 
# dev.off()
# rm(x)
# 
f_lrm_num_text <-lrm(label ~ preICU_risk_score + shock_index + code_status + activity_bed + vent + 
                       lactate_max + cci_score + resp_rate_mean + temperature_mean + gcs_min 
                     , data=training_dataset, x=TRUE, y=TRUE) #
# print(f_lrm_num_text)
# ddist <- datadist(training_dataset)
# options(datadist='ddist')
# png(file=paste(output_dir, "nomogram_num_text.png", sep = ""), width=2200, height=2300, res = 300)
# nomogram <- nomogram(f_lrm_num_text,fun=function(x)1/(1+exp(-x)),
#                      fun.at = c(0.01,0.1,0.3,0.5,0.8,0.9,0.99),
#                      funlabel = "Probability of death",
#                      lp=F,
#                      conf.int = F,
#                      abbrev = F)
# plot(nomogram)
# dev.off()
# svg(file=paste(output_dir, "nomogram_num_text.svg", sep = ""), width=11, height=9)
# nomogram <- nomogram(f_lrm_num_text,fun=function(x)1/(1+exp(-x)),
#                      fun.at = c(0.01,0.1,0.3,0.5,0.8,0.9,0.99),
#                      funlabel = "Probability of death",
#                      lp=F,
#                      conf.int = F,
#                      abbrev = F)
# plot(nomogram)
# dev.off()
# rm(ddist, nomogram)



loop_list <- list("test", "temp")
for(i in loop_list){
  if(i=='test'){
    data_plot <- test_dataset # temp_dataset
  }
  else{
    data_plot <- temp_dataset
  }

  f_lrm_num <-lrm(label ~ shock_index + code_status + activity_bed + vent + 
                    lactate_max + cci_score + resp_rate_mean + temperature_mean + gcs_min , data=training_dataset, x=TRUE, y=TRUE)

  pred_f_num_text <- predict(f_lrm_num_text, data_plot)
  pred_f_num_text <- 1/(1+exp(-pred_f_num_text))
  pred_f_num <- predict(f_lrm_num, data_plot)
  pred_f_num <- 1/(1+exp(-pred_f_num))
  # modelroc <- roc(data_plot$label,pred_f_num_text,ci = TRUE)

  roc_test_set <- subset(data_plot, select = c(id, label, preICU_risk_score, sofa, saps))
  roc_test_set$preICU_risk_score <- roc_test_set$preICU_risk_score/10
  roc_test_set$pred_f_num_text <- pred_f_num_text
  roc_test_set$pred_f_num <- pred_f_num
  write.csv(roc_test_set, paste(output_dir, "roc_", i, "_set.csv",sep = ""))


  roc.pred1 <- roc(roc_test_set$label, roc_test_set$pred_f_num_text, percent = TRUE, main = "Smoothing")
  roc.pred2 <- roc(roc_test_set$label, roc_test_set$pred_f_num, percent = TRUE, main = "Smoothing")
  roc.pred3 <- roc(roc_test_set$label, roc_test_set$preICU_risk_score, percent = TRUE, main = "Smoothing")
  roc.pred4 <- roc(roc_test_set$label, roc_test_set$saps, percent = TRUE, main = "Smoothing")
  roc.pred5 <- roc(roc_test_set$label, roc_test_set$sofa, percent = TRUE, main = "Smoothing")

  png(file=paste(output_dir, "roc_", i, ".png", sep = ""), width=1900, height=1600, res = 300)
  plot.roc(roc_test_set$label, roc_test_set$pred_f_num_text, percent = TRUE, add =  FALSE, asp = NA, cex.axis = 1.2, cex.lab = 1.5, col = "blue")
  lines(roc.pred2, type = "l", lty = 1, col = "red")
  lines(roc.pred3, type = "l", lty = 1, col = "green")
  lines(roc.pred4, type = "l", lty = 1, col = "yellow")
  lines(roc.pred5, type = "l", lty = 1, col = "purple")

  # set.seed(1234)
  # roc_95_pred1 <- ci.auc(roc.pred1, conf.level=0.95, method=c("bootstrap"), boot.n = 500, boot.stratified = TRUE)
  # set.seed(1234)
  # roc_95_pred2 <- ci.auc(roc.pred2, conf.level=0.95, method=c("bootstrap"), boot.n = 500, boot.stratified = TRUE)
  # set.seed(1234)
  # roc_95_pred3 <- ci.auc(roc.pred3, conf.level=0.95, method=c("bootstrap"), boot.n = 500, boot.stratified = TRUE)
  # set.seed(1234)
  # roc_95_pred4 <- ci.auc(roc.pred4, conf.level=0.95, method=c("bootstrap"), boot.n = 500, boot.stratified = TRUE)
  # set.seed(1234)
  # roc_95_pred5 <- ci.auc(roc.pred5, conf.level=0.95, method=c("bootstrap"), boot.n = 500, boot.stratified = TRUE)
  # legend1 <- paste("Nomogram score (", round(roc_95_pred1[1],1), round(roc_95_pred1[2],1), sep = " ")
  legend("bottomright", 
        legend = c(
          paste("Integrative score (AUC = ", round(roc.pred1$auc[1],1)/100, ")", sep = ""), 
          paste("Structured data score (AUC = ", round(roc.pred2$auc[1],1)/100, ")", sep = ""), 
          paste("preICU_risk_score (AUC = ", round(roc.pred3$auc[1],1)/100, ")", sep = ""), 
          paste("SAPSII (AUC = ", round(roc.pred4$auc[1],1)/100, ")", sep = ""), 
          paste("SOFA (AUC = ", round(roc.pred5$auc[1],1)/100, ")", sep = "")
          ), 
        col = c("blue", "red", "green", "yellow", "purple"),
        lty = c(1, 1, 1, 1, 1), cex=1.3, pt.cex = 1, lwd=2) 
  dev.off()
  rm(roc.pred1, roc.pred2, roc.pred3, roc.pred4, roc.pred5, data_plot)
}
rm(loop_list, i, roc_test_set)




cal <- calibrate(f_lrm_num_text, predy=seq(0, 1.0, length=50), B=500)#method=c("boot","crossvalidation",".632","randomization")
png(file=paste(output_dir, "calibration_train.png", sep = ""), width=1900, height=1600, res = 300) 
plot(cal, cex.axis = 1.2, cex.lab = 1.5)
dev.off()

pred.lg<- predict(f_lrm_num_text, temp_dataset)
temp_dataset$prob <- 1/(1+exp(-pred.lg))
png(file=paste(output_dir, "calibration_temp.png", sep = ""), width=1900, height=1600, res = 300)
val.prob(temp_dataset$prob, as.numeric(temp_dataset$label) , m=10, cex=1)
dev.off()
rm(pred.lg)

pred.lg<- predict(f_lrm_num_text, test_dataset)
test_dataset$prob <- 1/(1+exp(-pred.lg))
png(file=paste(output_dir, "calibration_test.png", sep = ""), width=1900, height=1600, res = 300)
val.prob(test_dataset$prob, as.numeric(test_dataset$label) , m=10, cex=1)
dev.off()
rm(pred.lg)


loop_list <- list("test", "temp")
for(i in loop_list){
  if(i=='test'){
    training_dataset_dca <- test_dataset # training_dataset, test_dataset, temp_dataset
  }
  else{
    training_dataset_dca <- temp_dataset
  }

  training_dataset_dca$label <- as.numeric(training_dataset_dca$label)-1
  model1 <- decision_curve(label ~ preICU_risk_score + shock_index + code_status + activity_bed + vent + 
                             lactate_max + cci_score + resp_rate_mean + temperature_mean + gcs_min + braden_score,
                          data=training_dataset_dca,
                          #thresholds = seq(0, .4, by = .01),
                          study.design = 'cohort',
                          bootstraps = 500)
  model2 <- decision_curve(label ~ shock_index + code_status + activity_bed + vent + 
                             lactate_max + braden_score + cci_score + resp_rate_mean + gcs_min + temperature_mean,
                          data=training_dataset_dca,
                          #thresholds = seq(0, .4, by = .01),
                          study.design = 'cohort',
                          bootstraps = 500)
  model3 <- decision_curve(label ~ preICU_risk_score,
                          data=training_dataset_dca,
                          #thresholds = seq(0, .4, by = .01),
                          study.design = 'cohort',
                          bootstraps = 500)
  model4 <- decision_curve(label ~ saps,
                          data=training_dataset_dca,
                          #thresholds = seq(0, .4, by = .01),
                          study.design = 'cohort',
                          bootstraps = 500)
  model5 <- decision_curve(label ~ sofa,
                          data=training_dataset_dca,
                          #thresholds = seq(0, .4, by = .01),
                          study.design = 'cohort',
                          bootstraps = 500)
  png(file=paste(output_dir, "dca_", i, ".png", sep = ""), width=1900, height=1600, res = 300)
  plot_decision_curve( list(model1, model2, model3, model4, model5),
                      curve.names = c("Nomogram score", "Structured data score", "preICU_risk_score", "SAPSII", "SOFA"),
                      col = c("blue", "red", "green", "yellow", "purple"),
                      confidence.intervals = FALSE,  #remove confidence intervals
                      cost.benefit.axis = FALSE, #remove cost benefit axis
                      xlim = c(0, 1), #set xlim
                      legend.position = "topright") #remove the legend
  dev.off()
  rm(model1, model2, model3, model4, model5, training_dataset_dca)
}
























#                 ------------------------- no use -------------------------                   #
#
# -------------------------------------------------------------------------------------------- #

library(rmda)
data(dcaData)
baseline.model <- decision_curve(Cancer~Age + Female + Smokes,
                                 data = dcaData,
                                 thresholds = seq(0, .4, by = .01),
                                 study.design = 'cohort',
                                 bootstraps = 10) #number of bootstraps should be higher
#plot using the defaults
plot_decision_curve(baseline.model,  curve.names = "baseline model")

full.model <- decision_curve(Cancer~Age + Female + Smokes + Marker1 + Marker2,
                             data = dcaData,
                             thresholds = seq(0, .4, by = .01),
                             bootstraps = 10)
summary(full.model, nround = 2, measure = "TPR")

# for lwd, the first two positions correspond to the decision curves, then 'all' and 'none'
plot_decision_curve( list(baseline.model, full.model),
                     curve.names = c("Baseline model", "Full model"),
                     col = c("blue", "red"),
                     lty = c(1,2),
                     lwd = c(3,2, 2, 1),
                     legend.position = "bottomright")



plot_decision_curve( list(baseline.model, full.model),
                     curve.names = c("Baseline model", "Full model"),
                     col = c("blue", "red"),
                     confidence.intervals = FALSE,  #remove confidence intervals
                     cost.benefit.axis = FALSE, #remove cost benefit axis
                     legend.position = "none") #remove the legend














plot.roc(temp_dataset$label, temp_dataset$sofa,
         main="ROC Curve", percent=TRUE,
         print.auc=TRUE,
         ci=TRUE, of="thresholds",
         thresholds="best",
         print.thres="best")















cal.pred1 <- calibrate(f_lrm_nog,predy=seq(0, 1.0, length=50), B=1000)
lines(cal.pred1, type = "l", lty = 1, col = "blue")






rt <- subset(temp_dataset, select = c(label, pre_risk_score, sofa))
rt$pre_risk_score <- rt$pre_risk_score/10
rt$pred_text_num_score <- pred_f_validation
y=colnames(rt)[1]

bioCol=c("#DB7093","#FF69B4","#FF1493")
if(ncol(rt)>3){
  bioCol=rainbow(ncol(rt))}

roc1 <- roc(rt[,y], rt[,2])


aucText=c( paste0(colnames(rt)[2],", AUC=",sprintf("%0.3f",roc1$auc[1])) )
plot(roc1, col=bioCol[1])
for(i in 3:ncol(rt)){
  roc1=roc(rt[,y], as.vector(rt[,i]))
  lines(roc1, col=bioCol[i-1])
  aucText=c(aucText, paste0(colnames(rt)[i],", AUC=",sprintf("%0.3f",roc1$auc[1])) )
}
legend("bottomright", aucText,lwd=2,bty="n",col=bioCol[1:(ncol(rt)-1)], pch = 16)
dev.off()