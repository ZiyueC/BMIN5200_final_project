
library(Hmisc)
library(car)
library(rms)
library(pROC)
library(rmda)
library(dplyr)
library(readxl);library(caret);library(glmnet);library(corrplot)
library(Metrics);library(ggplot2)
library(plyr)
library(epiDisplay)
#library(gtsummary)
library("writexl")


num_dir <- "C:/Users/Ellery/Desktop/BMIN5200/Final_Project/data/"
text_dir <- "C:/Users/Ellery/Desktop/BMIN5200/Final_Project/data/"
output_dir <- "C:/Users/Ellery/Desktop/BMIN5200/Final_Project/result/"

development_num_older <- read.csv(paste(num_dir,"training_num_older.csv",sep = ""), header=TRUE)
temporal_num_older <- read.csv(paste(num_dir,"testing_num_older.csv",sep = ""), header=TRUE)
#development_num_older <- subset(development_num_older, select = -c(inr_min, inr_max, braden_flag, braden_score_cat))
#temporal_num_older <- subset(temporal_num_older, select = -c(hadm_id, inr_min, inr_max, braden_flag, braden_score_cat))
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
training_dataset <- subset(training_dataset, select = -c(id,first_careunit,ethnicity,anchor_year_group))
test_dataset <- subset(test_dataset, select = -c(id,first_careunit,ethnicity,anchor_year_group))
temp_dataset <- subset(temp_dataset, select = -c(id,first_careunit,ethnicity,anchor_year_group))
# training_dataset <- subset(training_dataset, select = -c(dobutamine,dopamine,norepinephrine,epinephrine))
# test_dataset <- subset(test_dataset, select = -c(dobutamine,dopamine,norepinephrine,epinephrine))
# temp_dataset <- subset(temp_dataset, select = -c(dobutamine,dopamine,norepinephrine,epinephrine))
cols <- c(
  "label", "code_status", "code_status_eva_flag", "activity_stand", "vent", 
  "gender", "delirium_eva_flag", "activity_bed", "delirium_flag", 
  "activity_sit", "activity_eva_flag", "pao2fio2ratio_vent_flag", 
  "pao2fio2ratio_novent_flag", "bilirubin_max_flag", "alp_max_flag", "alt_max_flag", 
  "ast_max_flag", "baseexcess_min_flag" , "fio2_max_flag", "lactate_max_flag", "lymphocytes_max_flag", 
  "lymphocytes_min_flag", "neutrophils_min_flag", "paco2_max_flag", 
  "pao2_min_flag", "nlr_flag", "vasopressor", "rrt", "gnri_flag"
)

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


Uni_glm_model<- 
  function(x){
    FML<-as.formula(paste0("label==1~",x))
    glm1<-glm(FML,data=training_dataset,family = binomial)
    glm2<-summary(glm1)
    OR<-round(exp(coef(glm1)),2)
    SE<-glm2$coefficients[,2]
    CI5<-round(exp(coef(glm1)-1.96*SE),2)
    CI95<-round(exp(coef(glm1)+1.96*SE),2)
    CI<-paste0(CI5,'-',CI95)
    P<-round(glm2$coefficients[,4],2)
    Uni_glm_model <- data.frame('Characteristics'=x,
                                'OR' = OR,
                                'CI' = CI,
                                'P' = P)[-1,]
    return(Uni_glm_model)
  }
variable.names<- colnames(training_dataset)
variable.names <- variable.names[! variable.names %in% c('label', 'sofa', 'oasis', 'saps', 'apsiii')];variable.names

Uni_glm <- lapply(variable.names, Uni_glm_model)
Uni_glm<- ldply(Uni_glm,data.frame);Uni_glm

linearvars <- setdiff(colnames(subset(training_dataset, select = -c(sofa,oasis,saps,apsiii))),'label')
linearphrase <- paste(linearvars, collapse=" + ")
fmla <- as.formula( paste0( "label==1~", linearphrase))
names<- glm(fmla,
            data=training_dataset,
            family = binomial)
name<-data.frame(summary(names)$aliased)
rownames(Uni_glm)<-rownames(name)[-1]
Uni_glm <- Uni_glm[,-1]
del_col_Uni <- Uni_glm
del_col_Uni <- del_col_Uni[del_col_Uni$P > 0.05,]
del_col_Uni <- rownames(del_col_Uni)
del_col_Uni <- gsub("1", "", del_col_Uni)
# del_col_Uni <- del_col_Uni[del_col_Uni != "braden_score"] 
Uni_glm$P[Uni_glm$P==0] <- "<0.001"
Uni_glm$OR_95CI <- paste0(Uni_glm$OR, ' (', Uni_glm$CI, ')')

if (dir.exists(paste(output_dir, "nomogram/", sep = ""))) {  
  print("The direcoty exists")
} else {  
  dir.create(paste(output_dir, "nomogram/", sep = ""))
}

write.csv(Uni_glm, paste(output_dir,"nomogram/","univariable_models.csv",sep = ""))
rm(name, names, Uni_glm, fmla, linearvars, linearphrase, variable.names)


`%ni%` <- Negate(`%in%`)
X <- subset(training_dataset, select = -c(preICU_risk_score,sofa,oasis,saps,apsiii,label))
X <- subset(X,select = names(X) %ni% del_col_Uni)
X <- data.matrix(X)
Y <- training_dataset$label
set.seed(12345)
cv.fit <- cv.glmnet(X,Y,alpha = 1, nfolds =5,
                    family = "binomial",type.measure = "class")
png(filename=paste(output_dir,"nomogram/","lasso_cv_info1.png",sep = ""), width = 1600, height = 1400, res = 300)
plot(cv.fit)
dev.off()
f1 =glmnet(X, Y, family="binomial", alpha=1)
png(filename=paste(output_dir,"nomogram/","lasso_cv_info2.png",sep = ""), width = 1600, height = 1400, res = 300)
plot(f1,xvar="lambda", label=TRUE)
dev.off()
lasso.coef1 <- predict(cv.fit, s=cv.fit$lambda.1se, type = "coefficients");lasso.coef1
lasso.coef2 <- predict(cv.fit, s=cv.fit$lambda.min, type = "coefficients");lasso.coef2
lasso_best <- glmnet(X,Y,alpha = 1,lambda = cv.fit$lambda.1se,
                     family = "binomial")
gx <- list(coef(lasso_best))
coef_info <- data.frame(Group = rownames(gx[[1]]), Value = gx[[1]][,1])
del_col_lassocv <- coef_info
del_col_lassocv <- del_col_lassocv[del_col_lassocv$Value == 0,]
del_col_lassocv <- del_col_lassocv$Group
write.csv(coef_info, paste(output_dir,"nomogram/","lasso_cv_info.csv",sep = ""))
coef_info <- coef_info[coef_info$Value != 0,]
linearvars <- coef_info$Group[-1]
linearvars <- grep("_flag", linearvars, invert=TRUE, value = TRUE) # remove flag columns
linearphrase <- paste(linearvars, collapse=" + ")
fmla_lasso <- as.formula( paste0("label==1~", linearphrase))
glm1 <- glm(fmla_lasso, family = binomial, data=subset(training_dataset, select = -c(preICU_risk_score,sofa,oasis,saps,apsiii)))
set.seed(12345)
glm2 <- stepAIC(glm1,direction="both")
gx <- list(coef(glm2))
coef_info <- as.data.frame(gx[[1]])
coef_info <- plyr::rename(coef_info, c("gx[[1]]"="coefficient"))
coef_info <- cbind(names = rownames(coef_info), coef_info)
rownames(coef_info) <- 1:nrow(coef_info)
coef_info <- coef_info[-1,]
coef_info$names <- gsub('1', '', coef_info$names)
write.csv(coef_info, paste(output_dir,"nomogram/","stepAIC_info.csv",sep = ""))
use_col_aic <- coef_info$names
rm(glm1, glm2, gx, lasso.coef1, lasso.coef2, lasso_best, fmla_lasso, linearphrase, linearvars, Y, X, f1)



#linearvars <- use_col_aic
linearvars <- c("gcs_min","cci_score","shock_index","vent","activity_bed","code_status","resp_rate_mean","lactate_max",
                "braden_score","hemoglobin_min","temperature_mean","potassium_max","preICU_risk_score")
linearvars <- c("gcs_min","cci_score","shock_index","vent","activity_bed","code_status","resp_rate_mean","lactate_max",
                "hemoglobin_min","temperature_mean","potassium_max","preICU_risk_score")


linearphrase <- paste(linearvars, collapse=" + ")
fmla_aic <- as.formula( paste0("label==1~", linearphrase))

glm1 <- glm(fmla_aic, family = binomial, data=training_dataset)
res <- logistic.display(glm1, simplified=T);res
res <- res$table
res[,'OR'] <- round(res[,'OR'],2)
res[,'lower95ci'] <- round(res[,'lower95ci'],2)
res[,'upper95ci'] <- round(res[,'upper95ci'],2)
res[,'Pr(>|Z|)'] <- round(res[,'Pr(>|Z|)'],5)
res <- as.data.frame(res)
res$OR_95CI <- paste0(res$OR, ' (', res$lower95ci, '-', res$upper95ci, ')')
res$`Pr(>|Z|)`[res$`Pr(>|Z|)`==0] <- "<0.001"



write.csv(res, paste(output_dir,"nomogram/","multivariable_info.csv",sep = ""))





##### SHAP for each variable

glm1 = glm1

library(DALEX)
library(iml)
library(purrr)

# Step 1: Compute SHAP values


test_dataset_demo = test_dataset[1:10,]
test_dataset_demo_1 = test_dataset_demo[,colnames(test_dataset_demo) %in% linearvars]
test_dataset_demo_1$sofa = test_dataset_demo$sofa


predictor <- Predictor$new(
  model = glm1, 
  data = test_dataset_demo_1[ , !names(test_dataset_demo_1) %in% "sofa"],  # Exclude the response variable
  y = test_dataset_demo_1$sofa  # Specify the response variable
)


shap_values_list <- map(seq_len(nrow(test_dataset_demo_1)), function(i) {
  Shapley$new(predictor, x.interest = test_dataset_demo_1[i, !names(test_dataset_demo_1) %in% "sofa"])$results
})

# Combine SHAP values for all observations (optional)
shap_values_df <- do.call(rbind, shap_values_list)


shap_values_df$temperature_mean <- test_dataset_demo_1$temperature_mean
shap_values_df_filtered <- shap_values_df[shap_values_df$feature == "temperature_mean", ]


library(ggplot2)

ggplot(data = shap_values_df, aes(x = temperature_mean, y = phi)) +  # `phi` contains SHAP values
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "temperature_mean",
    y = "SHAP Value",
    title = "SHAP Values for temperature_mean"
  ) +
  theme_minimal()


ggplot(data = shap_values_df_filtered, aes(x = temperature_mean, y = phi)) + 
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "temperature_mean",
    y = "SHAP Value",
    title = "SHAP Values for temperature_mean"
  ) +
  theme_minimal()


## finalized SHAP cycle


library(DALEX)
library(iml)
library(purrr)

test_dataset_demo = test_dataset[1:200,]
test_dataset_demo_1 = test_dataset_demo[,colnames(test_dataset_demo) %in% linearvars]
test_dataset_demo_1$sofa = test_dataset_demo$sofa


predictor <- Predictor$new(
  model = glm1, 
  data = test_dataset_demo_1[ , !names(test_dataset_demo_1) %in% "sofa"],  # Exclude the response variable
  y = test_dataset_demo_1$sofa  # Specify the response variable
)


shap_values_list <- map(seq_len(nrow(test_dataset_demo_1)), function(i) {
  Shapley$new(predictor, x.interest = test_dataset_demo_1[i, !names(test_dataset_demo_1) %in% "sofa"])$results
})



# Combine SHAP values for all observations (optional)
shap_values_df <- do.call(rbind, shap_values_list)
shap_values_df$temperature_mean <- test_dataset_demo_1$temperature_mean
shap_values_df_filtered <- shap_values_df[shap_values_df$feature == "temperature_mean", ]

ggplot(data = shap_values_df_filtered, aes(x = temperature_mean, y = phi)) + 
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "temperature_mean",
    y = "SHAP Value",
    title = "SHAP Values for temperature_mean"
  ) +
  theme_minimal()




shap_values_df <- do.call(rbind, shap_values_list)
shap_values_df$resp_rate_mean <- test_dataset_demo_1$resp_rate_mean
shap_values_df_filtered_resp_rate <- shap_values_df[shap_values_df$feature == "resp_rate_mean", ]

ggplot(data = shap_values_df_filtered_resp_rate, aes(x = resp_rate_mean, y = phi)) + 
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "resp_rate_mean",
    y = "SHAP Value",
    title = "SHAP Values for resp_rate_mean"
  ) +
  theme_minimal()

shap_values_df <- do.call(rbind, shap_values_list)
shap_values_df$hemoglobin_min <- test_dataset_demo_1$hemoglobin_min
shap_values_df_filtered_hemoglobin <- shap_values_df[shap_values_df$feature == "hemoglobin_min", ]

ggplot(data = shap_values_df_filtered_hemoglobin, aes(x = hemoglobin_min, y = phi)) + 
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "hemoglobin_min",
    y = "SHAP Value",
    title = "SHAP Values for hemoglobin_min"
  ) +
  theme_minimal()

shap_values_df <- do.call(rbind, shap_values_list)
shap_values_df$potassium_max <- test_dataset_demo_1$potassium_max
shap_values_df_filtered_potassium <- shap_values_df[shap_values_df$feature == "potassium_max", ]

ggplot(data = shap_values_df_filtered_potassium, aes(x = potassium_max, y = phi)) + 
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "potassium_max",
    y = "SHAP Value",
    title = "SHAP Values for potassium_max"
  ) +
  theme_minimal()

shap_values_df <- do.call(rbind, shap_values_list)
shap_values_df$gcs_min <- test_dataset_demo_1$gcs_min
shap_values_df_filtered_gcs <- shap_values_df[shap_values_df$feature == "gcs_min", ]

ggplot(data = shap_values_df_filtered_gcs, aes(x = gcs_min, y = phi)) + 
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "gcs_min",
    y = "SHAP Value",
    title = "SHAP Values for gcs_min"
  ) +
  theme_minimal()

shap_values_df <- do.call(rbind, shap_values_list)
shap_values_df$lactate_max <- test_dataset_demo_1$lactate_max
shap_values_df_filtered_lactate <- shap_values_df[shap_values_df$feature == "lactate_max", ]

ggplot(data = shap_values_df_filtered_lactate, aes(x = lactate_max, y = phi)) + 
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "lactate_max",
    y = "SHAP Value",
    title = "SHAP Values for lactate_max"
  ) +
  theme_minimal()

shap_values_df <- do.call(rbind, shap_values_list)
shap_values_df$shock_index <- test_dataset_demo_1$shock_index
shap_values_df_filtered_shock <- shap_values_df[shap_values_df$feature == "shock_index", ]

ggplot(data = shap_values_df_filtered_shock, aes(x = shock_index, y = phi)) + 
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "shock_index",
    y = "SHAP Value",
    title = "SHAP Values for shock_index"
  ) +
  theme_minimal()












predictor <- Predictor$new(
  model = glm1, 
  data = test_dataset_demo_1[ , !names(test_dataset_demo_1) %in% "sofa"],  # Exclude the response variable
  y = test_dataset_demo_1$sofa  # Response variable
)

shapley <- Shapley$new(predictor, x.interest = test_dataset_demo_1[1, ])
shap_values_list <- map(seq_len(nrow(test_dataset)), function(i) {
  Shapley$new(predictor, x.interest = test_dataset[i, !names(test_dataset) %in% "sofa"])$results
})

shap_values_df <- do.call(rbind, shap_values_list)

shap_values_df$feature <- rep(colnames(test_dataset[, !names(test_dataset) %in% "sofa"]), each = nrow(test_dataset))

shap_values_df$value <- as.vector(as.matrix(test_dataset[, !names(test_dataset) %in% "sofa"]))

library(ggplot2)

ggplot(shap_values_df, aes(x = feature, y = phi, color = value)) +
  geom_violin() +
  coord_flip() +
  scale_color_gradient(low = "blue", high = "red") +  # Gradient color for feature values
  labs(
    title = "SHAP Summary Plot",
    x = "Feature",
    y = "SHAP Value (Impact on Model)"
  ) +
  theme_minimal()






rm(glm1, fmla_aic, linearphrase, linearvars, res)




glm1 <- glm(label ~ preICU_risk_score + shock_index + code_status + activity_bed + vent + 
              lactate_max + cci_score + resp_rate_mean + temperature_mean + gcs_min + braden_score, 
            family = binomial, data=training_dataset)
res <- logistic.display(glm1, simplified=T);res
res <- res$table
write.csv(res, paste(output_dir,"nomogram/","multivariable_info_noround.csv",sep = ""))












library(ggplot2)

data <- data.frame(
  NoteType = c("All", "History of present illness", "Physical exam", "Chief complaint", 
               "Past medical history", "Medications on admission"),
  InternalValidation = c(0.842, 0.837, 0.837, 0.823, 0.812, 0.812),
  InternalCI_Lower = c(0.822, 0.817, 0.818, 0.803, 0.791, 0.789),
  InternalCI_Upper = c(0.862, 0.857, 0.855, 0.842, 0.836, 0.833),
  TestingValidation = c(0.854, 0.843, 0.84, 0.835, 0.821, 0.823),
  TestingCI_Lower = c(0.83, 0.819, 0.819, 0.81, 0.799, 0.8),
  TestingCI_Upper = c(0.871, 0.864, 0.859, 0.854, 0.842, 0.845)
)

ggplot(data, aes(x = NoteType, y = InternalValidation)) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.8) +
  geom_errorbar(aes(ymin = InternalCI_Lower, ymax = InternalCI_Upper), 
                width = 0.2, color = "black", size = 0.6) +
  geom_text(aes(label = sprintf("%.3f", InternalValidation)), 
            vjust = -0.5, color = "black", size = 4) +
  labs(title = "Internal Validation AUROC", x = "Note Type", y = "AUROC") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 1)

ggplot(data, aes(x = NoteType, y = TestingValidation)) +
  geom_bar(stat = "identity", fill = "gray40", alpha = 0.8) +
  geom_errorbar(aes(ymin = TestingCI_Lower, ymax = TestingCI_Upper), 
                width = 0.2, color = "black", size = 0.6) +
  geom_text(aes(label = sprintf("%.3f", TestingValidation)), 
            vjust = -0.5, color = "black", size = 4) +
  labs(title = "Testing Validation AUROC", x = "Note Type", y = "AUROC") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 1)
















glm1 <- glm(label~pre_risk_score + chloride_min + bicarbonate_max + activity_bed + cci_score
            + gcs_min + age + temperature_mean + inr_min + vent + code_status + shock_index
            + pre_icu_los_day + spo2_min + resp_rate_mean + bilirubin_max + lactate_max
            , family = binomial, data=training_dataset)
res <- logistic.display(glm1, simplified=T);res

glm1 <- glm(label~pre_risk_score + activity_bed + cci_score
            + gcs_min + age + temperature_mean + inr_min + vent + code_status + shock_index
            + pre_icu_los_day + resp_rate_mean + lactate_max
            , family = binomial, data=training_dataset)
res <- logistic.display(glm1, simplified=T);res



# #         ------------------------------ Step 4. nomogram ------------------------------            #
# #
# #         ------------------------------------------------------------------------------            #
# f_lrm_nog <-lrm(label ~ pre_risk_score + activity_bed + cci_score
#                 + gcs_min + age + temperature_mean + inr_min + vent + code_status + shock_index
#                 + pre_icu_los_day + resp_rate_mean + lactate_max
#                 , data=training_dataset, x=TRUE, y=TRUE) # 
# print(f_lrm_nog)
# ddist <- datadist(training_dataset)
# options(datadist='ddist')
# pdf(file=paste(output_dir, "nomogram/", "nomogram_num.pdf", sep = ""), width=10, height=10) 
# nomogram <- nomogram(f_lrm_nog,fun=function(x)1/(1+exp(-x)),
#                      fun.at = c(0.01,0.1,0.3,0.5,0.8,0.9,0.99),
#                      funlabel = "Probability of death",
#                      lp=F,
#                      conf.int = F,
#                      abbrev = F
# )
# plot(nomogram)
# dev.off()



# pred_f_validation <- predict(f_lrm_nog, temp_dataset)
# pred_f_validation<-1/(1+exp(-pred_f_validation))
# modelroc <- roc(temp_dataset$label,pred_f_validation,ci = TRUE)

# plot.roc(temp_dataset$label, temp_dataset$sofa,
#          main="ROC Curve", percent=TRUE,
#          print.auc=TRUE,
#          ci=TRUE, of="thresholds",
#          thresholds="best",
#          print.thres="best")