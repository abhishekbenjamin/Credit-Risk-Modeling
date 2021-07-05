#install.packages("devtools", dependencies = TRUE)
#library(devtools)
#devtools::install_github("ayhandis/creditR")

library(creditR)
#ls("package:creditR")

library(dplyr)
library(zoo)
library(stringr)
library(woeBinning)

back_up <- bank_loan_data

df <- back_up

df <- df%>% select(-annual_inc_joint,-collection_recovery_fee,-collections_12_mths_ex_med,
                   -delinq_2yrs,-desc,-emp_title,-id,-initial_list_status,-inq_last_6mths,
                   -member_id,-policy_code,-pymnt_plan,-recoveries,-title,-url,-zip_code,
                   -open_acc_6m,-open_il_6m,-open_il_12m,-open_il_24m,-mths_since_rcnt_il,
                   -total_bal_il,-il_util,-open_rv_12m,-open_rv_24m,-max_bal_bc,-all_util,
                   -inq_fi,-total_cu_tl,-inq_last_12m,-tot_coll_amt,-dti_joint,-X)


df$verification_status_joint <- NULL
df$mths_since_last_major_derog <- NULL
df$mths_since_last_record <- NULL  
df$mths_since_last_delinq <- NULL
df$application_type <- NULL
df$verification_status <- NULL
df$next_pymnt_d <- NULL


# Missing value imputation 

df$annual_inc[is.na(df$annual_inc)] = median(df$annual_inc, na.rm = T)
df$open_acc[is.na(df$open_acc)] = median(df$open_acc, na.rm = T) 
df$annual_inc[is.na(df$annual_inc)] = 0
df$revol_util[is.na(df$revol_util)] = median(df$revol_util, na.rm = T)
df$total_acc[is.na(df$total_acc)] = median(df$total_acc, na.rm = T)
df$acc_now_delinq[is.na(df$acc_now_delinq)] = 0
df$tot_cur_bal[is.na(df$tot_cur_bal)] = median(df$tot_cur_bal, na.rm = T)
df$total_rev_hi_lim[is.na(df$total_rev_hi_lim)] = median(df$total_rev_hi_lim, na.rm = T)
df$pub_rec[is.na(df$pub_rec)] = 0
df$dti[is.na(df$dti)] = median(df$dti, na.rm = T) 


# Preprocessing Target variable: loan_status

unique(df$loan_status)

new_loan_status <- function(x) {
  ifelse(x %in% c("Charged Off","Default","Late (31-120 days)",
                  "Late (16-30 days)","Does not meet the credit policy. Status:Charged Off"),1,0)
}

df <- df %>% mutate(across(loan_status, new_loan_status, .names = "{col}_recode"))

df$loan_status <- NULL

# Tranforming categorical variables into factor

df$grade <- as.factor(df$grade)
df$sub_grade <- as.factor(df$sub_grade)
df$home_ownership <- as.factor(df$home_ownership)
df$addr_state <- as.factor(df$addr_state)
df$purpose <- as.factor(df$purpose)


# Pre-processing variable: earliest_cr_line

df$earliest_cr_line_dt <- as.Date(as.yearmon(df$earliest_cr_line, "%b-%y"))

class(df$earliest_cr_line_dt)

date_1 <- as.Date("2017-12-01")

df$mts_since_earliest_cr_line <- round((date_1 - df$earliest_cr_line_dt)/30)

df$mts_since_earliest_cr_line <- as.numeric(df$mts_since_earliest_cr_line)

class(df$mts_since_earliest_cr_line)

summary(df$mts_since_earliest_cr_line) 

df$mts_since_earliest_cr_line[df$mts_since_earliest_cr_line <0] = max(df$mts_since_earliest_cr_line, na.rm = F)

df$mts_since_earliest_cr_line[is.na(df$mts_since_earliest_cr_line)] = median(df$mts_since_earliest_cr_line,na.rm=T)

summary(df$mts_since_earliest_cr_line)

df$earliest_cr_line <- NULL
df$earliest_cr_line_dt <- NULL

# Pre-processing variable: emp_length

df$emp_length_int <- str_replace_all(df$emp_length, "years","")
df$emp_length_int <- str_replace_all(df$emp_length_int, "< 1 year","0")
df$emp_length_int <- str_replace_all(df$emp_length_int, "year","")
df$emp_length_int <- str_replace_all(df$emp_length_int, "\\+","")
df$emp_length_int[df$emp_length_int == ""] <- 0

df$emp_length_int <- as.integer(df$emp_length_int)

df$emp_length <- NULL

# Pre-processing variable: issue_d

head(df$issue_d)

df$issue_d_dt <- as.Date(as.yearmon(df$issue_d, "%b-%y"))

df$mts_since_issue_d <- round((date_1 - df$issue_d_dt)/30)

df$mts_since_issue_d <- as.numeric(df$mts_since_issue_d)

df$issue_d_dt <- NULL
df$issue_d <- NULL 

# Pre-processing: term variable

df$term_int <- str_replace_all(df$term,"months","")
df$term_int <- as.integer(df$term_int)

df$term <- NULL 

# Pre-processing last_pymnt_d

df$last_pymnt_d_dt <- as.Date(as.yearmon(df$last_pymnt_d,"%b-%y")) 

df$mts_since_last_pymnt_d <- round((date_1 - df$last_pymnt_d_dt)/30)

unique(df$mts_since_last_pymnt_d)

df$mts_since_last_pymnt_d[is.na(df$mts_since_last_pymnt_d)] <- median(df$mts_since_last_pymnt_d,na.rm = T)

df$mts_since_last_pymnt_d <- as.numeric(df$mts_since_last_pymnt_d)

summary(df$mts_since_last_pymnt_d)

df$last_pymnt_d <- NULL
df$last_pymnt_d_dt <- NULL

# Pre-processing last_credit_pull_d

df$last_credit_pull_d_dt <- as.Date(as.yearmon(df$last_credit_pull_d, "%b-%y"))

df$mts_since_last_credit_pull_d <-  round((date_1 - df$last_credit_pull_d_dt)/30)

unique(df$mts_since_last_credit_pull_d)

df$mts_since_last_credit_pull_d[is.na(df$mts_since_last_credit_pull_d)] <- median(df$mts_since_last_credit_pull_d,na.rm=T)

df$mts_since_last_credit_pull_d <- as.numeric(df$mts_since_last_credit_pull_d)

summary(df$mts_since_last_credit_pull_d)

df$last_credit_pull_d <- NULL
df$last_credit_pull_d_dt <- NULL

# Splitting data into train - test split

set.seed(123)

pd <- sample(2, nrow(df), replace = T, prob = c(0.8,0.2))

train <- df[pd == 1,]

test  <- df[pd == 2,]

# WoE transformation

woerules <- woe.binning(df = train,target.var = "loan_status_recode",pred.var = train,event.class = 1)

train_woe <- woe.binning.deploy(train, woerules, add.woe.or.dum.var='woe')

#Creating a data set with only Transformed Variables and Target Variable

train_woe <- woe.get.clear.data(train_woe,default_flag = "loan_status_recode",prefix = "woe")

colnames(train_woe)

#Applying the WoE rules used on train data to the test data

test_woe <- woe.binning.deploy(test, woerules, add.woe.or.dum.var='woe')

test_woe <- woe.get.clear.data(test_woe,default_flag = "loan_status_recode",prefix = "woe")

# IV and GINI Calculations for variable selection for whole data set
## Generally, a threshold value of 0.30 for IV & 0.10 for Gini is used. 

IV.calc.data(train_woe,"loan_status_recode")

Gini.univariate.data(train_woe,"loan_status_recode")

#  Creating a new data set based on GINI threshold to select variables

eliminated_data <- Gini_elimination(train_woe,"loan_status_recode",0.10)

colnames(eliminated_data)

# Clustering to find Variables with similar characteristics

clustered_data <- variable.clustering(eliminated_data,"loan_status_recode", 4)

clustered_data

# Finding correlation among Clusters
# correlation.cluster(eliminated_data,clustered_data,variables = "variable",clusters = "Group")

# Fitting Logistic Regression

model= glm(loan_status_recode ~ ., family = binomial(link = "logit"),  data = eliminated_data)

summary(model)

# Finding important variables based on weights
## Weights are calculated on the basis of the effect of a single unit change on the probability.

woe.glm.feature.importance(eliminated_data, model, "loan_status_recode")

#Generating PD values for train and test data

ms_train_data <- cbind(eliminated_data,model$fitted.values)


ms_test_data <- cbind(test_woe[,colnames(eliminated_data)], 
                      predict(model,type = "response", newdata = test_woe))

# NOTE: elimnated_data has used train_woe to screen variables based on GINI hence we used
## test_woe[,colnames(eliminated_data)] above

colnames(eliminated_data)

# c-binding of column names using eliminated_data and also adding PD name

colnames(ms_train_data) <- c("woe.last_pymnt_amnt.binned","woe.total_rec_prncp.binned",
                             "woe.total_pymnt.binned","woe.total_pymnt_inv.binned",
                             "woe.mts_since_last_pymnt_d.binned","woe.grade.binned",
                             "woe.sub_grade.binned","woe.int_rate.binned","woe.total_rec_late_fee.binned",
                             "woe.term_int.binned","woe.mts_since_last_credit_pull_d.binned",
                             "woe.revol_util.binned","woe.purpose.binned",
                             "woe.annual_inc.binned","loan_status_recode","PD")


colnames(ms_test_data) <- c("woe.last_pymnt_amnt.binned","woe.total_rec_prncp.binned",
                             "woe.total_pymnt.binned","woe.total_pymnt_inv.binned",
                             "woe.mts_since_last_pymnt_d.binned","woe.grade.binned",
                             "woe.sub_grade.binned","woe.int_rate.binned","woe.total_rec_late_fee.binned",
                             "woe.term_int.binned","woe.mts_since_last_credit_pull_d.binned",
                             "woe.revol_util.binned","woe.purpose.binned",
                             "woe.annual_inc.binned","loan_status_recode","PD")

colnames(ms_train_data)

# Calibration using Regression Method

regression_calibration <- regression.calibration(model, test_woe,"loan_status_recode")

regression_calibration$calibration_data
regression_calibration$calibration_model
regression_calibration$calibration_formula

# Creating scaled score for convenient interpretation of probabilities

scaled.score(regression_calibration$calibration_data, "calibrated_pd", 3000, 15)


             ################################### Model Validation ###################################


# A] Checking Multicollinearity in Logistic Regression Model
##   Generally values greater than 5 indicate this problem

vif.calc(model)

# B] Calculating the GINI for the model
##   Generally acceptable lower limit is 0.40 for GINI coefficient

Gini(model$fitted.values,ms_train_data$loan_status_recode)

# C] Performing the 5 Fold cross validation

k.fold.cross.validation.glm(ms_train_data,"loan_status_recode",5,1)

# D] The KS test on the distributions of good and bad observation estimates

Kolmogorov.Smirnov(ms_train_data,"loan_status_recode","PD")

Kolmogorov.Smirnov(ms_test_data,"loan_status_recode","PD")

# E] System Stability Index(SSI) to measure the model and variable stability
##   SSI values above 0.25 indicate that variable stability is impaired.

SSI.calc.data(train_woe,test_woe,"loan_status_recode")

SSI.calc.data(df,df1,"loan_status_recode") 

# F] # Calculating ROC Curve

library(ROCR)
library(ROSE)

test_woe$score <- predict(model, type="response", test_woe)

pred <- prediction(test_woe$score, test_woe$loan_status_recode)

perf <- performance(pred, "tpr","fpr")

plot(perf, main = "ROC Curve")


pred2 <- predict(model, test_woe)

accuracy.meas(test_woe$loan_status_recode, pred2)

roc.curve( test_woe$loan_status_recode, pred2, plotit = F)

# AUC - 92%
# Precision - 81.9%
# Recall - 42.9%
# F Test - 28.2 %
# KS Test - 0.67

#rm(list=ls())
