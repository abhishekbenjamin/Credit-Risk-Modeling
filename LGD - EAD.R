library(dplyr)
library(zoo)
library(stringr)


default <- bank_loan_data

default <- default %>% select(-annual_inc_joint,-collection_recovery_fee,-collections_12_mths_ex_med,
                   -delinq_2yrs,-desc,-emp_title,-id,-initial_list_status,-inq_last_6mths,
                   -member_id,-policy_code,-pymnt_plan,-title,-url,-zip_code,
                   -open_acc_6m,-open_il_6m,-open_il_12m,-open_il_24m,-mths_since_rcnt_il,
                   -total_bal_il,-il_util,-open_rv_12m,-open_rv_24m,-max_bal_bc,-all_util,
                   -inq_fi,-total_cu_tl,-inq_last_12m,-tot_coll_amt,-dti_joint,-X)

#default <- default %>% filter(loan_status =="Charged Off",loan_status == "Does not meet the credit policy. Status:Charged Off")

default <- filter (default, loan_status %in%  c("Charged Off","Does not meet the credit policy. Status:Charged Off")) 

unique(default$loan_status)

default$verification_status_joint <- NULL
default$mths_since_last_major_derog <- NULL
default$mths_since_last_record <- NULL  
default$mths_since_last_delinq <- NULL
default$application_type <- NULL
default$verification_status <- NULL
default$next_pymnt_d <- NULL

# Preprocessing Target variable: loan_status

unique(default$loan_status)

default$loan_status <- as.factor(default$loan_status)

colnames(default)

anyNA(default)

# Imputing missing values of mths_since_last_delinq & mths_since_last_record to 0

default[is.na(default)] = 0

# Tranforming categorical variables into factor

default$grade <- as.factor(default$grade)
default$sub_grade <- as.factor(default$sub_grade)
default$home_ownership <- as.factor(default$home_ownership)
default$addr_state <- as.factor(default$addr_state)
default$purpose <- as.factor(default$purpose)

# Pre-processing variable: earliest_cr_line

default$earliest_cr_line_dt <- as.Date(as.yearmon(default$earliest_cr_line, "%b-%y"))

class(default$earliest_cr_line_dt)

date_1 <- as.Date("2017-12-01")

default$mts_since_earliest_cr_line <- round((date_1 - default$earliest_cr_line_dt)/30)

default$mts_since_earliest_cr_line <- as.numeric(default$mts_since_earliest_cr_line)

class(default$mts_since_earliest_cr_line)

summary(default$mts_since_earliest_cr_line) 

default$mts_since_earliest_cr_line[default$mts_since_earliest_cr_line <0] = max(default$mts_since_earliest_cr_line, na.rm = F)

default$mts_since_earliest_cr_line[is.na(default$mts_since_earliest_cr_line)] = median(default$mts_since_earliest_cr_line,na.rm=T)

summary(default$mts_since_earliest_cr_line)

default$earliest_cr_line <- NULL
default$earliest_cr_line_dt <- NULL

# Pre-processing variable: emp_length

default$emp_length_int <- str_replace_all(default$emp_length, "years","")
default$emp_length_int <- str_replace_all(default$emp_length_int, "< 1 year","0")
default$emp_length_int <- str_replace_all(default$emp_length_int, "year","")
default$emp_length_int <- str_replace_all(default$emp_length_int, "\\+","")
default$emp_length_int[default$emp_length_int == ""] <- 0

default$emp_length_int <- as.integer(default$emp_length_int)

default$emp_length <- NULL

# Pre-processing variable: issue_d

head(default$issue_d)

default$issue_d_dt <- as.Date(as.yearmon(default$issue_d, "%b-%y"))

default$mts_since_issue_d <- round((date_1 - default$issue_d_dt)/30)

default$mts_since_issue_d <- as.numeric(default$mts_since_issue_d)

default$issue_d_dt <- NULL
default$issue_d <- NULL 

# Pre-processing: term variable

default$term_int <- str_replace_all(default$term,"months","")
default$term_int <- as.integer(default$term_int)

default$term <- NULL 

# Pre-processing last_pymnt_d

default$last_pymnt_d_dt <- as.Date(as.yearmon(default$last_pymnt_d,"%b-%y")) 

default$mts_since_last_pymnt_d <- round((date_1 - default$last_pymnt_d_dt)/30)

unique(default$mts_since_last_pymnt_d)

default$mts_since_last_pymnt_d[is.na(default$mts_since_last_pymnt_d)] <- median(default$mts_since_last_pymnt_d,na.rm = T)

default$mts_since_last_pymnt_d <- as.numeric(default$mts_since_last_pymnt_d)

summary(default$mts_since_last_pymnt_d)

default$last_pymnt_d <- NULL
default$last_pymnt_d_dt <- NULL

# Pre-processing last_credit_pull_d

default$last_credit_pull_d_dt <- as.Date(as.yearmon(default$last_credit_pull_d, "%b-%y"))

default$mts_since_last_credit_pull_d <-  round((date_1 - default$last_credit_pull_d_dt)/30)

unique(default$mts_since_last_credit_pull_d)

default$mts_since_last_credit_pull_d[is.na(default$mts_since_last_credit_pull_d)] <- median(default$mts_since_last_credit_pull_d,na.rm=T)

default$mts_since_last_credit_pull_d <- as.numeric(default$mts_since_last_credit_pull_d)

summary(default$mts_since_last_credit_pull_d)

default$last_credit_pull_d <- NULL
default$last_credit_pull_d_dt <- NULL


# Calculating recovery_rate as its a dependent variable for LGD

default$recovery_rate <- default$recoveries/default$funded_amnt


head(default$recovery_rate)

summary(default$recovery_rate)

# Capping recovery_rate between 0 and 1
# default <- default %>% mutate(recovery_rate = if_else(recovery_rate >1,1,recovery_rate))
# summary(default$recovery_rate)

# EAD = Total funded amount X Credit conversion factor(ccf)

# Calculating ccf
## Proportion from original loan that is still outstanding at the moment when
### borrower defaulted.

default$ccf <- (default$funded_amnt - default$total_rec_prncp)/default$funded_amnt

summary(default$ccf)

# Target variable transformation: recovery_rate
#default$recovery_rate <- (default$recovery_rate *(6957-1)+ 0.5)/6957    
#summary(default$recovery_rate)

# Modeling LGD
# A] Is recovery_rate equal to 0 or greater than 0 ? - Logistic Regression
# B] If recovery_rate is greater than 0 then by how much - Linear Regression
# i.e we get estimate of recovery rate for borrowers above 0


default$recovery_rate_0_1 <- if_else (default$recovery_rate == 0,0,1)

default$recovery_rate_0_1 <- as.factor(default$recovery_rate_0_1)

grep("ccf", colnames(default))

grep("recovery_rate", colnames(default))

# Creating a new data set minus ccf and recovery_rate variables

default_logit <- default[,c(-38,-37)] 

library(caret)
library(ROSE)

set.seed(123)

pd <- createDataPartition(default_logit$recovery_rate_0_1, p = .8,list = FALSE,times = 1)

train <- default_logit[pd,]

test <- default_logit[-pd,]


m <- glm(recovery_rate_0_1 ~., data = train, family = "binomial")

summary


pred <- predict(m,train, type = "response")

pred

miss <- ifelse(pred >0.5,1,0)

tab1 <- table(miss,train$recovery_rate_0_1)


# Errors
1- sum(diag(tab1))/sum(tab1) 

accuracy.meas(train$recovery_rate_0_1, pred)

roc.curve( train$recovery_rate_0_1, pred, plotit = F)

# Accuracy is 86 %
# AUC - 81%
# Precision - 88%
# Recall - 93%
# F - 0.45


# Accuracy of test data

pred2 <- predict(m,test)


miss2 <- ifelse(pred2 > 0.5,1,0)

tab2 <- table(miss2, test$recovery_rate_0_1)

tab2

1- sum(diag(tab2))/sum(tab2) 


accuracy.meas(test$recovery_rate_0_1, pred2)

roc.curve( test$recovery_rate_0_1, pred2, plotit = F)

# Accuracy - 82 % 
# AUC - 88.6%
# Precision - 85%
# Recall - 91.6%
# F - 0.44


# Fitting Linear Regression

default_linear <- default %>% filter(recovery_rate_0_1 == 1)

default_linear$ccf <- NULL # Dependant variable of EAD
default_linear$recovery_rate_0_1 <- NULL


pd2 <- createDataPartition(default_linear$recovery_rate, p = 0.8, times =1, list = FALSE)

train2 <- default_linear[pd,]
  
test2  <- default_linear[-pd,]  

tc <- trainControl(method = "repeatedcv", 
                   number = 10,
                   repeats = 3)


m2 <- lm(recovery_rate ~., data = train2, preProcess = c("center","scale"),trcontrol = tc)

summary(m2)


# Accuracy of train2

pred3 <- predict(m2,train2)

summary(pred3)

# Capping Pred3 values between 0 & 1

pred3 <- ifelse(pred3 <0,0,pred3)

pred3 <- ifelse(pred3>1,1, pred3)

miss3 <- ifelse(pred3 > 0.5,1,0)

tab3 <- table(miss3, train2$recovery_rate)

1- sum(diag(tab3))/sum(tab3) 

## Comparing predicted values with actual values

plot(test2$recovery_rate, type = "l", lty = 1.8, col = "red" )
lines(pred3, type= "l", col= "blue")


# Accuracy of test2

pred4 <- predict(m2,test2)

miss4 <- ifelse(pred4 > 0.5,1,0)

tab4 <- table(miss4, test2$recovery_rate)

1- sum(diag(tab4))/sum(tab4)

plot(test2$recovery_rate, type = "l", lty = 1.8, col = "red" )
lines(pred4, type= "l", col= "blue")


#lgd_f <- pred2*pred4

#summary(lgd_f)
#lgd_f <- ifelse(lgd_f <0,0,lgd_f)
#lgd_f <- ifelse(lgd_f>1,1,lgd_f)


LGD_final <- pred*pred3

summary(LGD_final)

LGD_final <- ifelse (LGD_final<0,0, LGD_final)

LGD_final <- ifelse (LGD_final>1,1, LGD_final)

summary(LGD_final)

LGD_final <- as.data.frame(LGD_final)

head(LGD_final)


# EAD = Total funded amount X Credit conversion factor(ccf)

# Calculating ccf
## Proportion from original loan that is still outstanding at the moment when
### borrower defaulted.

default$ccf <- (default$funded_amnt - default$total_rec_prncp)/default$funded_amnt

hist(default$ccf)                  

# Creating a data set minus recovery_rate & recovery_rate_0_1

default_EAD <- default[,-c(37,39)]

set.seed(123)

pd3 <- createDataPartition(default_EAD$ccf, p = .8,list = FALSE,times = 1)

train3 <- default_EAD[pd3,]
test3 <- default_EAD[-pd3,]

m3 <- lm(ccf~., data=train3, preprocess = c("center","scale"))

#m3 <- lm(ccf~.,data = train3[,!colnames(train2) %in% c("addr_state")])   

summary(m3)



pred5 <- predict(m3,test3)

summary(pred5)

# Capping values of Pred5 between 0 & 1

pred5 <- ifelse(pred5 <0,0,pred5)
pred5 <- ifelse(pred5 >1,1,pred5)

summary(pred5)

miss5 <- table(pred5, test3$ccf)

miss5

1- sum(diag(miss5))/sum(miss5) 


# Coefficient tells us that with each %age increase in a variable ccf increase by coefficient value
# Like each %age increase in loan_amt, ccf would be about -1.686 times lower(low risk)

m3$coefficients

plot(test3$ccf, type = "l", lty = 1.8, col = "red" )
lines(pred5, type= "l", col= "blue")


rm(list=ls())





