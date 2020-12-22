
#LOADING THE DATA AND CHECKING FOR MISSING VALUES
bank = read.csv(file.choose())


#bank$age = as.numeric(bank$age)
bank$job = factor(bank$job)
bank$marital = factor(bank$marital)
bank$education = factor(bank$education)
bank$default = factor(bank$default)
#bank$balance = as.numeric(bank$balance)
bank$housing.loan = factor(bank$housing.loan)
bank$personal.loan = factor(bank$personal.loan)
#bank$current.campaign = as.numeric(bank$current.campaign)
#bank$previous.campaign = as.numeric(bank$previous.campaign)
bank$subscribed_036 = factor(bank$subscribed_036)
str(bank)

summary(bank$subscribed_036)
library(gmodels)
CrossTable(bank$subscribed_036)
#From CrossTable we can see that there are 88.5% No values and 11.5% yes values. Hence, we can say that the people who have not subscribed for term loan is more than those who have subscribed for it. 

#Creating a binary dependent varaible for potential regression models.
library(tidyverse)
bank = bank %>%
  mutate(subscribed_36_bin = ifelse(subscribed_036 == "no",0,1))

hist(bank$subscribed_36_bin)
summary(bank$subscribed_36_bin)
#The histogram of target variable also shows that the values of No is more than that of Yes.


#CHECKING FOR MISSING VALUES
colSums(is.na(bank))
colSums(bank == "")
colSums(bank == "unknown")
# 2 variables have unknown values. There are 38 missing values in Job and 187 missing values in Education. The missing values here are encoded as 'unknown'.
#Before deciding how to manage those missing values, we'll study each variable and take a decision after visualisations.

#EXPLORATORY DATA ANALYSIS
library(ggplot2)
#age
summary(bank$age)
bank %>% 
  ggplot() +
  aes(x = age) +
  geom_bar() +
  geom_vline(xintercept = c(30, 60), 
             col = "red",
             linetype = "dashed") +
  facet_grid(subscribed_036 ~ .,
             scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 100, 5))
#We can see after the 60-years threshold, the relative frequency is higher when y = 1. In other words, we can say that elderly persons are more likely to subscribe to a term deposit.

#We can also slice the age feature at 30 years to make three easily interpretable classes : [0,30],[30,60][60+]

#The minimum and maximum values are 19 and 87 but we can expect new observations outside this range.
#We're replacing the continious variable "age" by this categorical variable. We might lose some information from this continious-to-discrete transformation, but there wasn't any clear pattern between years.

bank = bank %>% 
  mutate(age_discrete = if_else(age > 60, "high", if_else(age > 30, "mid", "low")))

#Cross-tab with our dependent variable

CrossTable(bank$age_discrete, bank$subscribed_036)
#37.8% of people over 60 years old subscribed to a term deposit, which is a lot more than younger individuals, 14.2% for adults aged less than 30 and only 10.2% for the remaining observations have subscibed.

#jobs
library(ggmosaic)
bank %>% 
  ggplot() +
  geom_mosaic(aes(x = product(subscribed_036, job), fill = subscribed_036)) +
  xlab("Jobs") +
  ylab(NULL)
#We can say that the retired people and students are more who says yes for subscription.

table(bank$job)
CrossTable(bank$job, bank$subscribed_036)
#Here we can see that 18.4% of people whose jobs are unknown or missing has subscribed yes. This can also be an indication that we simply cannot remove the missing variables here.
#22.6% of students and 23.5% of retired people are those who subscribe yes for term deposit.

#marital status
table(bank$marital)
CrossTable(bank$marital, bank$subscribed_036)
bank %>% 
  ggplot() +
  geom_mosaic(aes(x = product(subscribed_036, marital), fill = subscribed_036)) +
  xlab("Marital Status") +
  ylab(NULL)

#We see that divorced and single people are more who have subscribed for term deposit.
#14.6% of divorced people and 14% single have subscribed yes.

#Education
CrossTable(bank$education, bank$subscribed_036)
bank %>% 
  ggplot() +
  geom_mosaic(aes(x = product(subscribed_036, education), fill = subscribed_036)) +
  xlab("Education Level") +
  ylab(NULL)
#Here we see that more teritary level of people have subscribed yes as compared to other levels.
#14.3% of people with teritary level of education has said yes for subscription

chisq.test(bank$education, bank$subscribed_036)
#Here our pvalue <0.05, hence we can say that the variable is required for further analysis.

#Default
table(bank$default)
CrossTable(bank$default, bank$subscribed_036)
chisq.test(bank$default, bank$subscribed_036)
#the p-value for default is 1 which means we can remove default from our model.
#We can also see that the % of people who has default yes or no and has subscribed for term depost is 11%, So we can say that there is no effect of default on subscription


#Housing loan
table(bank$housing)
CrossTable(bank$housing.loan, bank$subscribed_036)
chisq.test(bank$housing.loan, bank$subscribed_036)
#our p-value is less than 0.05, hence it is significant.
#15.3% of people who do not have housing loan has subscribed yes, and 8.6%of people who have housing loan has said yes.

#personal loan
table(bank$personal.loan)
CrossTable(bank$personal.loan, bank$subscribed_036)
chisq.test(bank$personal.loan, bank$subscribed_036)
#p-value<0.05 so we cannot remove this variable
#12.5% of people who donot have personal loan has subscribed yes and 6% of those who has personal loan has said yes.

#Current campaign
bank = bank %>%   
  filter(current.campaign <= 10) 

bank %>% 
  ggplot() +
  aes(x = current.campaign) +
  geom_bar() +
  facet_grid(subscribed_036 ~ .,
             scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 10, 1))
CrossTable(bank$current.campaign, bank$subscribed_036)
bank %>% 
  ggplot() +
  geom_mosaic(aes(x = product(subscribed_036, current.campaign), fill = subscribed_036)) +
  xlab("Current ampaign") +
  ylab(NULL)
#current campaign with 1 and 4 has 13% of people who has said yes for term deposit


#previous camapign
table(bank$previous.campaign)
CrossTable(bank$previous.campaign, bank$subscribed_036)
bank = bank %>% 
  mutate(previous.campaign_binned = if_else(previous.campaign >=  2, "2+", if_else(previous.campaign == 1, "1", "0")))

#Cross-tab on binned variable with our dependent variable:

CrossTable(bank$previous.campaign_binned, bank$subscribed_036)
#25% of people with 2+ campaign has subscribed yes.

str(bank)
library(corrplot)
bank %>% 
  select(age, balance, current.campaign, previous.campaign) %>% 
  cor() %>% 
  corrplot(method = "number",
           type = "upper",
           tl.cex = 0.8,
           tl.srt = 45,
           tl.col = "black")
#Here we see that there is no variable which is highly correlated so we can consider all the varibles.

# FINAL DATA PREPARATION
#backup
bank_backup = bank
#replacing the missing values
bank_backup = bank_backup %>% 
  mutate(education = recode(education, "unknown" = "primary"))

bank_backup = bank_backup %>% 
  mutate(job = recode(job, "unknown" = "unemployed"))


#Converting our variables to factors with ordered levels (ordinal variables)


#Splitting the data into training and test datasets (80-20 split):
library(caTools)
set.seed(3236)
split = sample.split(bank_backup$subscribed_36_bin,SplitRatio = 0.80)
training = subset(bank_backup, split == TRUE)
testing = subset(bank_backup, split == FALSE)

#LOGISTIC REGRESSION
lr <- glm(subscribed_36_bin ~ age_discrete + job + marital + education + balance + housing.loan + personal.loan + current.campaign + previous.campaign_binned,family=binomial(link='logit'),data=training)
summary(lr)
lr2=step(lr)
summary(lr2)

#subscribed_36_bin = -0.59211 -1.00447*age_discrete(low) -1.21189*age_discrete(mid) -0.40315*marital(married) -0.11895*marital(single) + 0.25359*education(secondary) + 0.47861*education(tertiary) -0.73258*housing.loan(yes) -0.63062*personal.loan(yes)  -0.05967*current.campaign + 0.74742*previous.campaign_binned(1) + 1.19852*previous.campaign_binned(2+)

pred=predict(lr2,newdata=testing)
library(ROCR)
ROCRpred = prediction(pred, testing[,11])
ROCRperf = performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize=T, main= "ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
abline(a=0,b=1)
auc = performance(ROCRpred,measure = "auc")
auc = auc@y.values[[1]]
auc
#area under ROC curve shows 67.55% accuracy of our Logistic regression model

#building misclassification at 50%
pred=ifelse(pred>0.5,2,1)
train_tab = table(pred,testing$subscribed_036)
train_tab
mmce <- 1 - (sum(diag(train_tab))/sum(train_tab))
mmce
#we can see that the misclassification error is 11.61%

#DECISION TREE
library(rpart)
cartfit = rpart(subscribed_36_bin ~ age_discrete + job + marital + education + balance + housing.loan + personal.loan + current.campaign + previous.campaign_binned,data=training)
library(rpart.plot)
rpart.plot(cartfit)

pred=predict(cartfit,testing)
ROCRpred = prediction(pred, testing[,11])
ROCRperf = performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize=T, main= "ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
abline(a=0,b=1)
auc = performance(ROCRpred,measure = "auc")
auc = auc@y.values[[1]]
auc
#we can see that we get 62.13% accuracy for Decision Tree model
pred=ifelse(pred>0.5,2,1)
train_tab = table(pred,testing$subscribed_036)
train_tab
mmce <- 1 - (sum(diag(train_tab))/sum(train_tab))
mmce
#Misclassification error for decision tree is 11.73%

#As of now we can see Logistic Regression gives us a better model as compared to Decision tree model. The accuracy of Logistic regression is more than Decision Tree model and the misclassification is also slightly low. The misclassification error is almost similat, i.e. 11.7%

#RANDOM FOREST MODEL
library(randomForest)
str(training)
training$age_discrete=factor(training$age_discrete)
training$previous.campaign_binned=factor(training$previous.campaign_binned)
rf=randomForest(subscribed_036 ~ age_discrete + job + marital + education + balance + housing.loan + personal.loan + current.campaign + previous.campaign_binned,data=training)
varImpPlot(rf)
importance(rf)
#According to Random forest we can say that balance has more significance followed by job, current.campaign and so on
testing$age_discrete=factor(testing$age_discrete)
testing$previous.campaign_binned=factor(testing$previous.campaign_binned)
pred=predict(rf,testing,type='prob')
ROCRpred = prediction(pred[,2], testing[,11])
ROCRperf = performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize=T, main= "ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
auc = performance(ROCRpred,measure = "auc")
auc = auc@y.values[[1]]
auc
#Accuracy of random forest is 66.68%
pred=ifelse(pred[,2]>0.5,2,1)
train_tab = table(pred,testing$subscribed_036)
train_tab
mmce <- 1 - (sum(diag(train_tab))/sum(train_tab))
mmce
#misclassification error is 11.61%

#The misclassification error is same but the accuracy of Logistic is 1% more than Random forest.

#According to the models we can see that Logistic regression gives the most accuracy and the misclassification error for all is more than 11.5% and less than 12%.

