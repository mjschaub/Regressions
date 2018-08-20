setwd('D:/CS498/HW5 - REGRESSION/')


#problem 7.9
raw_data<-read.csv('data_7_9.txt', header=TRUE,sep='\t')
str(raw_data)

# a) Prepare a plot showing (a) the data points and (b) the regression line in log-log coordinates
log_log_lm <- lm(log(Sulfate) ~ log(Hours),data=raw_data)
log_log_lm
plot(log(Sulfate) ~ log(Hours),raw_data,main="log-log plot")
abline(lm(log(Sulfate) ~ log(Hours),raw_data))


regular_lm <- lm(Sulfate ~ Hours,data=raw_data)
plot(Sulfate ~ Hours,raw_data,main="original coordinates plot")
abline(regular_lm)



plot(exp(log(Sulfate)) ~ exp(log(Hours)),raw_data,main="original coordinates plot")
#lines(raw_data[,1],predict(log_log_lm))
#matlines(raw_data[,1],predict(log_log_lm),lwd=2)
#lines(predict(log_log_lm))
abline(lm(exp(log(Sulfate)) ~ exp(log(Hours)),raw_data))


plot(log_log_lm) #residuals vs fitted for log-log
raw_data[,2]
new_res_sulfate <- raw_data[,2] - exp(log_log_lm$fitted.values)
plot(regular_lm) #residuals vs fitted for regular
plot(y=new_res_sulfate,x=log_log_lm$fitted.values,main="residuals vs Fitted for original coordinates of log-log",xlab="fitted values",ylab="residuals in original coordinates")

#problem 7.10
raw_data_2 <- read.csv('data_7_10.txt',header=TRUE,sep='\t')
raw_data_2
raw_data_2[,1]
mass_vs_diameters_lm <- lm(Mass ~ Fore+Bicep+Chest+Neck+Shoulder+Waist+Height+Calf+Thigh+Head,raw_data_2)
plot(mass_vs_diameters_lm)

cube_root_lm <- lm((Mass)^(1/3) ~ Fore+Bicep+Chest+Neck+Shoulder+Waist+Height+Calf+Thigh+Head,raw_data_2)
plot(cube_root_lm)
new_res <- raw_data_2[,1]-(cube_root_lm$fitted.values)^3
plot(y=new_res,x=cube_root_lm$fitted.values, main="residuals vs Fitted for original coordinates of cube root",xlab="cube root fitted.values", ylab="residuals in original coordinates")

#problem 7.11
raw_data_3 <- read.csv('data_7_11.txt', header=FALSE,sep=',')
raw_data_3

predict_age_ignore_gender <- lm(V9 ~ V2+V3+V4+V5+V6+V7+V8,raw_data_3)
summary(predict_age_ignore_gender)
plot(predict_age_ignore_gender)

#use -1,0,1 for genders
levels(raw_data_3[,1])
gender_data <- raw_data_3
gender_data[,1] <- unclass(gender_data[,1])-2
predict_age_with_gender <- lm(V9 ~ .,gender_data)
plot(predict_age_with_gender)

predict_log_age_ignore_gender <- lm(log(V9) ~ V2+V3+V4+V5+V6+V7+V8,raw_data_3)
plot(predict_log_age_ignore_gender)
raw_data_3[,9]
new_res_2 <- raw_data_3[,9]-exp(predict_log_age_ignore_gender$fitted.values)
plot(y=new_res_2,x=predict_log_age_ignore_gender$fitted.values, main="residuals vs Fitted for original coordinates of log of ignored gender",xlab="fitted.values", ylab="residuals in original coordinates")

predict_log_age_with_gender <- lm(log(V9) ~ .,gender_data)
plot(predict_log_age_with_gender)
new_res_3 <- raw_data_3[,9]-exp(predict_log_age_with_gender$fitted.values)
plot(y=new_res_3,x=predict_log_age_with_gender$fitted.values, main="residuals vs Fitted for original coordinates of log with gender",xlab="fitted.values", ylab="residuals in original coordinates")

#glmnet to fix these regressions by using a regularizer
library(glmnet)
x_data_without_gender <- as.matrix(raw_data_3[,-c(1,9)])
x_data_without_gender
x_data_with_gender <- as.matrix(gender_data[,-c(9)])
new_gender_data <- gender_data
new_gender_data[,c(1)] <- as.numeric(gender_data[,c(1)])
as.matrix(new_gender_data[,-c(9)])
x_data_with_gender
glmnet_regression_without_gender <- glmnet(x=x_data_without_gender,y=raw_data_3[,9],alpha=1)
summary(glmnet_regression_without_gender)
cross_validation_with_gender <- cv.glmnet(x=as.matrix(new_gender_data[,-c(9)]),y=raw_data_3[,9],alpha=0) #ridge regression for gender included data
plot(cross_validation_with_gender)
cross_validation <- cv.glmnet(x=x_data_without_gender,y=raw_data_3[,9],alpha=0) #ridge regression without gender data
plot(cross_validation)
cross_validation$lambda.min
cross_validation_with_gender$lambda.min


cross_validation_with_gender_lasso <- cv.glmnet(x=as.matrix(new_gender_data[,-c(9)]),y=raw_data_3[,9],alpha=1) #lasso regression for gender included data
plot(cross_validation_with_gender_lasso)
cross_validation_lasso <- cv.glmnet(x=x_data_without_gender,y=raw_data_3[,9],alpha=1) #lasso regression without gender data
plot(cross_validation_lasso)
cross_validation_lasso$lambda.min
cross_validation_with_gender_lasso$lambda.min


#cv.glmnet to see the cross validation
#https://drsimonj.svbtle.com/ridge-regression-with-glmnet



#Sex / nominal / -- / M, F, and I (infant)
#Length / continuous / mm / Longest shell measurement
#Diameter / continuous / mm / perpendicular to length
#Height / continuous / mm / with meat in shell
#Whole weight / continuous / grams / whole abalone
#Shucked weight / continuous / grams / weight of meat
#Viscera weight / continuous / grams / gut weight (after bleeding)
#Shell weight / continuous / grams / after being dried
#Rings / integer / -- / +1.5 gives the age in years 


