setwd('D:/CS498/HW6 - REGRESSIONTWO/')

#problem 1
raw_data<-read.csv('default_plus_chromatic_features_1059_tracks.txt', header=FALSE,sep=',')
str(raw_data)

#part a
raw_data[,c(117,118)] #latitude and longitude
latitude_reg <- lm(V117 ~ . - V118, data=raw_data)
summary(latitude_reg) #R^2 is in the summary
plot(latitude_reg) #residuals vs fitted

longitude_reg <- lm(V118 ~ . - V117, data=raw_data)
summary(longitude_reg) #R^2
plot(longitude_reg)

#part b box-cox stuff
#make lat and long positive by adding to them to make the latitude start at 0 at south pole and longitude start furthest east
positive_data <- raw_data
positive_data[,117] <- raw_data[,117]+180
positive_data[,118] <- raw_data[,118]+180

box_latitude_reg <- latitude_reg <- lm(V117 ~ . - V118, data=positive_data)
box_longitude_reg <- longitude_reg <- lm(V118 ~ . - V117, data=positive_data)


library(MASS)
box_latitude <- boxcox(V117 ~ . - V118, data=positive_data)
#box_latitude <- boxcox(V117 ~ . - V118, data=positive_data, lambda=c(1,4)) #if the optimal value includes lambda = 1 then the boxcox transformation is not needed
box_longiude <- boxcox(V118 ~ . - V117, data=positive_data)

boxcox(V117^2 ~ . - V118,data=positive_data) #latitude has a lambda of 2 that makes it better

new_latitude_reg <- lm(V117^2 ~ . - V118, data=raw_data) #lambda of 2 makes it better
summary(new_latitude_reg)  #bigger R^2 is better
plot(new_latitude_reg)
#boxcox is helpful for latitude but not longitude

#part c glmnet stuff
#ridge regressions
library(glmnet)
glm_data <- as.matrix(raw_data)
glm_data
latitude_glm <- cv.glmnet(y=glm_data[,117]^2,x=glm_data[,-c(117,118)], alpha=0)
plot(latitude_glm) #minimum error at about 20 for ^6 or 5 for ^2
reg_lambda = latitude_glm$lambda.min
com_model = cv.glmnet(y=glm_data[,117]^2,x=glm_data[,-c(117,118)], alpha=0,lambda=c(reg_lambda,0))
com_model$cvm
lat_vars = latitude_glm$glmnet.fit$df[which.min(com_model$cvm)]
lat_vars
longitude_glm <- cv.glmnet(y=glm_data[,118],x=glm_data[,-c(117,118)], alpha=0)
plot(longitude_glm) #minimum error at about 2
reg_lambda = longitude_glm$lambda.min
com_model = cv.glmnet(y=glm_data[,118]^2,x=glm_data[,-c(117,118)], alpha=0,lambda=c(reg_lambda,0))
com_model$cvm
lat_vars = longitude_glm$glmnet.fit$df[which.min(com_model$cvm)]
lat_vars

#lasso regressions
latitude_glm_2 <- cv.glmnet(y=glm_data[,117]^2,x=glm_data[,-c(117,118)], alpha=1,family="gaussian")
plot(latitude_glm_2) #minimum error at about 18 for ^6 or 2 at ^2
reg_lambda_2 = latitude_glm_2$lambda.min
com_model_2 = cv.glmnet(y=glm_data[,117]^2,x=glm_data[,-c(117,118)], alpha=1,lambda=c(reg_lambda_2,0),family="gaussian")
com_model_2$cvm
lat_vars_2 = latitude_glm_2$glmnet.fit$df[which.min(com_model_2$cvm)]
lat_vars_2

longitude_glm_2 <- cv.glmnet(y=glm_data[,118],x=glm_data[,-c(117,118)], alpha=1,family="gaussian")
plot(longitude_glm_2) #minimum error at about -1
reg_lambda = longitude_glm_2$lambda.min
com_model = cv.glmnet(y=glm_data[,118],x=glm_data[,-c(117,118)], alpha=1,lambda=c(reg_lambda,0),family="gaussian")
com_model$cvm
lat_vars = longitude_glm_2$glmnet.fit$df[which.min(com_model$cvm)]
lat_vars

#elastic net regression
latitude_glm_3 <- cv.glmnet(y=glm_data[,117]^2,x=glm_data[,-c(117,118)], alpha=.5)
latitude_glm_3$lambda.min
reg_lambda = latitude_glm_3$lambda.min
com_model = cv.glmnet(y=glm_data[,117]^2,x=glm_data[,-c(117,118)], alpha=.5,lambda=c(reg_lambda,0))
com_model$cvm
plot(latitude_glm_3) #minimum error at about 19 or 2 for ^2
longitude_glm_3 <- cv.glmnet(y=glm_data[,118],x=glm_data[,-c(117,118)], alpha=.5)
plot(longitude_glm_3) #minimum error at about 0
reg_lambda = longitude_glm_3$lambda.min
com_model = cv.glmnet(y=glm_data[,118],x=glm_data[,-c(117,118)], alpha=.5,lambda=c(reg_lambda,0))
com_model$cvm

latitude_glm_3 <- cv.glmnet(y=glm_data[,117]^2,x=glm_data[,-c(117,118)], alpha=.25)
latitude_glm_3$lambda.min
plot(latitude_glm_3) 
reg_lambda = latitude_glm_3$lambda.min
com_model = cv.glmnet(y=glm_data[,117]^2,x=glm_data[,-c(117,118)], alpha=.5,lambda=c(reg_lambda,0))
com_model$cvm
longitude_glm_3 <- cv.glmnet(y=glm_data[,118],x=glm_data[,-c(117,118)], alpha=.25)
plot(longitude_glm_3) 
reg_lambda = longitude_glm_3$lambda.min
com_model = cv.glmnet(y=glm_data[,118],x=glm_data[,-c(117,118)], alpha=.5,lambda=c(reg_lambda,0))
com_model$cvm

latitude_glm_3 <- cv.glmnet(y=glm_data[,117]^2,x=glm_data[,-c(117,118)], alpha=.75)
latitude_glm_3$lambda.min
plot(latitude_glm_3) 
reg_lambda = latitude_glm_3$lambda.min
com_model = cv.glmnet(y=glm_data[,117]^2,x=glm_data[,-c(117,118)], alpha=.5,lambda=c(reg_lambda,0))
com_model$cvm
longitude_glm_3 <- cv.glmnet(y=glm_data[,118],x=glm_data[,-c(117,118)], alpha=.75)
plot(longitude_glm_3) 
reg_lambda = longitude_glm_3$lambda.min
com_model = cv.glmnet(y=glm_data[,118],x=glm_data[,-c(117,118)], alpha=.5,lambda=c(reg_lambda,0))
com_model$cvm


#problem 2
raw_data_2 <-read.csv('credit_card_client_data.txt', header=TRUE,sep="\t")
str(raw_data_2)
#https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/


library(caret)
data_partition <- createDataPartition(y=raw_data_2[,24], p=.8, list=FALSE) #.8 for training
train_data <- raw_data_2[data_partition,]
test_data <- raw_data_2[-data_partition,]

logistic_reg <- glm(Y ~ ., data=train_data,family=binomial)
summary(logistic_reg)
plot(logistic_reg)
library(pscl)
pR2(logistic_reg) #McFadden R^2 is similar to linear regression R^2 which in this case is .117

predict_reg <- predict(logistic_reg,newdata = test_data, type = 'response')
table(test_data[,24], predict_reg > 0.5) #confusion matrix
library(ROCR)
ROCRpd <- prediction(predict_reg, test_data[,24])
ROCRpf <- performance(ROCRpd, 'tpr','fpr')
plot(ROCRpf, colorize = TRUE, text.adj = c(-0.2,1.7))

auc <- performance(ROCRpd, measure = "auc")
auc@y.values[[1]] #.735 for this train test set, pretty good
#a good predictive value is closer to 1 for auc


glmnet_data <- as.matrix(raw_data_2)
ridge_logistics <- cv.glmnet(y=glmnet_data[,24],x=glmnet_data[,-c(24)],alpha=0)
ridge_logistics$lambda.min
plot(ridge_logistics)
reg_lambda = ridge_logistics$lambda.min
com_model = cv.glmnet(y=glmnet_data[,24],x=glmnet_data[,-c(24)], alpha=0,lambda=c(reg_lambda,0))
com_model$cvm

lasso_logistics <- cv.glmnet(y=glmnet_data[,24],x=glmnet_data[,-c(24)],alpha=1)
plot(lasso_logistics)
reg_lambda = lasso_logistics$lambda.min
com_model = cv.glmnet(y=glmnet_data[,24],x=glmnet_data[,-c(24)], alpha=1,lambda=c(reg_lambda,0))
com_model$cvm

enet_logistics <- cv.glmnet(y=glmnet_data[,24],x=glmnet_data[,-c(24)],alpha=0.5)
plot(enet_logistics)
reg_lambda = enet_logistics$lambda.min
com_model = cv.glmnet(y=glmnet_data[,24],x=glmnet_data[,-c(24)], alpha=0.5,lambda=c(reg_lambda,0))
com_model$cvm

#train/test split for problem 2

