RNGversion(vstr = '3.6.1')
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(caret)
library(randomForest)
library(e1071)
library(ROCR)
library(caTools)
library(lm.beta)
library(rpart)
library(rpart.plot)
library(corrplot)
getwd()
setwd('/Users/shashank/Desktop/Frameworks/')
data = read.csv('versionA.csv')
set.seed(617)
split = sample(1:nrow(data), nrow(data)*0.75)
train = data[split,]
test = data[-split,] 

#Q2: How many observations are in the test sample?
 nrow(test)
#ans: 10000
 
#Q3: What is the class for “carat”?
 class(data$carat)
#ans: numeric
 
#Q4: What is the minimum price for diamonds of carat size greater than 2 and Fair cut? Use the train sample
 subset1 = train[train$carat > 2,]
 subset2 = subset1[subset1$cut == 'Fair',]
 min(subset2$price)
#ans: 5405 
 
#Q5:  Construct a scatter plot of carat (on the x-axis) and price (on the y-axis). Use the train sample.
      #What is the direction (or trend) of the points?
      #(Hint: Draw a linear regression line through the points to be sure of the direction.)
 ggplot(data = train, aes(x= carat, y =price)) + geom_point() + geom_smooth(method = 'lm')

#ans: bottom left to top right 
 
#Q6: What is the correlation between price and y? Use the train sample.
 cor(train$price, train$y)
#ans:0.8513644
 
#Q7: Build a linear regression model to predict price using y. Call this model1. Use the train sample.
    #What is the model R2 (R-squared)?
  model1 = lm(price~y, data = train)  
summary(model1)$r.squared   
#ans:0.7248213 

#Q8: Which of the following is the correct interpretation for the coefficient of y in model1?
paste('price','=',round(coef(model1)[1],0),'+',round(coef(model1)[2],0),'y')
#ans: For a 1mm increase in y, price goes up by $2920.05

#Q9: Build a linear regression model to predict price using cut. Call this model2. Use the train sample. 
#Based on the results of the regression, which of the following statements is true?
  levels(train$cut)
  model2 = lm(price~cut, data = train)  
  summary(model2)
#ans: Price of Ideal cut diamonds is significantly less than the price of Fair cut diamonds
  
#Q10: Build a linear regression model to predict price using all variables EXCEPT price_hilo, x, y, and z. Call this model3. Use the train sample.
  #What is the R2 (R-squared) for model3?  
  model3 = lm(price~carat + cut+ color + clarity + depth + table, data = train)
  summary(model3)$r.squared
  #ans: 0.916861
  
#Q11: Based on model3, which of the following variables have a significant effect on price? (Check all that apply)  
#ans: carat, cut, color, clarity, depth, table
  
#Q12: Based on model3, what is the predicted price for the fifth diamond in the train sample?
  train[5,]
  pred3 = predict(model3)
  pred3[5]
#ans: 1491.826
  
#Q13: Based on model3, what is the rmse (root mean squared error) for the test sample?
  pred3_test = predict(model3, newdata = test)
  rmse3_test = sqrt(mean((pred3_test-test$price)^2)); rmse3_test
#ans: 1172.86  
  
#Q14: Now, construct a linear regression model to predict price using the following predictors: carat, depth, table, x, y, z. Use the train sample. Call this model4.
  #Which predictor has the lowest Variance Inflation Factor (VIF)?  
  model4 = lm(price~carat+depth+table+x+y+z, data = train)
  vif(model4) 
  sort(vif(model4), decreasing = TRUE)
#ans: table
  
#Q15: Run a hybrid stepwise regression using the same predictors as in model4.
  #Which of the following predictors are included in this model? 
  start_mod=lm(price~1,data=train)
  empty_mod=lm(price~1,data=train)
  full_mod=lm(price~carat+depth+table+x+y+z,data=train)
  hybridstepwise=step(start_mod,scope=list(upper=full_mod,lower=empty_mod),direction='both')
  summary(hybridstepwise)
  
  #ans: all except y

#Q16: Now, construct a logistic regression model to predict price_hilo using the following predictors: carat, cut, color, clarity, depth, and table. Use the train sample. Call this model5. For the purpose of the exam, ignore the warning message from running this logistic regression model.
  #What is the AIC for model5?
  set.seed(617)
  model5=glm(price_hilo~carat+cut+color+clarity+table,data=train,family='binomial')
  summary(model5)$aic
  summary(model5)
  #ans: 3952.722
  
  
#Q17: This question is based on model5.
#Compared to a Fair cut diamond, how much more likely is a Good cut diamond to have a high price? 
  exp(model5$coefficients[3])
  100*(exp(summary(model5)$coef[3])-1)  
  #ans: 508%
  
#Q18: What is the accuracy of model5 in the test sample? Use a threshold of 0.42
  pred_test5=predict(model5,newdata=test,type='response')
  ct=table(price_hilo=test$price_hilo,
           predictions=as.integer(pred_test5>0.42));ct
  accuracy=sum(ct[1,1],ct[2,2])/nrow(test);accuracy
  #ans: 0.977
  
#Q19: Construct a classification tree (NOT regression tree) to predict price_hilo using the following predictors: carat, cut, color, clarity, depth and table. Use the train sample. Do not specify cp, or minbucket, and do not cross-validate. Call this model6.
  #Which is the most important predictor of price_hilo?  
  model6=rpart(price_hilo~carat+cut+color+clarity+depth+table,data=train, method='class')
  summary(model6)
  rpart.plot(model6)
  #ans: carat
  
#Q20: Based on model6, what is the chance of a 0.8 carat diamond having a High price?
  #ans:1%
  
  
#21: Based on model6, what is the chance of a 1 carat diamond having a High price?
  #ans: 94%
  
#22:Construct a random forest model to predict price_hilo with the same predictors used in model6: carat, cut, color, clarity, depth and table. Use factor(price_hilo) as outcome instead of price_hilo. Use the train sample. Set number of trees to 125 and use a seed of 3110. Do not specify mtry. Do not tune or cross-validate. Call this model7.
  #In model7, which is the second most important predictor of price_hilo?
  set.seed(3110)
  model7=randomForest(factor(price_hilo)~carat+cut+color+clarity+depth+table,data=train,ntree=125)
  varImpPlot(model7)
  importance(model7)
  
  #ans: clarity

#23: For model7, what is the Area under the ROC curve (AUC) in the test sample? 
  pred_forest=predict(model7,newdata=test,type="prob")[,2]
  ROCRforest=prediction(pred_forest,test$price_hilo)
  auc_forest=as.numeric(performance(ROCRforest,"auc")@y.values)
  auc_forest
  
  #ans: 0.9966595
  
#24: Use a Support Vector Machine to predict price_hilo using the same predictors as in model7: carat, cut, color, clarity, depth and table. Use factor(price_hilo) as outcome instead of price_hilo. Use the train sample. Use a radial basis function kernel. Do not set cost, gamma or coef0, in other words, use defaults. Do not tune or cross-validate. Call this model8.
  #Based on model8, how many diamonds in the train sample were predicted to be "low" priced and were "low" priced?  
  
  model8=svm(factor(price_hilo)~carat+cut+color+clarity+depth+table,data=train,
             kernel='radial',type='C-classification')
  pred8=predict(model8)
  table(pred8,train$price_hilo)

  #ans: 18593
  
#25:What is the accuracy of model8 in the test sample?  
  pred_test8 = predict(model8,newdata=test)
  table(pred_test8,test$price_hilo)
  accuracy = mean(pred_test8==test$price_hilo)
  accuracy
  #ans: 0.9692
  
 