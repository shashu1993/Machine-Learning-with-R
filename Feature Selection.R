house = read.csv("/Users/shashank/Desktop/Frameworks/houses.csv")

library(caret)
library(tidyr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(car)
library(glmnet)


#Q1
set.seed(1031)
split = createDataPartition(y = house$price, p = 0.7, groups = 100, list =F)
train = house[split, ]
test = house[-split, ]

mean(train$price)

#Q2
cor(train[,-17])
round(cor(train[,-17]), 2)*100


corMatrix = as.data.frame(cor(train[,-17]))
corMatrix$var1 = rownames(corMatrix)

corMatrix %>%
  gather(key=var2,value=r,1:16)%>%
  arrange(var1,desc(var2))%>%
  ggplot(aes(x=var1,y=reorder(var2, order(var2,decreasing=F)),fill=r))+
  geom_tile()+
  geom_text(aes(label=round(r,2)),size=3)+
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#a6d96a','#1a9641'))+
  theme(axis.text.x=element_text(angle=75,hjust = 1))+xlab('')+ylab('')


#Q3

corrplot(cor(train[,c(3:7, 10:13,16)]),method = 'square',type = 'lower',diag = F)

#Q4

sum_sqft = train$sqft_above + train$sqft_basement
cor(sum_sqft, train$sqft_living)

#Q5

model1 = lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age, data = train)
summary(model1)
vif(model1)


#part2 
#Q1

library(leaps)
subsets = regsubsets(price~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + age, data = train, nvmax = 6)
summary(subsets)

#Q2
names(summary(subsets))
subsets_measures = data.frame(model=1:length(summary(subsets)$cp),
                              cp=summary(subsets)$cp,
                              bic=summary(subsets)$bic,
                              rss=summary(subsets)$rss,
                              rsq=summary(subsets)$rsq,
                              adjr2=summary(subsets)$adjr2)
subsets_measures

#Q3
start_mod = lm(price~1,data=train)
empty_mod = lm(price~1,data=train)
full_mod = lm(price~bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + age,data=train)
forwardStepwise = step(start_mod,
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction='forward')
summary(forwardStepwise)
length(coefficients(forwardStepwise))-1

#Q4
start_mod2 = lm(price~., data=train)
empty_mod2 = lm(price~1,data=train)
full_mod2 = lm(price~bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + age,data=train)
backwardStepwise = step(full_mod2,
                        scope=list(upper=start_mod2,lower=empty_mod2),
                        direction='backward')
summary(backwardStepwise)


#Q5
start_mod = lm(price~1,data=train)
empty_mod = lm(price~1,data=train)
full_mod2 = lm(price~bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + age,data=train)
hybridStepwise = step(start_mod,
                      scope=list(upper=full_mod,lower=empty_mod),
                      direction='both')
summary(hybridStepwise)

#Q6
x = model.matrix(price~.-1,data=train)
y = train$price
lassoModel = glmnet(x,y, alpha=1) 
plot(lassoModel,xvar='lambda',label=T)
set.seed(1031)
cv.lasso = cv.glmnet(x,y,alpha=1) # 10-fold cross-validation
plot(cv.lasso)
coef(cv.lasso)

#Q7
model1 = lm(price~bathrooms+sqft_living+waterfront+view+grade+age, data=train)
summary(model1)

#Q8
trainPredictors = train[,c(3:11,16)]
testPredictors = test[,c(3:11,16)]
x = preProcess(x = trainPredictors,method = 'pca',thresh = 0.9)
trainComponents = predict(x,newdata=trainPredictors)
trainComponents$price = train$price
head(trainComponents)

#Q9
train_model = lm(price~.,data = trainComponents)
summary(train_model)

#Q10
testComponents = predict(x,newdata=testPredictors)
testComponents$price = test$price

str(trainComponents)
str(testComponents)

#Next, apply the train model created with components to the test-component dataset just created. Compute R2 on the test set. Remember R2 = 1 – sse/sst

pred = predict(train_model,newdata=testComponents)
sse = sum((pred-testComponents$price)^2)
sst = sum((mean(trainComponents$price) - testComponents$price)^2)
r2_test = 1 - sse/sst
r2_test




