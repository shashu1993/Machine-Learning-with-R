library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(caTools)
library(lm.beta)
house = read.csv("/Users/shashank/Desktop/Frameworks/houses.csv")

set.seed(1031)
split = createDataPartition(y = house$price, p = 0.7, list = F, groups = 100)
train = house[split,]
test = house[-split,]

ggplot(house,aes(x = sqft_living , y = price))+
  geom_point() +   geom_smooth(method='lm',size=1.3,color='steelblue3')
cor(house$sqft_living,house$price)

model1 = lm(price~sqft_living,data=train)
paste('price','=',round(coef(model1)[1],0),'+',round(coef(model1)[2],0),'area')
anova(model1)
summary(model1)

pred = predict(model1)
data.frame(price = train$price[100:109], prediction = pred[100:109])


sse = sum((pred - train$price)^2)
sst = sum((mean(train$price)-train$price)^2)
model1_r2 = 1 - sse/sst; model1_r2

sse1 = sum((pred-train$price)^2); sse1

rmse1 = sqrt(mean((pred-train$price)^2)); rmse1

a=predict(model1,newdata=data.frame(sqft_living = 1400)); a;
b=predict(model1,newdata=data.frame(sqft_living = 1600)); b;
b-a

ggplot(train,aes(x=waterfront ,y=price,fill=waterfront))+
  geom_bar(stat='summary',fun.y='mean',position='dodge')+
  guides(fill=F)

model2 = lm(price~waterfront,data=train)
class(train$waterfront)
levels(train$waterfront)

summary(model2)

pred = predict(model2)
data.frame(price = train$price[100:109], prediction = pred[100:109])

sse = sum((pred - train$price)^2)
sst = sum((mean(train$price)-train$price)^2)
model2_r2 = 1 - sse/sst; model2_r2

sse2 = sum((pred-train$price)^2); sse2

rmse2 = sqrt(mean((pred-train$price)^2)); rmse2


model3 = lm(price~sqft_living+waterfront,data=train)
summary(model3)

pred3 = predict(model3)

sse3 = sum((pred3 - train$price)^2)
sst3 = sum((mean(train$price)-train$price)^2)
model3_r2 = 1 - sse4/sst4; model3_r2

rmse3 = sqrt(mean((pred3-train$price)^2)); rmse3

model4 = lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age, data=train)
summary(model4)

pred4 = predict(model4)

sse4 = sum((pred4 - train$price)^2)
sst4 = sum((mean(train$price)-train$price)^2)
model4_r2 = 1 - sse4/sst4; model4_r2

rmse4 = sqrt(mean((pred4-train$price)^2)); rmse4

lm.beta(model4)

pred5 = predict(model4, newdata = test)

sse4_test = sum((pred5 - test$price)^2)
sst4_test = sum((mean(train$price)-test$price)^2)
model4_r2_test = 1 - sse4_test/sst4_test; model4_r2_test

rmse5_test = sqrt(mean((pred5-test$price)^2)); rmse5_test
