library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(caTools)
house = read.csv("/Users/shashank/Desktop/Frameworks/houses.csv")
set.seed(1031)
split = createDataPartition(y = house$price, p = 0.7, list = F, groups = 100)
train = house[split,]
test = house[-split,]

nrow(train)
nrow(test)

mean(train$price)
mean(test$price)

train %>%
  select(id,price:sqft_lot,age)%>%
  gather(key=numericVariable,value=value,price:age)%>%
  ggplot(aes(x='',y=value))+
  geom_boxplot(outlier.color = 'red')+
  facet_wrap(~numericVariable,scales='free_y')

train[which.max(train$bedrooms),]

ggplot(train, aes(x = sqft_living, y = price)) + geom_smooth() + geom_point()
cor(train$sqft_living, train$price)




