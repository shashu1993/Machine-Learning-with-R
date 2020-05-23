library(ISLR)
data(OJ)
str(OJ)
head(OJ)

library(caTools)
set.seed(1234)
split = sample.split(OJ$Purchase, SplitRatio = 0.7)
train = OJ[split,]
test = OJ[!split,]

#q1 
nrow(train)

#q2
table(train$Purchase)

#q3
tapply(train$PriceMM, train$Purchase, mean)

#q4
tapply(train$DiscMM, train$Purchase, mean)

#q5
table(OJ$Purchase,OJ$WeekofPurchase) 


