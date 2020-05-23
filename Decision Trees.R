data = read.csv("/Users/shashank/Desktop/Frameworks/eBayAssignment.csv")

library(caTools)
set.seed(617)
split = sample.split(data$sold,SplitRatio = 0.7)
train = data[split,]
test = data[!split,]

library(rpart.plot)
library(rpart)
classTree1 = rpart(sold~startprice,data=train,method='anova', cp = 0.001)
summary(classTree1)
#rpart.plot(classTree1)

pred_test = predict(classTree1, newdata = test)
