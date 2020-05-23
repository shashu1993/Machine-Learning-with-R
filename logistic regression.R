
getwd()
data = read.csv("/Users/shashank/Desktop/Frameworks/eBayAssignment.csv")
summary(data)
nrow(data)

table(data$color)
colnames(data)
summary(data$productline)

max(data$startprice)
which.max(data$startprice)
data[1397,]

data[which.max(data$startprice), 'startprice']
library(caTools)
set.seed(196)
split = sample.split(data$sold, SplitRatio = 0.8)
train = data[split, ]
test = data[!split, ]

nrow(train)
head(train)

ipadSold = subset(train, sold == 1, startprice)
median(ipadSold$startprice) 

ipadnotSold = subset(train, sold == 0, startprice)
median(ipadnotSold$startprice) 

model1 = glm(sold~biddable + startprice + condition + cellular + carrier + color + storage + productline + noDescription + charCountDescription + upperCaseDescription + startprice_99end, data=train, family='binomial')
summary(model1)

model1$aic

model2 = glm(sold~biddable+startprice+condition+storage+productline+upperCaseDescription+startprice_99end, data=train, family='binomial')
summary(model2)

library(lm.beta)
lm.beta(model2)

100*(exp((model2)$coef[3])-1)
exp(model2$coef[3])


100*(exp(summary(model2)$coef[12])-1)
exp(summary(model2)$coef[12])
    
    
model_productline = glm(sold~productline, data=train, family='binomial')
summary(model_productline)


pred = predict(model2, type = 'response', newdata=test)
pred[test$UniqueID==10940]

ct = table(sold = test$sold, predictions = as.numeric(pred>0.5))
ct

accuracy = sum(ct[1,1],ct[2,2])/nrow(test)
accuracy


prop.table(table(test$sold))


#install.packages('ROCR')   # if you have not installed ROCR, be sure to install it first. 
library(ROCR)
ROCRpred = prediction(pred,test$sold)
as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure

## construct plot
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.2),text.adj=c(-0.3,2),xlab="1 - Specificity",ylab="Sensitivity") # color coded, annotated ROC curve
