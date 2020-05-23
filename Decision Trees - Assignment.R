getwd()
wages = read.csv("/Users/shashank/Desktop/Frameworks/assignment7_wages.csv")

str(data)

#Section 1
#q1
head(data)

#q2
wages = wages[wages$earn>0,]
prop.table(table(wages$sex))

#q3
tapply(wages$earn, wages$race, mean)


#q4
set.seed(1731)
split = sample(1:nrow(wages), nrow(wages)*0.75)
train = wages[split,]
test = wages[-split,]

#q5
1026/1368           

#Section 2
#q1
model1 = lm(earn~.,data=train)
summary(model1)

#q2
pred = predict(model1)
rmse1 = sqrt(mean((pred-train$earn)^2)); rmse1

#q3
library(ggplot2)
ggplot(data=train,aes(y=earn,x=sex,fill=factor(ed)))+ 
  geom_bar(stat="summary",fun.y="mean",position="dodge")


ggplot(data=train,aes(y=earn,x=ed,color=sex))+  
  geom_smooth(method="lm",se=F,size=1.2)+  
  scale_x_continuous(breaks=c(seq(2,20,2)))+  
  scale_y_continuous(breaks=c(seq(0,100000,10000)))

58000-38000

#q4
33000-18000

#q5
model_sex_ed = lm(earn~sex + ed + sex*ed,data=train)
summary(model_sex_ed)

#q6

model2 = lm(earn~height + sex + race + ed + age + sex*ed,data=train)
pred = predict(model2)
rmse2 = sqrt(mean((pred-train$earn)^2)); rmse2

#q7
#yes

#q8
model3 = lm(earn~height + sex + race + ed + age + sex*ed + sex*age,data=train)
pred = predict(model3)
rmse3 = sqrt(mean((pred-train$earn)^2)); rmse3


#q8
model4 = lm(earn~height + sex + race + ed + age + sex*ed + sex*age + age*ed,data=train)
pred = predict(model4)
rmse4 = sqrt(mean((pred-train$earn)^2)); rmse4


#q9
model5 = lm(earn~(height+sex+race+ed+age)^2,data=train)
pred = predict(model5)
rmse5 = sqrt(mean((pred-train$earn)^2)); rmse5

#q10
summary(model1)
summary(model5) 

#section3 
#q1
library(rpart)
library(rpart.plot)
tree1 = rpart(earn~.,data=train)
rpart.plot(tree1, digits=5)

#2 
#last leaf

#3 
#second leaf

#4 & #5
summary(tree1)
pred_tree1 = predict(tree1,train)
rmse1 = sqrt(mean((pred_tree1-train$earn)^2)); rmse1

#6
treeSimp1 = rpart(earn~.,data=train,control=rpart.control(minbucket=20))
rpart.plot(treeSimp1)


#7
pred_tree2 = predict(treeSimp1,train)
rmse2 = sqrt(mean((pred_tree2-train$earn)^2)); rmse2


#8
treeSimp2 = rpart(earn~.,data=train,control=rpart.control(minbucket=50))
rpart.plot(treeSimp2)


#9
pred_tree3 = predict(treeSimp2,train)
rmse3 = sqrt(mean((pred_tree3-train$earn)^2)); rmse3

#10
treeComplex1 = rpart(earn~.,data=train,control=rpart.control(minbucket=5))
rpart.plot(treeComplex1)
pred_tree3 = predict(treeComplex1,train)
rmse3 = sqrt(mean((pred_tree3-train$earn)^2)); rmse3

#11 & #12
treeComplex2 = rpart(earn~.,data=train,control=rpart.control(minbucket=1))
rpart.plot(treeComplex2)
pred_tree3 = predict(treeComplex2,train)
rmse3 = sqrt(mean((pred_tree3-train$earn)^2)); rmse3



#section4
#q1

predt3 = predict(model5, newdata = test)
rmse3_test = sqrt(mean((predt3-test$earn)^2)); rmse3_test

#q2

predt1 = predict(tree1, newdata = test)
rmse1_test = sqrt(mean((predt1-test$earn)^2)); rmse1_test

#q3

predt2 = predict(treeSimp2, newdata = test)
rmse1_test = sqrt(mean((predt2-test$earn)^2)); rmse1_test



#q4

predt4 = predict(treeComplex2, newdata = test)
rmse1_test = sqrt(mean((predt4-test$earn)^2)); rmse1_test





