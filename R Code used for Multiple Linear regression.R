## set the directory of R studio As Same in which data is avalilable
setwd("D:\\desktop files\\R Directory\\Machine Learning Data")

## Importing dataset
dataset<- read.csv("50_Startups.csv")

## converting Categorical dataset into dummy variable 
dataset$State<- factor(dataset$State, levels= c("New York", "California", "Florida"), labels= c('1','2','3'))

## splitting data 

library(caTools)
set.seed(123)
split=sample.split(dataset$Profit, SplitRatio = 0.8)
trainingSet<- subset(dataset, split==T)
testSet<- subset(dataset, split==F)

# feature scaling not required for lm function

# fitting linera regression to training set
regressor1<- lm(formula = Profit~ R.D.Spend+ State+ Administration+ Marketing.Spend , data = trainingSet)
summary(regressor1)
regressor2<- lm(formula = Profit~ R.D.Spend+ Administration+ Marketing.Spend , data = trainingSet)
summary(regressor2)
regressor3<- lm(formula = Profit~ R.D.Spend+ Marketing.Spend , data = trainingSet)
summary(regressor3)
regressor4<- lm(formula = Profit~ R.D.Spend, data = trainingSet)
summary(regressor4)
## Exporting diffrent multiple Linear regression output
library(texreg)
texreg::htmlreg(list(regressor, regressor2, regressor3, regressor4),file='Linear regression output.doc')


## predicting the test set result by different modeling output
y_pred1<- predict(regressor1, newdata= testSet)
y_pred2<- predict(regressor2, newdata= testSet)
y_pred3<- predict(regressor3, newdata= testSet)
y_pred4<- predict(regressor4, newdata= testSet)
testSet$Predict1 <- y_pred1
testSet$Predict2 <- y_pred2
testSet$Predict3 <- y_pred3
testSet$Predict4 <- y_pred4
## exporting testset data with predictions in different models 
write.csv(testSet, "Profit comparision of different model.csv")
getwd()


    
