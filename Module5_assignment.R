#Install the package and download the data from package 

install.packages("AER")
library(AER)
data("Affairs")


#viewing and analysing the data

head(Affairs, 20)
sum(is.na(Affairs))

summary(Affairs)

#TASK 1

#Create a new column with nominal values "YES" and "NO"

Affairs$affairs

Affairs$affairs <- ifelse(Affairs$affairs > 0 , "Yes" , "No")
table(Affairs$affairs)

#Convert it intofactor
Affairs$affairs <- as.factor(Affairs$affairs)
class(Affairs$affairs)

#Setting up the data for the model 

set.seed(100)
forTrainig <- sample(2, nrow(Affairs), prob = c(0.7,0.3), replace = TRUE)

train_set <- Affairs[forTrainig == 1,]
test_set <- Affairs[forTrainig ==2, ]

nrow(train_set)
nrow(test_set)

class(train_set)

#Decision tree Model 

install.packages("rpart.plot")	
install.packages("rpart")	
library(rpart)
library(rpart.plot)

fit <- rpart(train_set$affairs ~., data = train_set)
summary(fit)
rpart.plot(fit, extra = 106)

predict_model <- predict(fit, newdata = test_set, type = "class")
summary(predict_model)



library(caret)
library(lattice)
library(ggplot2)
library(e1071)

confusionMatrix(table(predict_model, test_set$affairs))

rpart.plot(fit, box.palette="RdBu", shadow.col="gray", nn=TRUE)

#______________________________________________________________________________________________
#Random forest 

library(randomForest)
set.seed(10)
forTrainig <- sample(2, nrow(Affairs), prob = c(0.7,0.3), replace = TRUE)

train_set_random <- Affairs[forTrainig == 1,]
test_set_random <- Affairs[forTrainig ==2, ]

nrow(train_set_random)
nrow(test_set_random)

class(train_set_random)

fit_random <- randomForest(affairs~. , data = train_set_random, ntree = 100, mtry = 3, importance = TRUE)
summary(fit_random)
#OBB DATA______________________________________________________________________________________
#GET OBB data from the plot and coerce to data.table 
library(data.table)
oobdata = as.data.table(plot(fit_random))

#cast to long format
oobdata2 = melt(oobdata, id.vars = "trees")
setnames(oobdata2, "value", "error")
#Plot using ggplot 2
ggplot(data = oobdata2, aes(x = trees, y= error, color = variable)) + geom_line()

#Define trees as 1:ntree
oobdata[,trees := .I]

predic_random <- predict(fit_random, newdata = test_set_random, type = "class")
summary(predic_random)

confusionMatrix(table(predic_random,test_set_random$affairs))

hist(treesize(fit_random))
varImp(fit_random)
varImpPlot(fit_random, sort = TRUE, ttype = 2)
importance(fit_random)
varUsed(fit_random)
#mtry_______________________________________________________________________________________
#Using for loop to identify the right mtry for model
a <- c()
i <-  5

for(i in 3:8){
  model3 <- randomForest(train_set_random$affairs~., data = train_set_random, ntree = 100, 
                         mtry = i, importance = TRUE)
  predValid <- predict(model3, test_set_random, type = "class")
  a[i-2] =mean(predValid == test_set_random$affairs)
  }

a

plot(3:8,a)
#__________________________________________________________________________________________





