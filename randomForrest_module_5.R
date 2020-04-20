#Import the dataset 

diabities <- read.csv("C:/Users/ICH/Google Drive/Data Science/Edureka/Data Science Certification with R/Random forest model/338_m5_dataset_v3.0/Diabetes.csv", header = TRUE)

#Backup - data
original_diabities <- diabities
diabities <- original_diabities

view(diabities)
#view the data
head(diabities)
str(diabities)

#Setting up the model for the data set 

id <- sample(2,nrow(diabities), prob = c(0.7,0.3) , replace = TRUE)

train_set <- diabities[id==1,]
valid_set <- diabities[id==2,]

nrow(train_set)
nrow(valid_set)

diabities$Is_Diabetic <- as.factor(diabities$Is_Diabetic)
train_set$Is_Diabetic <- as.factor(train_set$Is_Diabetic)


#Running the random forest model__________________________________________________________
 library(randomForest)
bestmtry <- tuneRF(train_set, train_set$Is_Diabetic, stepFactor = 1.2 ,improve =  0.01)

model <- randomForest(Is_Diabetic~., data = train_set)

model

importance(model) #checking the most important factor that effects the decison tree
varImpPlot(model) #plotting the importance gini flott

#Prediction - testing the model___________________________________________________________

predict_model <- predict(model, valid_set, type = "class")
predict_model

library(caret)
confusionMatrix(table(predict_model, valid_set$Is_Diabetic))
