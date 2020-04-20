
#Unsupervised learning 

#k means clustering -  Exclusive clustering 

#How K means work

# Intialization  ----> Cluster assignment ----> Move centroid ----> Optimization ----> Convergence 

#Analyze the information given in the following 'Insurance Policy dataset' to create clusters of persons falling in the same type.


#TASK 1
#Import the data __________________________________________________________________________________

getwd()
setwd("C:/Users/ICH/Google Drive/Data Science/Edureka/Data Science Certification with R/Module 6 _ unsupervised learning/338_m6_dataset_v3.0")
list.files()

insurance <- read_excel("InsuranceData.xlsx")

#Install the packages_______________________________________________________________________________
require(readxl)
require(ggplot2)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms

#Data exploration 
head(insurance, 30)
sum(is.na(insurance))
dim(insurance)
#TASK 2 - Perform K-means clustering on the data

km <- kmeans(insurance , 3) #apply k means clustering and 3 is random centroid points
km

km$cluster
clust <- km$cluster
insurance <- cbind(insurance, clust)
head(insurance)

#order the isurance data based on the insurance clust
grp_insurance <- insurance[order(insurance$clust),]
grp_insurance

#Find the optimal number of clusters for our data
mydata <- grp_insurance
set.seed(123)
k.max <- 10
wss <- sapply(1:k.max, function(k){kmeans(mydata,k,nstart = 1)$tot.withinss})
wss

plot(1:k.max,wss, type="b", frame=FALSE, xlab = "Number of clusters k" , ylab = 'total within clusters sum of squares')
abline(v=3, lty=4)

km <- kmeans(insurance , 3) #apply k means clustering and 3 is random centroid points
km



#Plot age vs income while giving them cluster colors

install.packages("animation")
library(animation)

km_ani <- kmeans.ani(insurance, 4) #apply kmeans clustering


ggplot(insurance, aes(x = Age, y = Income))+geom_point(col = clust ,shape = 11)


#Task 3:Perform C-means clustering on the data and save the membership dataframe in csv format.
require(e1071)


insurance_cmean <- insurance
insurance_cmean

cm <- cmeans(insurance_cmean, 3)
options(scipen = 10) # to change the scientic numbers into decimal values 
cm$cluster
clust_c <-cm$cluster 
cm

insurance_cmean <- cbind(insurance_cmean, clust_c)
head(insurance_cmean)
class(insurance_cmean)

write.csv(insurance_cmean,"C:/Users/ICH/Google Drive/Data Science/Edureka/Data Science Certification with R/Module 6 _ unsupervised learning\\membership_df.csv", row.names = FALSE)





