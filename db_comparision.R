##############################
# skrypt porównujący czasy wykonań niektórych algorytmów uczenia nienadzorowanego
# pomiędzy bazami danych w postaci pojedynczej tabeli a relacyjną bazą danych MS
##############################


########### załadowanie prostej bazy danych do data.frame ##########

simple_db <- read.table(file = '~/R/db_comparision/household_power_consumption.txt',sep = ';',header = TRUE)
simple_db <- simple_db[,-2]
simple_db <- simple_db[,-1]

########### przygotowanie danych ##########

simple_db <- na.omit(simple_db) # skasowanie brakujących wartości
#simple_db <- scale(simple_db) # standardyzacja danych

########### K-Means ##########
fit <- kmeans(na.omit(simple_db), 5) # pięć klastrów
# obliczenie średnich klastrów
aggregate(na.omit(simple_db),by=list(fit$cluster),FUN=mean)
# dołącz do simple_db kolumnę przynależności punktu do klastra
simple_db <- data.frame(simple_db, fit$cluster) 

########### svm -> e1071 ##########
cut_simple_db <- simple_db[1:10000,]

library("e1071")
x <- cut_simple_db[,1:7]
y <- cut_simple_db$fit.cluster

svm_model <- svm(fit.cluster ~ ., data=cut_simple_db)

pred <- predict(svm_model,x)

########### Neural net -> caTools, RSNNS ###########

#rozdzielanie danych na testowe i treningowe
library(caTools)
set.seed(101)
split = sample.split(cut_simple_db$fit.cluster, SplitRatio = 0.7)
train = subset(cut_simple_db, split == TRUE)
test = subset(cut_simple_db, split == FALSE)

train_x_neural <- train[,1:7]
train_x_neural <- data.matrix(train_x_neural)

train_y_neural <- train$fit.cluster

test_x_neural <- test[,1:7]
test_x_neural <- data.matrix(test_x_neural)

test_y_neural <- test$fit.cluster

#część właściwa
library(RSNNS)

model <- mlp(train_x_neural, train_y_neural, size=5, learnFuncParams=c(0.001), 
    maxit=50)

predictions <- predict(model,test_x_neural)
