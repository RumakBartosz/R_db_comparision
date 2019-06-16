#############################
# ORACLE DB SCRIPT
#############################

Sys.setenv(JAVA_HOME='sciezka do JAVA_HOME')
options(java.parameters="-Xmx2g")
library(rJava)

.jinit()
print(.jcall("java/lang/System", "S", "getProperty", "java.version"))

########### Zaladowanie bazy -> RJDBC ###########
library(RJDBC)

jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="Sciezka do ojdbc6.jar")
jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//HOST:PORT/NAZWA_DB", "nazwa konta w db", "haslo w db")

# Wyciągnięcie danych zapytaniem do db
oracle_db <- dbGetQuery(jdbcConnection, "SELECT * FROM hpc")

oracle_db <- oracle_db[,-2]
oracle_db <- oracle_db[,-1]

########### przygotowanie danych ##########

oracle_db <- na.omit(oracle_db) # skasowanie brakujących wartości
#oracle_db <- scale(simple_db) # standardyzacja danych

########### K-Means ##########
fit <- kmeans(na.omit(oracle_db), 5) # pięć klastrów
# obliczenie średnich klastrów
aggregate(na.omit(oracle_db),by=list(fit$cluster),FUN=mean)
# dołącz do simple_db kolumnę przynależności punktu do klastra
oracle_db <- data.frame(oracle_db, fit$cluster) 

########### svm -> e1071 ##########
cut_oracle_db <- oracle_db[1:10000,]

library("e1071")
x <- cut_oracle_db[,1:7]
y <- cut_oracle_db$fit.cluster

svm_model <- svm(fit.cluster ~ ., data=cut_oracle_db)

pred <- predict(svm_model,x)

########### Neural net -> caTools, RSNNS ###########

#rozdzielanie danych na testowe i treningowe
library(caTools)
set.seed(101)
split = sample.split(cut_oracle_db$fit.cluster, SplitRatio = 0.7)
train = subset(cut_oracle_db, split == TRUE)
test = subset(cut_oracle_db, split == FALSE)

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

