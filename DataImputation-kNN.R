library("dplyr")
library("readxl")
library("partykit")
library("rpart")
library("caTools")
library("caret")
library("Hmisc")
library(VIM)
Data3 <- read_xlsx("C:/Users/User/Desktop/SEM-1/Data Analytics/ProjectData.xlsx")
str(Data3)
summary(Data3)

get_pruned <- function(model_in){
  op <- which.min(model_in$cptable[,"xerror"])
  cp <- model_in$cptable[op,"CP"]
  print(cp)
  model_in_pruned <- prune(model_in,cp=cp)
  return(model_in_pruned)
}

#Inpute data using kNN
Impute1 <- kNN(Data3, variable= colnames(Data3), k=5)

Data <- Impute1[1:19]

df <- data.frame(Data[,-1]) #remove ID from dataframe
Target=ifelse(df$Response==1,'Y','N') 
df <- data.frame(df, Target) #add Target to the dataframe
df <- df[,-1] #remove Response
df <- mutate(df,Y1=factor(Y1),Y2=factor(Y2),Y3=factor(Y3),Y4=factor(Y4),Y5=factor(Y5),Y6=factor(Y6),Y7=factor(Y7))
str(df)
summary(df)

#split into train and test sets
set.seed(123)
sample= sample.split(df$Target,SplitRatio= 0.75)
train=subset(df, sample==TRUE)
test= subset(df, sample==FALSE)

# All X's, Y's and groups
df_with_all_X_and_Y_all_groups <- train
DT_Model_XY_all_groups <- rpart(Target~., data=df_with_all_X_and_Y_all_groups, 
                                control=rpart.control(minsplit=30, 
                                                      minbucket=15, 
                                                      maxdepth=4 )) 
plot(as.party(DT_Model_XY_all_groups))
# checking accuracy using confusion matrix
tree.predicted<- predict(DT_Model_XY_all_groups,test, type='class')
confusionMatrix(tree.predicted , test$Target)


# all Y's - for all groups
df_excluding_X <- within(train, rm(X1, X2,  X3, X4, X5, X6, X7))
DT_Model_excluding_X <- rpart(Target~., data=df_excluding_X, 
                              control=rpart.control(minsplit=30, 
                                                    minbucket=15, 
                                                    maxdepth=4 ))
plot(as.party(DT_Model_excluding_X))
# checking accuracy using confusion matrix
tree.predicted<- predict(DT_Model_excluding_X,test, type='class')
confusionMatrix(tree.predicted , test$Target)


# all X's - for all groups
df_excluding_Y <- within(train, rm(Y1, Y2, Y3, Y4, Y5, Y6, Y7))
DT_Model_excluding_Y <- rpart(Target~., data=df_excluding_Y, 
                              control=rpart.control(minsplit=30, 
                                                    minbucket=15, 
                                                    maxdepth=4 ))
plot(as.party(DT_Model_excluding_Y))
# checking accuracy using confusion matrix
tree.predicted<- predict(DT_Model_excluding_Y,test, type='class')
confusionMatrix(tree.predicted , test$Target)

# All X,Y, group 0
df_all_XY_group0 <- train[train$Group == 0,]
DT_Model_all_XY_group0 <- rpart(Target~., data=df_all_XY_group0, 
                                control=rpart.control(minsplit=30, 
                                                      minbucket=15, 
                                                      maxdepth=4 ))
plot(as.party(DT_Model_all_XY_group0))
# checking accuracy using confusion matrix
tree.predicted<- predict(DT_Model_all_XY_group0,test, type='class')
confusionMatrix(tree.predicted , test$Target)

# all X, group 0
df_all_X_group0 <- train[train$Group == 0,]
df_all_X_group0 <- within(df_all_X_group0, rm(Y1, Y2, Y3, Y4, Y5, Y6, Y7))
DT_Model_all_X_group0 <- rpart(Target~., data=df_all_X_group0, 
                               control=rpart.control(minsplit=30, 
                                                     minbucket=15, 
                                                     maxdepth=4 ))
plot(as.party(DT_Model_all_X_group0))
# checking accuracy using confusion matrix
tree.predicted<- predict(DT_Model_all_X_group0,test, type='class')
confusionMatrix(tree.predicted , test$Target)

# all Y, group 0
df_all_Y_group0 <- train[train$Group == 0,]
df_all_Y_group0 <- within(df_all_Y_group0, rm(X1, X2,  X3, X4, X5, X6, X7))
DT_Model_all_Y_group0 <- rpart(Target~., data=df_all_Y_group0, 
                               control=rpart.control(minsplit=30, 
                                                     minbucket=15, 
                                                     maxdepth=4 ))
plot(as.party(DT_Model_all_Y_group0))
# checking accuracy using confusion matrix
tree.predicted<- predict(DT_Model_all_Y_group0,test, type='class')
confusionMatrix(tree.predicted , test$Target)


# All X,Y, group 1
df_all_XY_group1 <- train[train$Group == 1,]
DT_Model_all_XY_group1 <- rpart(Target~., data=df_all_XY_group1, 
                                control=rpart.control(minsplit=30, 
                                                      minbucket=15, 
                                                      maxdepth=4 ))
plot(as.party(DT_Model_all_XY_group1))
# checking accuracy using confusion matrix
tree.predicted<- predict(DT_Model_excluding_X,test, type='class')
confusionMatrix(tree.predicted , test$Target)

# all X, group 1
df_all_X_group1 <- train[train$Group == 1,]
df_all_X_group1 <- within(df_all_X_group1, rm(Y1, Y2, Y3, Y4, Y5, Y6, Y7))
DT_Model_all_X_group1 <- rpart(Target~., data=df_all_X_group1, 
                               control=rpart.control(minsplit=30, 
                                                     minbucket=15, 
                                                     maxdepth=4 ))
plot(as.party(DT_Model_all_X_group1))
# checking accuracy using confusion matrix
tree.predicted<- predict(DT_Model_all_X_group1,test, type='class')
confusionMatrix(tree.predicted , test$Target)

# all Y, group 1
df_all_Y_group1 <- train[train$Group == 1,]
df_all_Y_group1 <- within(df_all_Y_group1, rm(X1, X2,  X3, X4, X5, X6, X7))
DT_Model_all_Y_group1 <- rpart(Target~., data=df_all_Y_group1, 
                               control=rpart.control(minsplit=30, 
                                                     minbucket=15, 
                                                     maxdepth=4 ))
plot(as.party(DT_Model_all_Y_group1))
# checking accuracy using confusion matrix
tree.predicted<- predict(DT_Model_all_Y_group1,test, type='class')
confusionMatrix(tree.predicted , test$Target)

#rmarkdown::render("C:/Users/User/Desktop/SEM-1/Data Analytics/DataImputation-kNN.R")
