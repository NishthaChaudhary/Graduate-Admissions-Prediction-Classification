install.packages('caret')
library('caret')

library(readxl)
Collegefull1 <- read_excel("C:/Sumathi/MBA/Spring 2019/Business Analytics/Project/CollegeDS.xlsx")

Collegefull<-Collegefull1
library(dplyr)
#Removing 1st column
Collegefull<-Collegefull[,c(2,3,4,5,6,7,8,9)]
summary(Collegefull)


View(Collegefull)

str(Collegefull)
# head(Collegefull)

set.seed(3000)

Cslice <-createDataPartition(y=Collegefull$`Chance of Admit`,p=0.7,list = FALSE)

CTrain <-Collegefull[Cslice,]
CTest <-Collegefull[-Cslice,]

dim(CTrain)
dim(CTest)

anyNA(Collegefull)

summary(Collegefull)


#CTrain[,c(8)]=factor(CTrain[,c(8)])
CTrain$`Chance of Admit`=factor(CTrain$`Chance of Admit`)

#print(CTrain$`Chance of Admit`)
#CTrain[["Chance.of.Admit"]]=factor(CTrain[["Chance.of.Admit"]])

CtrainCtrl <- trainControl(method = "repeatedcv" , number = 10, repeats = 3) 

#CtrainCtrl ; Linear

svm_Linear <- train(`Chance of Admit`~., data=CTrain, method = "svmLinear", trControl=CtrainCtrl, preProcess = c("center","scale"), tuneLength=10)
svm_Linear
Test_predict <- predict(svm_Linear,newdata = CTest)
Test_predict
confusionMatrix(table(Test_predict,CTest$`Chance of Admit`))
grid <- expand.grid(C =c(0.01, 0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2.5),gamma=c(0.1,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0))
svm_Linear_Grid <- train(`Chance of Admit`~., data=CTrain, method = "svmLinear", trControl=CtrainCtrl, preProcess = c("center","scale"), tuneGrid=grid , tuneLength=10)
svm_Linear_Grid
tune.outC = tune(svm, `Chance of Admit`~., data=CTrain, kernel="linear", ranges=list(cost=c(0.01 ,0.05 ,0.1 ,0.5 ,1.0), gamma=c(0.01,0.05,0.1,0.5,1.0)))
summary(tune.outC)
plot(svm_Linear_Grid)
plot(tune.outC)
Test_predict_Grid <- predict(svm_Linear_Grid,newdata = CTest)
Test_predict_Grid
confusionMatrix(table(Test_predict_Grid,CTest$`Chance of Admit`))

# Radial

svm_Radial <- train(`Chance of Admit`~., data=CTrain, method = "svmRadial", trControl=CtrainCtrl, preProcess = c("center","scale"), tuneLength=10)
svm_Radial
Test_predictR <- predict(svm_Radial,newdata = CTest)
Test_predictR


confusionMatrix(table(Test_predictR,CTest$`Chance of Admit`))
grid <- expand.grid(sigma=c(0.01,0.05,0.1,0.15,0.18,0.19,0.2,0.25,0.3,0.35,0.4),C =c(0.01, 0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2.5))
svm_Radial_Grid <- train(`Chance of Admit`~., data=CTrain, method = "svmRadial", trControl=CtrainCtrl, preProcess = c("center","scale"), tuneGrid=grid , tuneLength=10)
svm_Radial_Grid

tune.outR = tune(svm, `Chance of Admit`~., data=CTrain, kernel="radial", ranges=list(sigma=c(0.15,0.25,0.75,1.0,1.25), cost=c(0.5 ,1.0 ,1.5 ,2.0 ,2.5), gamma=c(0.5,1,2,3,4)))
summary(tune.outR)

plot(svm_Radial_Grid)

Test_predict_GridR <- predict(svm_Radial_Grid,newdata = CTest)
Test_predict_GridR
confusionMatrix(table(Test_predict_GridR,CTest$`Chance of Admit`))


svm_rad_uc <- svm(`Chance of Admit`~.,data = Collegefull, type='C-classification',method='radial', sigma=c(0.15,0.25,0.75,1.0,1.25), cost=c(0.5 ,1.0 ,1.5 ,2.0 ,2.5), gamma=c(0.5,1,2,3,4))
summary(svm_rad_uc)

#predicted_uc <- predict(svm_rad_uc, newdata= data.frame( Collegefull$`GRE Score`= "312",`TOEFL Score`= "10",`University Rating`="3",SOP=3,LOR = 3.2,CGPA= 8.55,Research= 1))
#predicted_uc
`

#Polynomial

CtrainCtrl1 <- trainControl(method = "repeatedcv" , number = 3, repeats = 1) 

svm_Poly <- train(`Chance of Admit`~., data=CTrain, method = "svmPoly", trControl=CtrainCtrl1, preProcess = c("center","scale"), tuneLength=5)
svm_Poly
Test_predictP <- predict(svm_Poly,newdata = CTest)
Test_predictP

confusionMatrix(table(Test_predictP,CTest$`Chance of Admit`))
grid <- expand.grid(degree=(1:5),scale=c(2.6,10,15) ,C =c(0.75,1,1.25,1.5,2.0,2.5))
svm_Poly_Grid <- train(`Chance of Admit`~., data=CTrain, method = "svmPoly", trControl=CtrainCtrl, preProcess = c("center","scale"), tuneGrid=grid , tuneLength=5)
svm_Poly_Grid

tune.outP = tune(svm, `Chance of Admit`~., data=CTrain, kernel='polynomial', ranges=list(degree=c(1,2,3,4,5), scale=c(0.001,0.002,0.003,0.005),cost=c(0.01 ,0.5 ,1 ,1.5 ,2), gamma=c(0.5,1,2,3,4)))
summary(tune.outP)

plot(svm_Poly_Grid)

Test_predict_GridP <- predict(svm_Poly_Grid,newdata = CTest)
Test_predict_GridP
confusionMatrix(table(Test_predict_GridP,CTest$`Chance of Admit`))


# Concentrating GRE and CGPA

library(ggplot2)
# graph for GRE and CGPA


#Collegefull
Col2Param<-Collegefull[,c(1,6,8)]
summary(Col2Param)


qplot(Collegefull$`GRE Score`,Collegefull$CGPA,data = Collegefull,color = Collegefull$`Chance of Admit`)

#Linear
library(e1071)
#LCModel<-svm(Collegefull$`Chance of Admit`~Collegefull$`GRE Score`+Collegefull$CGPA,data = Collegefull, kernal ="linear", degree=3)
#CModel<-svm(Col2Param$`Chance of Admit`~.,data = Col2Param)
install.packages("caTools")
library(caTools)

#Ctrain<-sample(nrow(Col2Param),nrow(Col2Param)*0.70)

CSplit=sample.split(Col2Param,SplitRatio = 0.7)
C.train<-subset(Col2Param,CSplit==TRUE)
C.test<-subset(Col2Param,CSplit==FALSE)
C.truth=`Chance of Admit`[!C.train]

View(C.train)
str(C.train)
str(C.test)

dim(C.train)
dim(C.test)

x=C.train[,-3]
y=C.train[,3]

x
y

u=C.test[,-3]
w=C.test[,3]

install.packages("rowr")
library(rowr)

LiCModel = svm(`Chance of Admit`~., C.train, kernel="linear", ranges=list(cost=c(0.01 ,0.5 ,1 ,1.5 ,2), gamma=c(0.5,1,2,3,4)))
Lpred<-predict(LiCModel,C.test[,-3])
ALPred <-data.frame(cbind(Lpred,C.test[,3]))
Ltab<-table(predict=ALPred$Lpred, truth=ALPred$Chance.of.Admit )#compare the predicted values and true values

#plot(LiCModel, data = C.train,`GRE Score`~ CGPA)

tune.Li = tune(svm, `Chance of Admit`~., data=C.train, kernel='linear', ranges=list(cost=c(0.01 ,0.5 ,1 ,1.5 ,2), gamma=c(0.5,1,2,3,4)))
summary(tune.Li)

plot((tune.Li))

LiCModel$nSV # number of support vector
LiCModel$tot.nSV # total support vector

summary(LiCModel)

#ConfusionMatrix and Misclassification 
Ltab
LAcc <-sum(diag(Ltab))/sum(Ltab)
LAcc
LMError<-1-sum(diag(Ltab))/sum(Ltab)
LMError


#GRE and GPA -Radial

RiCModel = svm(`Chance of Admit`~., C.train, kernel="radial", ranges=list(cost=c(0.01 ,0.5 ,1 ,1.5 ,2), gamma=c(0.5,1,2,3,4)))
Rpred<-predict(RiCModel,C.test[,-3])
ARPred <-data.frame(cbind(Rpred,C.test[,3]))
Rtab<-table(predict=ARPred$Rpred, truth=ARPred$Chance.of.Admit)#compare the predicted values and true values

#plot(LiCModel, data = C.train,`GRE Score`~ CGPA)

tune.Ri = tune(svm, `Chance of Admit`~., data=C.train, kernel='radial', ranges=list(sigma=c(0.15,0.25,0.75,1.0,1.25), cost=c(0.5 ,1.0 ,1.5 ,2.0 ,2.5), gamma=c(0.5,1,2,3,4)))
summary(tune.Ri)

#plot(tune.Ri)

RiCModel$nSV # number of support vector
RiCModel$tot.nSV # total support vector

summary(RiCModel)

#ConfusionMatrix and Misclassification 
Rtab
RAcc <-sum(diag(Rtab))/sum(Rtab)
RAcc
RMError<-1-sum(diag(Rtab))/sum(Rtab)
RMError


#GRE GPA-Polynomial


PiCModel = svm(`Chance of Admit`~., C.train, kernel="polynomial", ranges=list(cost=c(0.01 ,0.5 ,1 ,1.5 ,2), gamma=c(0.5,1,2,3,4)))
Ppred<-predict(PiCModel,C.test[,-3])
APPred <-data.frame(cbind(Ppred,C.test[,3]))
Ptab<-table(predict=APPred$Ppred, truth=APPred$Chance.of.Admit)#compare the predicted values and true values

#plot(LiCModel, data = C.train,`GRE Score`~ CGPA)

tune.Pi = tune(svm, `Chance of Admit`~., data=C.train, kernel='polynomial', ranges=list(degree=c(1,2,3), scale=c(0.5,1.0,2.0),cost=c(0.5,1 ,1.5), gamma=c(0.5,1,2)))
summary(tune.Pi)

#plot(tune.Ri)

PiCModel$nSV # number of support vector
PiCModel$tot.nSV # total support vector

summary(PiCModel)

#ConfusionMatrix and Misclassification 
Ptab
PAcc <-sum(diag(Ptab))/sum(Ptab)
PAcc
PMError<-1-sum(diag(Ptab))/sum(Ptab)
PMError









