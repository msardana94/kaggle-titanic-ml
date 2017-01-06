library(randomForest)
library(rpart)
library(party)
library(caret)
library(ROCR)

train<-read.csv("train.csv")
test<-read.csv("test.csv")

test$Survived<-NA
combi<-rbind(train,test)

combi$Name<-as.character(combi$Name)

combi$Title<-sapply(combi$Name, FUN=function(x){trimws(strsplit(x,split='[,.]')[[1]][2])})
# combi$Title<-sub('','',combi$Title)

combi$Title[combi$Title %in% c('Ms','Mlle')]<-'Miss'
combi$Title[combi$Title %in% c('Dr','Capt', 'Col', 'Don', 'Jonkheer', 'Major', 'Rev', 'Sir','Mr')]<-'Mr'
combi$Title[combi$Title %in% c('Dona','Lady','the Countess','Mrs','Mme')]<-'Mrs'

combi$Title<-factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1


combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 3,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

combi$FamilyID <- factor(combi$FamilyID)


summary(combi$Age)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
                data=combi[!is.na(combi$Age),], method="anova")

combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

summary(combi)

summary(combi$Embarked)
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)


combi$FamilyID2 <- combi$FamilyID

combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
# combi$FamilyID2[combi$FamilySize > 3] <- 'Large'

combi$FamilyID2 <- factor(combi$FamilyID2)

#Adding Mother
combi$Mother<-0
# combi$Mother[combi$Sex=='female' & combi$Parch>0 & combi$Age>18 & combi$Title!='Miss']<-1
combi$Mother[combi$Sex=='female' & combi$Parch>0 & combi$Age >18 & combi$Pclass == 1 & combi$Title!='Miss']<-1
#Adding Child
# combi$Child1<-0
# combi$Child1[ combi$Parch>0 & combi$Age<18 & (combi$Pclass ==2 | combi$Pclass ==1) ]<-1

combi$Child2<-0
combi$Child2[combi$FamilySize>1 & combi$Age<=18 & (combi$Pclass ==2 | combi$Pclass ==1)]<-1

# combi$Old1 <-0
# combi$Old1[combi$Sex=='female' & combi$Age>50]<-1
# combi$Old1[combi$Sex=='female' & (combi$Pclass ==2 | combi$Pclass ==1)]<-1

# combi$Old2 <-1
# combi$Old2[combi$Sex=='male' & combi$Age>50 & (combi$Pclass ==2 | combi$Pclass ==3)]<-0
# combi$Old1[combi$Sex=='female' & (combi$Pclass ==2 | combi$Pclass ==1)]<-1

# combi$Cabinletter<-sapply(combi$Cabin, function(x) strsplit(as.character(x),'[1-9]')[[1]][1])
# # combi$num<-as.numeric(combi$CabinNum)
# combi$Cabinletter[is.na(combi$Cabinletter)]<-"Z"
# combi$Cabinletter[combi$Cabinletter=='F E']<-"F"
# combi$Cabinletter[combi$Cabinletter=='F G']<-"F"
# combi$Cabinletter<- factor(combi$Cabinletter)
# 
# 
# combi$CabinNum<-sapply(combi$Cabin,function(x) strsplit(as.character(x),'[A-Z]')[[1]][2])
# combi$CabinNum<-as.numeric(combi$CabinNum)
# combi$CabinPos<-NA
# 
# #Categorize 1-50 as Front, 50-100 as Middle, >100 as End
# combi$CabinPos[combi$CabinNum<50]<-'Front'
# combi$CabinPos[combi$CabinNum>=50 & combi$CabinNum<100]<-'Middle'
# combi$CabinPos[combi$CabinNum>=100]<-'End'
# # combi<-combi[!is.na(combi$CabinNum),]
# combi$CabinPos<-factor(combi$CabinPos)

combi$firstcls<-0
combi$firstcls[(combi$Pclass==1 | combi$Pclass==2) & combi$Sex=='female']<-1

combi$midage<-1
combi$midage[combi$Sex=='male' & (combi$Pclass==2 | combi$Pclass==3) & combi$Age>18 & combi$Age<50]<-0

train <- combi[1:891,]
test <- combi[892:1309,]

########################################################################################
ggplot(train,aes(as.factor(train$Survived),train$Age))+geom_violin() + facet_grid(Pclass~.) + labs(x="Survived") +labs(y="Density of Age")

train$lonely <- factor(ifelse(train$FamilySize>1,"with family","alone"))
# output1 = train[train$Sex=='female'& train$SibSp == 0,]
output1 = train#[train$Age < 18 & train$Parch>0,]
# labelgraph <- list('Sex'="Sex",'Pclass'="Pclass")
ggplot(output1,aes(x=as.factor(Survived)))+geom_histogram(aes(y=..count../tapply(..count..,..PANEL..,sum)[..PANEL..]),binwidth=0.5)+
  facet_grid(Pclass~.) + labs(y = "Fraction")+ labs(x = "Death/Survived") +labs(title = "Facet Grid")
# ggplot_build(p)
#labeller <- function(variable,value){print(typeof(variable))return(labelgraph[variable])}
ggplot(output1,aes(as.factor(output1$Survived),output1$Age))+geom_violin() + facet_grid(Pclass~lonely)
output2 = train[train$Age<18,]
#length(train[train$Age < 10 & train$lonely == "with family" & train$Survived == 1 & train$Pclass==2,]$PassengerId)
# ggplot(output2,aes(x=as.factor(Survived)))+stat_summary(fun.y = mean, ymin=0, ymax=1, geom="bar", size=4) + facet_grid(Pclass~alone)
ggplot(output2,aes(x=as.factor(Survived)))+geom_histogram(aes(y=..count../tapply(..count..,..PANEL..,sum)[..PANEL..]),binwidth=0.5) + facet_grid(Pclass~lonely) + labs(y="Fraction")

output1 = train[train$Age > 50,]
p <- ggplot(output1,aes(x=as.factor(Survived)))+geom_histogram(aes(y=..count../tapply(..count..,..PANEL..,sum)[..PANEL..]),binwidth=0.5)+
  facet_grid(Pclass~Sex) + labs(y = "Fraction")+ labs(x = "Death/Survived") +labs(title = "Facet Grid for Age > 50")
q <- ggplot_build(p)
q
print(q$data[[1]])

output1 = train
p <- ggplot(output1,aes(x=as.factor(Survived)))+geom_histogram(aes(y=..count../tapply(..count..,..PANEL..,sum)[..PANEL..]),binwidth=0.5)+
  facet_grid(Pclass~Embarked) + labs(y = "Fraction")+ labs(x = "Death/Survived") +labs(title = "Facet Grid for Age>60")
q <- ggplot_build(p)
q
print(q$data[[1]])

output1 = train[train$Parch>0 & train$Age<=18,]
p <- ggplot(output1,aes(x=as.factor(Survived)))+geom_histogram(aes(y=..count../tapply(..count..,..PANEL..,sum)[..PANEL..]),binwidth=0.5)+
  facet_grid(Pclass~Sex) + labs(y = "Fraction")+ labs(x = "Death/Survived") +labs(title = "Facet Grid for Parch>0 & Age<18")
q <- ggplot_build(p)
q
print(q$data[[1]])

output1 = train[train$Age>18 & train$Age<50 ,] #& train$lonely=='with family'
p <- ggplot(output1,aes(x=as.factor(Survived)))+geom_histogram(aes(y=..count../tapply(..count..,..PANEL..,sum)[..PANEL..]),binwidth=0.5)+
  facet_grid(Pclass~Sex) + labs(y = "Fraction")+ labs(x = "Death/Survived") +labs(title = "Facet Grid")
q <- ggplot_build(p)
q
print(q$data[[1]])

ggplot(output1,aes(as.factor(output1$Survived),output1$Fare))+geom_violin() #+ facet_grid(Pclass~.)

ggplot(output1,aes(x=output1$Age))+geom_density() + facet_grid(Pclass~Survived) +labs(title= "Density of Age ") + labs(x="Age")
# output1 = train[train$Survived]
ggplot(train, aes(factor(train$Survived), train$Age)) + geom_violin() + facet_grid(Pclass~Sex) + labs(y = "Age")+ labs(x = "Death/Survived")
#######################################################################################
# set.seed(415)
# fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize,
#                     data=train, importance=TRUE, ntree=2000)
# 
# varImpPlot(fit)
# 
# Prediction <- predict(fit, test)
# submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
# write.csv(submit, file = "result.csv", row.names = FALSE)

train(train[ ,colnames(train) %in% c("Pclass", "Sex","Age", "SibSp" , "FamilyID","Parch" ,  "Embarked" , "Title" , "FamilySize" , "Mother","Child2","Old1","Old2")],as.factor(train$Survived),method = "cforest", trControl = trainControl(method = "oob"), metric = "Accuracy")

tuneRF(train[ ,colnames(train) %in% c("Pclass", "Sex","Age", "SibSp" , "Parch" , "Fare" , "Embarked" , "Title" , "FamilySize" , "FamilyID2","Mother","Child1","Child2","Old1")], as.factor(train$Survived),
        mtryStart = 2, ntreeTry = 500,improve = 0.001, plot = TRUE, doBest = FALSE, stepFactor = 0.5, trace=TRUE)

set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID + Mother + Child2 +midage +firstcls ,
               data = train, controls=cforest_unbiased(ntree=2000,mtry=4))

Prediction <- predict(fit,test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "result.csv", row.names = FALSE)

