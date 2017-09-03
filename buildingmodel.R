#model 1:random forest
library(randomForest)
library(e1071)
set.seed(731)
newtrain <- read_csv("~/Documents/2017Spring/Data Analytic/term-project/newtrain.csv")
newtest <- read_csv("~/Documents/2017Spring/Data Analytic/term-project/newtest.csv")
newtrain$OutcomeType=as.factor(newtrain$OutcomeType)
modle4 <- randomForest(OutcomeType ~ Name + AnimalType + Breed + Color + AgeStage + isIntact + Sex + AcceptedWeekday + AcceptedDayTime,data=newtrain)
pre <- predict(modelrf, newtest)
prob5 <- data.frame(ID = newtest$ID, pre)

#model 2
library(reshape2)
newtest <- read_csv("~/Documents/2017Spring/Data Analytic/term-project/newtest.csv")
newtrain <- read_csv("~/Documents/2017Spring/Data Analytic/term-project/newtrain.csv")
newtrain<- read.csv('newtrain.csv', header = T, stringsAsFactors = T)
newtest<- read.csv('newtest.csv', header = T, stringsAsFactors = T)
model <- multinom(OutcomeType ~ Name + AnimalType + Breed + Color + AgeStage + isIntact + Sex + AcceptedWeekday + AcceptedDayTime, data = newtrain)
pred <- predict(model, newdata = newtest, "probs")
sub <- data.frame(ID = newtest$ID, pred)

#model 3
library(VGAM)
newtrain<- read.csv('newtrain.csv', header = T, stringsAsFactors = T)
modle2 <- vglm(OutcomeType ~ Name + AnimalType + Breed + Color + AgeStage + isIntact + Sex + AcceptedWeekday + 
                 AcceptedDayTime,family=multinomial,data=newtrain) 

prob <- predict(modle2,newdata=newtest,type="response")

sub2 <- data.frame(ID = newtest$ID, prob)

#write.csv(sub2, file="submission2.csv")