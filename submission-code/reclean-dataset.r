library(lubridate)
train<- read.csv('train.csv', header = T, stringsAsFactors = F, na = 0)
library(lubridate)
train$Hour <- hour(train$DateTime)
train$Month <- month(train$DateTime)
train$Weekdays <- wday(train$DateTime)
train$Year <- year(train$DateTime)


train$Name = ifelse(nchar(train$Name) == 0, "Nameless", train$Name)
train$Name=ifelse(train$Name == 'Nameless', 0, 1)
train$OutcomeSubtype<-NULL
train$AnimalID<-NULL

train$TimeValue = sapply(train$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][1])
# retrive the time unit,like years, months, days
train$UnitofTime = sapply(train$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][2])
#modify the plural number into singular. for example 'years' to 'year'
train$UnitofTime = gsub('s', '', train$UnitofTime)
train$UnitofTime = as.factor(train$UnitofTime) # convert the unit of time into factor
train$TimeValue = as.numeric(train$TimeValue) #convert the value of time into interger
#compute the age of each animal in day unit by converting TimeValue in days using 
#the appropriate multiplier based on different unit.
multiplier = ifelse(train$UnitofTime == 'day', 1, ifelse(train$UnitofTime == 'week', 7, 
                                                         ifelse(train$UnitofTime == 'month', 30, ifelse(train$UnitofTime == 'year', 365, NA))))
train$AgeinDays = multiplier * train$TimeValue

#Sex translate
train$Sex <- ifelse(grepl('Male', train$SexuponOutcome), 'Male',
                    ifelse(grepl('Female', train$SexuponOutcome), 'Female', 'Male'))

train$Sex <- ifelse(train$Sex=='Male',1,0) 


# mix or pure
train$isMixBreed <- ifelse(grepl("Mix", train$Breed), 'MixBreed', 
                           ifelse(grepl("/", train$Breed), 'MixBreed', 'PureBreed'))
train$isMixBreed <- ifelse(train$isMixBreed=='MixBreed',1,0)
train$isMixColor <- ifelse(grepl('/' , train$Color), 'MixColor',
                           ifelse(grepl('Mix' , train$Color), 'MixColor','PureColor'))
train$isMixColor <- ifelse(train$isMixColor=='MixColor',1,0)

#intact or neutered
train$isIntact <- ifelse(grepl('Intact', train$SexuponOutcome), 'Intact',
                         ifelse(grepl('Unknown', train$SexuponOutcome), 'Neutered', 'Neutered')) 
train$isIntact <- ifelse(train$isIntact=='Intact',1,0)    
train$AnimalType <- ifelse(train$AnimalType=='Dog',1,0)

#Outcome translatetion
train$OutcomeType <- ifelse(train$OutcomeType=='Adoption', 1,
                               ifelse(train$OutcomeType=='Died', 2,
                                      ifelse(train$OutcomeType == 'Euthanasia', 3, 
                                             ifelse(train$OutcomeType == 'Return_to_owner',4, 5))))

#Delete unuseful variables
train$TimeValue<-NULL
train$UnitofTime<-NULL
train$AgeuponOutcome<-NULL
train$SexuponOutcome<-NULL
train$DateTime=as.POSIXct(train$DateTime)

#breed  (work!!)
all_breeds <- unique(train$Breed)
breed_words <- gsub('/',' ',all_breeds,ignore.case=T)
breed_words2 <- gsub('Mix','',breed_words,ignore.case=T)
breed_words1 <- unique(unlist(strsplit(breed_words2, " ")))
for (breed in breed_words1){
  train[breed] <- as.numeric(grepl(breed, train$Breed))
}
#trainframe=data.frame(train)
#train$Breed = NULL


#color
all_train_colors <- unique(train$Color)
color_words <- gsub('/',' ',all_train_colors,ignore.case=T)
color_words1 <- unique(unlist(strsplit(color_words, " ")))
for (color in color_words1){
  train[color] <- as.numeric(grepl(color, train$Color))
}
train$Breed<-NULL
train$Color<-NULL