library(readr)
test <- read_csv("~/Documents/2017Spring/Data Analytic/term-project/test.csv", 
                 na = "0")
test$Hour <- hour(test$DateTime)
test$Month <- month(test$DateTime)
test$Weekdays <- wday(test$DateTime)
test$Year <- year(test$DateTime)

#test<- read.csv('test.csv', header = T, stringsAsFactors = F, na = 0)
test$Name = ifelse(nchar(test$Name) == 0, "Nameless", test$Name)
test$Name=ifelse(test$Name == 'Nameless', 0, 1)


test$TimeValue = sapply(test$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][1])
# retrive the time unit,like years, months, days
test$UnitofTime = sapply(test$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][2])
#modify the plural number into singular. for example 'years' to 'year'
test$UnitofTime = gsub('s', '', test$UnitofTime)
test$UnitofTime = as.factor(test$UnitofTime) # convert the unit of time into factor
test$TimeValue = as.numeric(test$TimeValue) #convert the value of time into interger
#compute the age of each animal in day unit by converting TimeValue in days using 
#the appropriate multiplier based on different unit.
multiplier = ifelse(test$UnitofTime == 'day', 1, ifelse(test$UnitofTime == 'week', 7, 
                                                         ifelse(test$UnitofTime == 'month', 30, ifelse(test$UnitofTime == 'year', 365, NA))))
test$AgeinDays = multiplier * test$TimeValue

#Sex translate
test$Sex <- ifelse(grepl('Male', test$SexuponOutcome), 'Male',
                    ifelse(grepl('Female', test$SexuponOutcome), 'Female', 'Male'))
test$Sex <- ifelse(test$Sex=='Male',1,0) 


# mix or pure
test$isMixBreed <- ifelse(grepl("Mix", test$Breed), 'MixBreed', 
                           ifelse(grepl("/", test$Breed), 'MixBreed', 'PureBreed'))
test$isMixBreed <- ifelse(test$isMixBreed=='MixBreed',1,0)
test$isMixColor <- ifelse(grepl('/' , test$Color), 'MixColor',
                           ifelse(grepl('Mix' , test$Color), 'MixColor','PureColor'))
test$isMixColor <- ifelse(test$isMixColor=='MixColor',1,0)

#intact or neutered
test$isIntact <- ifelse(grepl('Intact', test$SexuponOutcome), 'Intact',
                         ifelse(grepl('Unknown', test$SexuponOutcome), 'Neutered', 'Neutered')) 
test$isIntact <- ifelse(test$isIntact=='Intact',1,0)    

test$AnimalType <- ifelse(test$AnimalType=='Dog',1,0)

#Delete unuseful variables
test$TimeValue<-NULL
test$UnitofTime<-NULL
test$AgeuponOutcome<-NULL
test$SexuponOutcome<-NULL
test$DateTime=as.POSIXct(test$DateTime)

#breed  (work!!!!!!)
all_breeds <- unique(test$Breed)
breed_words <- gsub('/',' ',all_breeds,ignore.case=T)
breed_words2 <- gsub('Mix','',breed_words,ignore.case=T)
breed_words1 <- unique(unlist(strsplit(breed_words2, " ")))
for (breed in breed_words1){
  test[breed] <- as.numeric(grepl(breed, test$Breed))
}
for (breed in all_breeds){
  test[breed]<-as.numeric(grepl(breed, test$Breed))
}
#trainframe=data.frame(train)
#train$Breed = NULL


#color
all_test_colors <- unique(test$Color)
color_words <- gsub('/',' ',all_test_colors,ignore.case=T)
color_words1 <- unique(unlist(strsplit(color_words, " ")))
for (color in color_words1){
  test[color] <- as.numeric(grepl(color, test$Color))
}
for (color in all_train_colors){
  test[color]<-as.numeric(grepl(color, test$Color))
}

test$Breed<-NULL
test$Color<-NULL