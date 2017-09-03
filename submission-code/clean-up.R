library(readr)
#re-load train dataset with setting in empty value as "0"
#cleanup and eliminate NA value in the train dataset and convert it into 0 or 1 value
train<- read.csv('train.csv', header = T, stringsAsFactors = F, na = 0)
train$Name = ifelse(nchar(train$Name) == 0, "Nameless", train$Name)
train$Name=ifelse(train$Name == 'Nameless', 0, 1)
#Replace all null value in the OutcomeSubType colum with 'Other' value
train$OutcomeSubtype = ifelse(nchar(train$OutcomeSubtype) == "0", 'Other', train$OutcomeSubtype)

# there are 18 missing value in AgeuponOutcome column, we find that the  young age stage animals has 
#the most number, so we set the empty value in AgeuponOutcome column is '7 months', which is for the young stage.
train$AgeuponOutcome=ifelse(nchar(train$AgeuponOutcome) == 0, '7 months', train$AgeuponOutcome)
# retrive the time value
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

#then we convert the AgeinDays into more understanable formate.
train$AgeStage <- ifelse(train$AgeinDays > 1 & train$AgeinDays < 180, 'Baby','Adult')
ggplot(train, aes(factor(AgeStage)))+geom_bar(col="black", alpha = .3)

train$Hour <- hour(train$DateTime)
train$Month <- month(train$DateTime)
train$Weekdays <- wday(train$DateTime)
train$Year <- year(train$DateTime)

#In this part we extract the Months of DateTime and translate it into the four seasons.
train$AcceptedSeasons <- ifelse(train$Month > 1 & train$Month < 5, 'Spring',
                                ifelse(train$Month > 4 & train$Month < 8 , 'Summer',
                                       ifelse(train$Month > 7 & train$Month < 11, 'Autumn', 'Winter' )))

#translate the the DateTime columns to the week day value
train$AcceptedWeekday <- weekdays(as.Date(train$DateTime))

#also, maybe the time in day may affect the outcomes of animals, divide it into two types, day and night
train$AcceptedDayTime <- ifelse(train$Hour > 5 & train$Hour < 18, 'day', 'night')

# find the most frequent sex attribute to replace the missing value
ggplot(train, aes(factor(SexuponOutcome)))+geom_bar(col="black", alpha = .3)
# we conclude that the Neutered Male is the most common one
train$SexuponOutcome = ifelse(train$SexuponOutcome == "Unknown", "Neutered Male", 
                              train$SexuponOutcome)
# Use "grepl" to look for "Intact". Neutered and spayed is the same meanings
train$isIntact <- ifelse(grepl('Intact', train$SexuponOutcome), 'Intact',
                         ifelse(grepl('Unknown', train$SexuponOutcome), 'Neutered', 'Neutered')) 
#replace the missing value with the most common Neutered Male.

# Use "grepl" to look for sex
train$Sex <- ifelse(grepl('Male', train$SexuponOutcome), 'Male',
                    ifelse(grepl('Female', train$SexuponOutcome), 'Female', 'Male'))

train$isMixBreed <- ifelse(grepl("Mix", train$Breed), 'MixBreed', 
                           ifelse(grepl("/", train$Breed), 'MixBreed', 'PureBreed'))
train$isMixColor <- ifelse(grepl('/' , train$Color), 'MixColor',
                           ifelse(grepl('Mix' , train$Color), 'MixColor','PureColor'))


