newtrain$OutcomeType <- ifelse(newtrain$OutcomeType=='Adoption', 1,
                                ifelse(newtrain$OutcomeType=='Died', 2,
                                       ifelse(newtrain$OutcomeType == 'Euthanasia', 3, 
                                              ifelse(newtrain$OutcomeType == 'Return_to_owner',4, 5))))
newtrain$AnimalType <- ifelse(newtrain$AnimalType=='Dog',1,0)
newtrain$Breed <- ifelse(newtrain$Breed=='MixBreed',1,0)
newtrain$Color <- ifelse(newtrain$Color=='MixColor',1,0)
newtrain$AgeStage <- ifelse(newtrain$AgeStage=='Baby', 1,0)
newtrain$isIntact <- ifelse(newtrain$isIntact=='Intact',1,0)    
newtrain$Sex <- ifelse(newtrain$Sex=='Male',1,0) 
newtrain$AcceptedWeekday <- ifelse(newtrain$AcceptedWeekday=='Saturday', 0,
                                   ifelse(newtrain$AcceptedWeekday=='Sunday', 0, 1))
newtrain$AcceptedDayTime <- ifelse(newtrain$AcceptedDayTime=='night', 0,
                                   ifelse(newtrain$AcceptedDayTime=='mid-night', 0, 1))
                                          


newtest$AnimalType <- ifelse(newtest$AnimalType=='Dog',1,0)
newtest$Breed <- ifelse(newtest$Breed=='MixBreed',1,0)
newtest$Color <- ifelse(newtest$Color=='MixColor',1,0)
newtest$AgeStage <- ifelse(newtest$AgeStage=='Baby', 1, 0)
newtest$isIntact <- ifelse(newtest$isIntact=='Intact',1,0)    
newtest$Sex <- ifelse(newtest$Sex=='Male',1,0) 
newtest$AcceptedWeekday <- ifelse(newtest$AcceptedWeekday=='Saturday', 0,
                                  ifelse(newtest$AcceptedWeekday=='Sunday', 0, 1))
newtest$AcceptedDayTime <- ifelse(newtest$AcceptedDayTime=='night', 0,
                                  ifelse(newtest$AcceptedDayTime=='mid-night', 0, 1))
