

library(dplyr)
library(ggplot2)
library(ggthemes)
library(Amelia)
library(ROCR)

setwd('/home/matthew/R/kaggle/titanic')

#reading training set in

full <- read.csv('train_titanic.csv', header = T, na.strings=c(""), stringsAsFactors = F)
#test <- read.csv('test.csv', header = T, na.strings=c(""), stringsAsFactors = F)
#full <- bind_rows(test,training)

#feature engineering - name

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
               'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')


full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'
 

full$Surname <- sapply(full$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])
full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste(full$Surname, full$Fsize, sep='_')

# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'


ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
   geom_bar(stat='count', position='dodge') +
   scale_x_continuous(breaks=c(1:11)) +
   labs(x = 'Family Size') +
   theme_few()

#deal with missing embarkment observations

embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) 
  theme_few()
  
  full$Embarked[c(62, 830)] <- 'C'
#missing values

  
  
  
  
  