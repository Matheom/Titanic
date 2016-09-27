

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

  missmap(full, main = "Missing values vs observed")
  
  data <- subset(full,select=c(2,3,4,5,6,7,8,10,12,13,14,15,16,17))
   
  data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)
   
  missmap(data, main = "Missing values vs observed")
  
  data <- data[!is.na(data$Embarked),]
  
  data_final <- subset(data,select=c(1,2,4,5,6,7,8,9,12,14))
   
  missmap(data_final)
   
    > #begin regression
    
    
  train <- data_final[1:800,]
  test <- data_final[801:889,]
   
  model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
   
  summary(model)
  
  #now we test on the test set
  
  fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8,9,10)),type='response')
  
  fitted.results <- ifelse(fitted.results > 0.5,1,0)
  misClasificError <- mean(fitted.results != test$Survived)
  print(paste('Accuracy',1-misClasificError))
  
  p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8,9,10)), type="response")
  pr <- prediction(p, test$Survived)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  plot(prf)
  
  uc <- performance(pr, measure = "auc"
  auc <- auc@y.values[[1]]
  auc
  
  
  
  
  