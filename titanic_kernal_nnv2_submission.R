

library(dplyr)
library(ggplot2)
library(ggthemes)
library(Amelia)
library(ROCR)
library(mice)
library(randomForest)

setwd('/home/matthew/R/kaggle/titanic')

#reading training set in

train <- read.csv('train_titanic.csv', header = T, na.strings=c(""), stringsAsFactors = F)

test <- read.csv('test.csv', header = T, na.strings=c(""), stringsAsFactors = F)

test$Survived <- 0

full <- rbind(train,test)

# Feature Engineering
## What's in a name?

# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Show title counts by sex
table(full$Sex, full$Title)

# Titles with very low cell counts to be combined to "rare" level
rare <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare]  <- 'Rare'

# Show title counts by sex again
table(full$Sex, full$Title)

# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

# Create a family size variable including the passenger themselves
full$Fsize <- as.numeric(full$SibSp + full$Parch + 1)

# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')

# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# Show family size by survival using a mosaic plot
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)

# Create a Deck variable. Get passenger deck A - F:
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

# Passengers 62 and 830 are missing Embarkment
full[c(62, 830), 'Embarked']

# Get rid of our missing passenger IDs
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2)  +
  theme_few()

# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full$Embarked[c(62, 830)] <- 'C'


# Set a random seed
set.seed(200)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)


# Replace Age variable from the mice model.
full$Age <- mice_output$Age

# Create the column child, and indicate whether child or adult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

# Adding Mother variable
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

#let's plit back into the train and test set
train <- full[1:800,]
test <- full[801:891,]

NN <- full[1:891,]

train1 <- subset(train, select = c(Survived, Pclass, Sex, Age, 
                                  Fare, Title))
test1 <- subset(test, select = c(Survived, Pclass, Sex, Age, 
                                Fare, Title))

#############################################################################
#additional variables

train1 <- subset(train, select = c(Survived, Pclass,  Sex, Age,  SibSp,  Parch,  
                                   Fare,  Embarked,  Title,  
                                   FsizeD,  Child,  Mother))
test1 <- subset(test, select = c(Survived, Pclass,  Sex, Age,  SibSp,  Parch,  
                  Fare,  Embarked,  Title,  
                  FsizeD,  Child,  Mother))
m <- model.matrix( 
  ~ Survived + Pclass +  Sex+ Age +  SibSp +Parch +  
    Fare  +  Embarked +  Title +  
    FsizeD +  Child +  Mother,data = train1)

nn <- neuralnet( 
  Survived ~ Pclass + Sexmale + Age + SibSp + Parch + Fare +EmbarkedQ + EmbarkedS + TitleMiss  + 
    TitleMr + TitleMrs + TitleRare + FsizeDsingleton + FsizeDsmall + ChildChild, 
  data=m, hidden=3, threshold=0.01, linear.output = F)

nn2 <- neuralnet( 
  Survived ~ Pclass + Sexmale + Age + SibSp + Parch + Fare +EmbarkedQ + EmbarkedS + TitleMiss  + 
    TitleMr + TitleMrs + TitleRare + FsizeDsingleton + FsizeDsmall + ChildChild, 
  data=m, hidden=4, threshold=0.01, linear.output = F)

nn3 <- neuralnet( 
  Survived ~ Pclass + Sexmale + Age + SibSp + Parch + Fare +EmbarkedQ + EmbarkedS + TitleMiss  + 
    TitleMr + TitleMrs + TitleRare + FsizeDsingleton + FsizeDsmall + ChildChild, 
  data=m, hidden=5, threshold=0.01, linear.output = F)

nn4 <- neuralnet( 
  Survived ~ Pclass + Sexmale + Age + SibSp + Parch + Fare +EmbarkedQ + EmbarkedS + TitleMiss  + 
    TitleMr + TitleMrs + TitleRare + FsizeDsingleton + FsizeDsmall + ChildChild, 
  data=m, hidden=6, threshold=0.01, linear.output = F)

nn5 <- neuralnet( 
  Survived ~ Pclass + Sexmale + Age + SibSp + Parch + Fare +EmbarkedQ + EmbarkedS + TitleMiss  + 
    TitleMr + TitleMrs + TitleRare + FsizeDsingleton + FsizeDsmall + ChildChild, 
  data=m, hidden=7, threshold=0.01, linear.output = F)

m1 <- model.matrix( 
  ~ Survived + Pclass +  Sex+ Age +  SibSp +Parch +  
    Fare  +  Embarked +  Title +  
    FsizeD +  Child +  Mother,data = test1)

res = neuralnet::compute(nn, m1[,c("Pclass","Sexmale","Age","SibSp","Parch","Fare","EmbarkedQ", "EmbarkedS","TitleMiss",   
    "TitleMr","TitleMrs","TitleRare","FsizeDsingleton","FsizeDsmall","ChildChild")])
pred_train = round(res$net.result)

res2 = neuralnet::compute(nn2, m1[,c("Pclass","Sexmale","Age","SibSp","Parch","Fare","EmbarkedQ", "EmbarkedS","TitleMiss",   
    "TitleMr","TitleMrs","TitleRare","FsizeDsingleton","FsizeDsmall","ChildChild")])
pred_train2 = round(res2$net.result)

res3 = neuralnet::compute(nn3, m1[,c("Pclass","Sexmale","Age","SibSp","Parch","Fare","EmbarkedQ", "EmbarkedS","TitleMiss",   
    "TitleMr","TitleMrs","TitleRare","FsizeDsingleton","FsizeDsmall","ChildChild")])
pred_train3 = round(res3$net.result)

res4 = neuralnet::compute(nn4, m1[,c("Pclass","Sexmale","Age","SibSp","Parch","Fare","EmbarkedQ", "EmbarkedS","TitleMiss",   
    "TitleMr","TitleMrs","TitleRare","FsizeDsingleton","FsizeDsmall","ChildChild")])
pred_train4 = round(res4$net.result)

res5 = neuralnet::compute(nn5, m1[,c("Pclass","Sexmale","Age","SibSp","Parch","Fare","EmbarkedQ", "EmbarkedS","TitleMiss",   
    "TitleMr","TitleMrs","TitleRare","FsizeDsingleton","FsizeDsmall","ChildChild")])
pred_train5 = round(res5$net.result)


test_final <- cbind(test1,pred_train)
test_final2 <- cbind(test1,pred_train2)
test_final3 <- cbind(test1,pred_train3)
test_final4 <- cbind(test1,pred_train4)
test_final5 <- cbind(test1,pred_train5)

test_final$correct[test_final$Survived == test_final$pred_train ] <- 1
test_final2$correct[test_final$Survived == test_final$pred_train ] <- 1
test_final3$correct[test_final$Survived == test_final$pred_train ] <- 1
test_final4$correct[test_final$Survived == test_final$pred_train ] <- 1
test_final5$correct[test_final$Survived == test_final$pred_train ] <- 1

paste(sum(test_final$correct, na.rm = TRUE)/91)
paste(sum(test_final2$correct, na.rm = TRUE)/91)
paste(sum(test_final3$correct, na.rm = TRUE)/91)
paste(sum(test_final4$correct, na.rm = TRUE)/91)
paste(sum(test_final5$correct, na.rm = TRUE)/91)


#########################################################################
#transform factors into binary, since NN only uses quantitative variables

m <- model.matrix( 
  ~ Survived + Pclass + Sex + Age + 
    Fare + Title,data = train1)

library(neuralnet)

nn <- neuralnet( 
          Survived ~ Pclass + Sexmale + Age + Fare + TitleMr + TitleMiss + TitleMrs + TitleRare, 
          data=m, hidden=3, threshold=0.01, linear.output = F)

#plot(nn)
#nn$net.result[[1]]
#Let's check accuracy on the test set
m1 <- model.matrix( 
  ~ Survived + Pclass + Sex + Age + 
    Fare + Title,data = test1)



res = neuralnet::compute(nn, m1[,c("Pclass",  "Sexmale", "Age", "Fare", "TitleMr", "TitleMiss",  
                                  "TitleMrs", "TitleRare")])
pred_train = round(res$net.result)

test_final <- cbind(test1,pred_train)

test_final$correct[test_final$Survived == test_final$pred_train ] <- 1

paste(sum(test_final$correct, na.rm = TRUE)/91)


#########################################################################################
test_submission <- full[892:1309,]
test_submission <- subset(test_submission, select = c(Survived, Pclass, Sex, Age, 
                                 Fare, Title, PassengerId))
test_submission1 <- model.matrix( 
  ~ Survived + Pclass + Sex + Age + 
    Fare + Title,data = test_submission)

ts = neuralnet::compute(nn, test_submission1[,c("Pclass",  "Sexmale", "Age", "Fare", "TitleMr", "TitleMiss",  
                                   "TitleMrs", "TitleRare")])
pred_ts = round(ts$net.result)

test_ts <- cbind(test_submission,pred_ts)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test_ts$PassengerId, Survived = test_ts$pred_ts)

#write out csvfile to work directory

write.csv(solution,file='out_nn.csv')


