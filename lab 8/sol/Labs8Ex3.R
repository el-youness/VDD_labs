# Author: Lionel Fillatre
# Lecture: Data Valorization

library('e1071')

#---------------------------------------------- 
data(Titanic) # load the data set
df<-as.data.frame(Titanic)  # Convert into a data frame

#----------------------------------------------
numberPassengers <- sum(df$Freq) # Number of passengers

numberPassengersSurvived <- sum(df[df$Survived == 'Yes',]$Freq) # Number of survival passengers
priorSurvived <- numberPassengersSurvived / numberPassengers # Prior for passengers survival
priorNotSurvived <- 1-priorSurvived

# Replicate the records to obtain a full data set: one passenger=one record
dfrep <- data.frame(Class = rep(df$Class, df$Freq),
                       Sex = rep(df$Sex, df$Freq),
                       Age = rep(df$Age, df$Freq),
                       Survived = rep(df$Survived, df$Freq)
                       )

# Just to verify the code, we compare the numerical values to the ones computed by "naiveBayes" built-in function.
# Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(Survived ~., data=dfrep)
# What does the model say? Print the model summary
Naive_Bayes_Model
# Prediction on the dataset
NB_Predictions=predict(Naive_Bayes_Model,dfrep)
# Confusion matrix to check accuracy
print(table(NB_Predictions,dfrep$Survived))



# Function to retrieve a row of a dataframe
rows = function(x) lapply(seq_len(nrow(x)), function(i) lapply(x,"[",i))

# ===============================================================
# Function Naive Bayes Test
naiveBayesDV <- function(dfrepTrain,dfTest){
  #---------------------------------------------- 
  # Train the model
  #
  priorYes <- dim(dfrepTrain[dfrepTrain$Survived=='Yes',])[1] / dim(dfrepTrain)[1] # Prior for passengers survival within the training data set
  priorNo <- 1-priorYes
  table.Prior <- as.table(cbind(priorNo,priorYes))
  colnames(table.Prior) <- c("No","Yes")
  rownames(table.Prior) <- c("")
  names(attributes(table.Prior)$dimnames) <- c("Survived","")
  
  
  # Estimation of the feature distribution, assuming independence between features
  dfrepTrainYes <- dfrepTrain[dfrepTrain$Survived=='Yes',] # surival training passengers
  dfrepTrainNo <- dfrepTrain[dfrepTrain$Survived=='No',] # surival testing passengers
  
  
  # Distribution of feature 'Class'
  tableClassYes <- table(dfrepTrainYes$Class)/dim(dfrepTrainYes)[1]
  tableClassNo <- table(dfrepTrainNo$Class)/dim(dfrepTrainNo)[1]
  table.Class<-as.table(rbind(tableClassNo,tableClassYes))
  rownames(table.Class) <- c("No", "Yes")
  names(attributes(table.Class)$dimnames) <- c("Survived","Class")
  
  # Distribution of feature 'Sex'
  tableSexYes <- table(dfrepTrainYes$Sex)/dim(dfrepTrainYes)[1]
  tableSexNo <- table(dfrepTrainNo$Sex)/dim(dfrepTrainNo)[1]
  table.Sex<-as.table(rbind(tableSexNo,tableSexYes))
  rownames(table.Sex) <- c("No", "Yes")
  names(attributes(table.Sex)$dimnames) <- c("Survived","Sex")
  
  # Distribution of feature 'Age'
  tableAgeYes <- table(dfrepTrainYes$Age)/dim(dfrepTrainYes)[1]
  tableAgeNo <- table(dfrepTrainNo$Age)/dim(dfrepTrainNo)[1]
  table.Age<-as.table(rbind(tableAgeNo,tableAgeYes))
  rownames(table.Age) <- c("No", "Yes")
  names(attributes(table.Age)$dimnames) <- c("Survived","Age")
  
  # Convert tables into dataframes
  dfPrior <- as.data.frame(table.Prior)
  dfClass <- as.data.frame(table.Class)
  dfAge <- as.data.frame(table.Age)
  dfSex <- as.data.frame(table.Sex)
  
  # Classify the train samples
  vectorDecision <- c()
  labelsNoYes <- c("No","Yes")
  for (dfrow in rows(dfrepTrain)) {
    passClass <- dfrow$Class  
    passAge <- dfrow$Age
    passSurvived <- dfrow$Survived
    passSex <- dfrow$Sex
    c<-dfClass[dfClass$Class==passClass,]
    a<-dfAge[dfAge$Age==passAge,]
    s<-dfSex[dfSex$Sex==passSex,]
    proba <- c$Freq*a$Freq*s$Freq*dfPrior$Freq # compute the probability to take the decision

    passDecision <- labelsNoYes[which.max(proba)]
    vectorDecision <- c(vectorDecision, passDecision)
  }
  
  # Classify the test samples
  vectorDecisionTest <- c()
  for (dfrow in rows(dfTest)) {
    passClass <- dfrow$Class  
    passAge <- dfrow$Age
    passSurvived <- dfrow$Survived
    passSex <- dfrow$Sex
    c<-dfClass[dfClass$Class==passClass,]
    a<-dfAge[dfAge$Age==passAge,]
    s<-dfSex[dfSex$Sex==passSex,]
    proba <- c$Freq*a$Freq*s$Freq*dfPrior$Freq
    passDecision <- labelsNoYes[which.max(proba)]
    vectorDecisionTest <- c(vectorDecisionTest, passDecision)
  }  

  # Compute the confusion matrices
  trainLabels <- dfrepTrain[,"Survived"]
  testLabels <- dfTest[,"Survived"]
  CMtrain <- table(factor(vectorDecision),trainLabels)
  CMtest <- table(factor(vectorDecisionTest),testLabels)
  
  output <- list(CMtrain,CMtest)

  return(output)
}

#----------------------------------------------
# Main code for the k-folds

# Define the folds
n_folds <- 10 # number of folds
folds_i <- sample(rep(1:n_folds, length.out = numberPassengers)) # generate the folds

# Initialization of the outputs
accuracy <- matrix(NA, nrow = n_folds, ncol = 1)
sensitivity <- matrix(NA, nrow = n_folds, ncol = 1)
specificity <- matrix(NA, nrow = n_folds, ncol = 1)

accuracy_test <- matrix(NA, nrow = n_folds, ncol = 1)
sensitivity_test <- matrix(NA, nrow = n_folds, ncol = 1)
specificity_test <- matrix(NA, nrow = n_folds, ncol = 1)

for (k in 1:n_folds) {
  test_i <- which(folds_i == k)
  
  # Prepare the fold datasets (train and test)
  trainfold <- dfrep[-test_i, ]
  testfold <- dfrep[test_i, ]
  
  N_k <- naiveBayesDV(trainfold,testfold)
  print(N_k)
  CM_k_train <- N_k[[1]]
  CM_k_test <- N_k[[2]]
  sensitivity[k] <- CM_k_train[2,2]/(CM_k_train[1,2]+CM_k_train[2,2]) # true positive rate
  specificity[k] <- CM_k_train[1,1]/(CM_k_train[1,1]+CM_k_train[2,1]) # true negative rate
  accuracy[k] <- (CM_k_train[1,1]+CM_k_train[2,2])/sum(CM_k_train) # accuracy
  
  sensitivity_test[k] <- CM_k_test[2,2]/(CM_k_test[1,2]+CM_k_test[2,2]) # true positive rate
  specificity_test[k] <- CM_k_test[1,1]/(CM_k_test[1,1]+CM_k_test[2,1]) # true negative rate
  accuracy_test[k] <- (CM_k_test[1,1]+CM_k_test[2,2])/sum(CM_k_test) # accuracy
  
}  

# Mean and variance of the results
sensitivity.mean <- mean(sensitivity)
sensitivity.std <- sd(sensitivity)

specificity.mean <- mean(specificity)
specificity.std <- sd(specificity)

accuracy.mean <- mean(accuracy)
accuracy.std <- sd(accuracy)


msg1 <- sprintf("\nAccuracy over the train folds: mean = %0.3f, std = %0.3f", accuracy.mean, accuracy.std)
msg2 <- sprintf("\nSpecificity over the train folds: mean = %0.3f, std = %0.3f", specificity.mean, specificity.std)
msg3 <- sprintf("\nSensitivity over the train folds: mean = %0.3f, std = %0.3f", sensitivity.mean, sensitivity.std)
cat(msg1)
cat(msg2)
cat(msg3)

msg1 <- sprintf("\n\nAccuracy over the test folds: mean = %0.3f, std = %0.3f", mean(accuracy_test), sd(accuracy_test))
msg2 <- sprintf("\nSpecificity over the test folds: mean = %0.3f, std = %0.3f", mean(specificity_test), sd(specificity_test))
msg3 <- sprintf("\nSensitivity over the test folds: mean = %0.3f, std = %0.3f", mean(sensitivity_test), sd(sensitivity_test))

cat(msg1)
cat(msg2)
cat(msg3)

#----------------------------------------------
# Plot the results

# Plot the accuracy, the false negative rate and the false positive rate
par(mfrow = c(2, 2)) # plot the results on the same figure

plot(seq(1,n_folds), accuracy, 
     type = "l", lwd = 2, col = "red", 
     ylab = "Accuracy", xlab = "Fold number", 
     main = paste0(n_folds, "-fold Cross-Validation"), 
     ylim = c(0.01, 1) 
)
lines(seq(1,n_folds),accuracy_test,col="blue",lwd = 2)

# Add legend to plot 
legend("bottomright", 
       inset=.05, 
       cex = 1, 
       title="Legend", 
       c("Train","Test"), 
       horiz=TRUE, 
       lty=c(1,1), 
       lwd=c(2,2), 
       col=c("red","blue"), 
       bg="grey96")


plot(seq(1,n_folds), 1-sensitivity, 
     type = "l", lwd = 2, col = "red", 
     ylab = "False negative rate", xlab = "Fold number", 
     main = paste0(n_folds, "-fold Cross-Validation"), 
     ylim = c(0.01, 1) 
)
lines(seq(1,n_folds),1-sensitivity_test,col="blue",lwd = 2)

plot(seq(1,n_folds), 1-specificity, 
     type = "l", lwd = 2, col = "red", 
     ylab = "False positive rate", xlab = "Fold number", 
     main = paste0(n_folds, "-fold Cross-Validation"), 
     ylim = c(0.01, 1) 
)
lines(seq(1,n_folds),1-specificity_test,col="blue",lwd = 2)


