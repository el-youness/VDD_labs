# Author: Lionel Fillatre
# Lecture: Data Valorization
# This code is a full implementation of the cross-validation
# We can use cv.glm instead of the full implementation 

# Fix the random generator seed
seed <- 1809
set.seed(seed)

library(caret) # For the confusion matrix

# Read CSV into R
DataSet <- read.csv(file="bank-additional.csv", header=TRUE, sep=";")
n <- nrow(DataSet)
# Select only the relevant variables
ReducedDataSet <- subset(DataSet, select=c("age", "job", "marital", "duration", "y"))

# Define the folds
n_folds <- 10 # number of folds
folds_i <- sample(rep(1:n_folds, length.out = n)) # generate the folds

# Initialization of the outputs
accuracy <- matrix(NA, nrow = n_folds, ncol = 1)
sensitivity <- matrix(NA, nrow = n_folds, ncol = 1)
specificity <- matrix(NA, nrow = n_folds, ncol = 1)

for (k in 1:n_folds) {
  test_i <- which(folds_i == k)

  # Prepare the fold datasets (train and test)
  trainfold <- ReducedDataSet[-test_i, ]
  testfold <- ReducedDataSet[test_i, ]
  
  # Now feed it to glm
  fitted_model <- glm( y ~  . , data=trainfold, family="binomial")
  
  # Test the model on test data
  predictions <- predict(fitted_model, testfold, type='response')
  predictions.results <- ifelse(predictions > 0.5, "yes", "no") # transform the output in "yes/no"
  
  # Compute the confusion matrix and the accuracy
  cM <- confusionMatrix(as.factor(predictions.results), 
                        as.factor(testfold$y), 
                        positive = "yes")
  accuracy[k] <- cM$overall[1]
  sensitivity[k] <- cM$byClass[1] # true positive rate
  specificity[k] <- cM$byClass[2] # true negative rate
}


# Plot the accuracy, the false negative rate and the false positive rate
par(mfrow = c(2, 2)) # plot the results on the same figure

plot(seq(1,n_folds), accuracy, 
     type = "l", lwd = 2, col = "blue", 
     ylab = "Accuracy", xlab = "Fold number", 
     main = paste0(n_folds, "-fold Cross-Validation"), 
     ylim = c(0.01, 1) 
     )

plot(seq(1,n_folds), 1-sensitivity, 
     type = "l", lwd = 2, col = "red", 
     ylab = "False negative rate", xlab = "Fold number", 
     main = paste0(n_folds, "-fold Cross-Validation"), 
     ylim = c(0.01, 1) 
)

plot(seq(1,n_folds), 1-specificity, 
     type = "l", lwd = 2, col = "green", 
     ylab = "False positive rate", xlab = "Fold number", 
     main = paste0(n_folds, "-fold Cross-Validation"), 
     ylim = c(0.01, 1) 
)
