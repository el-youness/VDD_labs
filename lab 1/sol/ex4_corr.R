# Correction Labs 1 - exercise 4
# to read the data: assume that the file '2008.csv' is in the current directory
# Author: Lionel Fillatre

flights <- read.csv('2008.csv')

#Q1
str(flights) 

#Q2
summary(flights)  

#Q3
dim(flights)

#Q4
nrow(flights)

#Q5
ncol(flights)

#Q6
names(flights)

#Q7
match(TRUE,is.na(flights[1]),nomatch = FALSE) # see help(match) to know how this function works

#Q8
count <- 0
for (i in 1:ncol(flights)){
  if( match(TRUE,is.na(flights[i]),nomatch = FALSE)){
    count <- count+1
  }
}
print(count)

#Q9
count/ncol(flights)

#Q10
name <- c()
for (i in 1:length(flights)){
  if (match(TRUE,is.na(flights[i]),nomatch = FALSE)){
    name<-append(name,names(flights[i]))
  }
}
print(name)

