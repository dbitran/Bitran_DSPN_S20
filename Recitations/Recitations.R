#Recitation 3
data_point <- 3
print (data_point)
vector_1 = c(data_point, data_point + 2)
Vector_2 = c(data_point+4, data_point+6)

#matrix is said to be of dimension (mxn); m = number of rows; n= number of columns

data <- seq(1:10) 
nrow <- 2
ncol <- 5

sample_matrix_by_col <- matrix(data, nrow, ncol, byrow = 0)
sample_matrix_by_row <- matrix(data, nrow, ncol, byrow = 1)

head(sample_matrix_by_col)
head(sample_matrix_by_row)

square_matrix_4 <- matrix(1:16, nrow=4)
square_matrix_4

square_matrix_10 <- matrix(1:100, nrow=10)
square_matrix_10

symmetric_matrix <- matrix(1:16, nrow=4)
lower.tri(symmetric_matrix)
lower_tri <- lower.tri(symmetric_matrix)
symmetric_matrix[lower_tri] <- t(symmetric_matrix)[lower_tri]
symmetric_matrix
t(symmetric_matrix) == symmetric_matrix

#identity matrix
mat <- matrix(0, 5, 5)
diag(mat) <- 1
mat 

###LOOK UP DETERMINANTS OF A MATRIX


#Intro calc
#integral for domain range from 0 to 1 
a <- runif(1000, 0, 1)
b <- a^2(1-0)
mean(b)

#integral for domain range from 2 to 4 
a <- runif(1000, 0, 1)
b <- a^2(4-2)
mean(b)

###RECITATION 6
install.packages("ISLR")
library(tidyverse)
library(ISLR)
library(MASS)

fix(Boston)
ses_input_values <- data.frame(lstat=c(5,10,15))
prediction_table <- data.frame(predict(lm.fit, ses_input_values, prediction_table))

#Working with categorical predictors
install.packages("car")
library(car)