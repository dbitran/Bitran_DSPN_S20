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