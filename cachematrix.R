##    function description :
## 	makeCacheMatrix creates a special matrix object
##    get method - to get the matrix in context
##    set method - to set the matrix in context 
##    getinverse - to get the inverse of the matrix in context
##    setinverse - to set the inverse of the matrix in context
##    setmethod nullifies the inverse matrix variable (inv_mat) when the matrix is modified. This condition is checked in cacheSolve to recompute Inverse  of the Matrix



makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL
    set <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }
    get <- function() x
    setinverse<- function(inversematrix) inv_mat <<-inversematrix
    getinverse <- function() inv_mat
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
 
##    function description :
##    cacheSolve returns the inverse of the matrix
## 	cacheSolve  computes the inverse of the matrix if the cached version does not exist
## 	or,  returns the cached version of the inverse of matrix  
##    Input : x = object of makeCacheMatrix 
##    Output : Inverse of the matrix in context
## 	inv_mat value obtained by  $getinverse() method is checked to validate if the cached version of inverse to be returned
##    If the cached version is no longer valid, Inverse is recomputed and Cached 


cacheSolve <- function(x, ...) {
    inv_mat <- x$getinverse()
    if (!is.null(inv_mat)) {
        message("Retrieving Cached Version of the Inverse of the Matrix")
        return(inv_mat)
    } else {
        inv_mat <- solve(x$get())
        x$setinverse(inv_mat)
        return(inv_mat)
    }
} 

#Below Commands Run in Succession with Displays the Inverse of the Matrix, Computes inverse for first time and returns from Cache next time onwards

#create special matrix object - Matrix 10 X 10 is just for testing, this could be of larger dimensions also 
#Z<-makeCacheMatrix(matrix(rnorm(100),10,10))
#invoke Inverse Matrix Calculation - Fresh computation is done for the first time
#cacheSolve(Z)
#invoke Inverse Matrix Calculation - Inverse Matrix Returned from the Cache
#cacheSolve(Z)


