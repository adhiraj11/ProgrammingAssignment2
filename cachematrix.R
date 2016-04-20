## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
mat_i <- NULL
        set <- function(y) {
                x <<- y
                mat_i <<- NULL
}
}

## Write a short comment describing this function

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
 mat_i<-x$getmatrix()
    if(!is.null(mat_i)){
      message("getting cached data")
      return(mat_i)
    }
    matrix<-x$get()
    mat_i<-solve(matrix, ...)
    x$setmatrix(mat_i)
    mat_i
}
