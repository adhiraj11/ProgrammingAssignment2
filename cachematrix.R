## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function 
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {    ## define the argument with default mode of "matrix"
	  inv <- NULL						## initialize inv as NULL; will hold value of matrix inverse	
        set <- function(y) {
                x <<- y					##Assigns a new value of matrix in the parent env
                inv <<- NULL			 ## if there is a new matrix, reset mat_i to NULL
}
get<-function() x						##returns value of the matrix argument
setmatrix <-function(solve) inv<<- solve		## assigns value of inv in parent environment
getmatrix <-function() inv				## gets the value of inv where called
list(set = set, get = get,
   setmatrix = setmatrix,
   getmatrix = getmatrix)
}

## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 inv <- x$getmatrix()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    matrix<-x$get()
    inv <- solve(matrix, ...)
    x$setmatrix(inv)
    inv
}

##To execute
##a <- makeCacheMatrix()
## a$set(matrix(1:4,2,2)) or any square matrix
##cacheSolve(a)
