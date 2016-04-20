## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function 
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {    ## define the argument with default mode of "matrix"
mat_i <- NULL						## initialize mat_i as NULL; will hold value of matrix inverse	
        set <- function(y) {
                x <<- y					##Assigns a new value of matrix in the parent env
                mat_i <<- NULL			 ## if there is a new matrix, reset mat_i to NULL
}
get<-function() x						##returns value of the matrix argument
setmatrix<-function(solve) mat_i<<- solve		## assigns value of inv in parent environment
getmatrix<-function() mat_i				## gets the value of inv where called
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

##To execute
##a <- makeCacheMatrix()
## a$set(matrix(1:4,2,2)) or any square matrix
##cacheSolve(a)
