## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 1st Function:This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # initializing the inverse as NULL
    i <- NULL  
    # function to set a matrix
    set <- function( matrix ) {
        m <<- matrix
        i <<- NULL
    }
    # function get the matrix
    get <- function() {
        m
    }
    # function to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    # function to get the inverse of the matrix
    getInverse <- function() {
        i
    }
    # return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Write a short comment describing this function
## 2nd Functioncomputes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    
    
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    # get the matrix from our object
    data <- x$get()
    # calculate the inverse by using a matrix multiplication
    m <- solve(data) %*% data
    # set the inverse of the object
    x$setInverse(m)
    # return the matrix
    m
}
