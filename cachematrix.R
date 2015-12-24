## Define a matrix object that can cache its inverse.
## makeCacheMatrix creates the object and defines the functions.
## cacheSolve performs the inverse calculation and caches the result in the object.

## Defines the get and set functions for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(newInverse) inverse <<- newInverse
    getinverse <- function() inverse
    
    #Return the list of functions so they can be called later
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Retrieves the cached inverse of the matrix, if any, 
## or calculates and caches the inverse if need be.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse() #See if x already has a cached inverse
    if(!is.null(inverse)) { #Inverse has already been stored, so return it
        message("getting cached data")
        return(inverse)
    }
    #No inverse stored -- calculate, set, and return it
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
