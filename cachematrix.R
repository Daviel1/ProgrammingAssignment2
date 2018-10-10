## Put comments here that give an overall description of what your
## functions do

## "makeCacheMatrix" returns a list of functions.
## Furthermore, it is designed to store a matrix and cache the value of the inverse in order to 
## avoid potentially time consuming computations for more complex datasets.

makeCacheMatrix <- function(x = matrix()) {
        ## this either holds the cached value or NULL if nothing is cached
        inv <- NULL
        
        ## stores a matrix
        set <- function(y){
        x <<- y
        
        ## as the matrix is assigned a new value, the cached value is cleared
        inv <<- NULL
        }
        
        ## returns the stored matrix
        get <- function()x
        
        ## sets the cache of the given argument
        setinverse <- function (inverse) inv <<-inverse
        
        ## get the cached value
        getinverse <- function () inv
        
        # returns a list. Each named element of the list is a function
        list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}



## Write a short comment describing this function

##This function is designed to calculate the inverse of the matrix that was
##set with "makeCacheMatrix"

cacheSolve <- function(x, ...) {
        
        ##get the cached value
        inv <- x$getinverse()
        
        ## if there is a cached value, store the cached value
        if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
        }
        
        ## If there's no cached value, calculate the inverse matrix
        ## and then store it in the cache
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        
        ## return the inverse 
        inv
        
}
