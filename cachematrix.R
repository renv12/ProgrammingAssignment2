## Put comments here that give an overall description of what your
## functions do


#returns a list with a function to:
#    - get the matrix
#    - set the matrix
#    - get the inverse of the matrix
#    - set the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    
    get <- function() x
    
    set <- function(mat)
    {
        #set the local matrix and reset the inverse to NULL
        x <<- mat    ## need to use <<- to set x, inverse in the parent environment (local to the makeCacheMatrix() function)
        inverse <<- NULL
    }
    
    getinverse <- function() inverse
    
    setinverse <- function(i) inverse <<- i
    
    #return the list of functions
    list(get=get, set=set, getinverse = getinverse, setinverse = setinverse)
}


#returns the inverse of the makeCacheMatrix function x
#if inverse is not null, then it will return the previously cached inverse
#if inverse is null, it will calculate the inverse, cache it, and return the inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    #check if value is cached
    inverseValue <- x$getinverse()
    if (!is.null(inverseValue))
    {
        message("getting cached inverse value")
        inverseValue
    }
    
    #calculate the inverse
    inverseValue <- solve(x$get(), ...)
    x$setinverse(inverseValue)
    
    inverseValue
}








