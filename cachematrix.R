## The scope of this R file is about lexical scoping and caching.
## The target is the creation of two functions that first will cache an invertible 
## Matrix and then compute the inverse Matrix and return it.
## Specifically I am using solve() to find the inverse of a matrix and cache it 
## using a free floating variable.I am also trying to cache the input matrix 
## so that I can retrieve it and compare it to any new input matrices. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # sets the value of m to NULL (initialization to avoid errors if no value is computed)
    ## y <- NULL # sets the value of y to NULL (initialization to avoid errors if no value is computed)
    
    ## constructor of the methods set,get
    setmatrix <- function(y) { # sets the value of the new matrix
        x <<- y ## caches the new matrix, the use of <<- for x is internal in the setmatrix function
        m <<- NULL ## initialization within the function of m to be used for the inversed matrix
    }
    getmatrix <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    # we return a list that hosts the functions set and get for both matrices
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache. 
## If the input is new, it calculates the inverse of the data 
## and sets the inverse in the cache via the setinverse function.

cacheSolve <- function (x, ...) {
    m <- x$getinverse()     ## it first checks to see if the inverse has already been calculated. 
    ## If so, it gets the inverse from the cache and skips the computation
    
    if(!is.null(m)) {       ## as we initialize m to NULL at the CacheMatrix, 
        ## is checking if cashesolve has run already once and returns message
        
        message("getting already cached data")
        
        ## if(m$setmatrix() == m$getmatrix()) {    ## check that matrix hasn't changed, 
        ## and sends a text message and returns the cached matrix
        return(m)
    }
    ## if x$getinverse() returns NULL 
    newmatrix <- x$getmatrix()      ## we get the value of the input matrix
    m <- solve(newmatrix, ...)      ## we calculate the inverse matrix values
    x$setinverse(m)       ## we cache the inverse
    m           ## and return the inverse   
}