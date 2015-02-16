## Put comments here that give an overall description of what your
## functions do

## The function provides special, cachable, inverted matrix 
## which may be used to improve performance 
## when repeated computation is needed

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    
    ## Return cachable matrix
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Provides inverted matrix in optimal from performance point of view way
## Note: The input matrix should be created with help of makeCacheMatrix

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}
