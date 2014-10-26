## makeCacheMatrix: Make a Matrix and Cache its inverse
## cacheSolve:      Return inverse of given matrix; checking for cached version first

## This function defines utility functions for creating a matrix and caching its inverse
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
            x <<- y
            s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        
        list(set = set, 
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        ##Check for cached version first
        s <- x$getsolve()
        if(!is.null(s)) {
            message("getting cached data")
            return(s)
        ##initialize data    
        data <- x$get()
        ##Compute inverse
        s <- solve(data, ...)
        ##Set cache for next time
        x$setsolve(s)
        ##Return result
        s
    }
}
