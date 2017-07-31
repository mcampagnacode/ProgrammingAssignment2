## Two functions that create a special object that stores a matrix and caches it's inversion.

## The first function creates a special matrix, which is really a list containing a function that sets the values of the 
## matrix, gets the values of the matrix, sets the value of the inversion, gets the value of the inversion.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The next function calculates the inverse of the matrix, checking first that it has not already been calculated.
## If so it will get the inverse from the cache and skip the computation. Otherwise, it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

#EXAMPLE
m <- matrix(1:4, nrow = 2, ncol = 2)
m
cacheSolve(makeCacheMatrix(x = m))
