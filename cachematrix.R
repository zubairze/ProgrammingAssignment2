## Below are two functions that are used to create a special
##  object that stores a matrix and cache's its inverse.

## makeCacheMatrix creates a list containing functions to set and
## get value of the matrix, and to set and get value of its inverse


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


## cacheSolve calculates the inverse of matrix used as argument
## for makeCacheMatrix, but gets value from cache if possible
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
