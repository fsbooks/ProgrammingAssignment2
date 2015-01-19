# cachematrix.R

# This file provides caching capabilities for inverting matrices

#   Two functions are provided:  
#   The first function (makeCacheMatrix) has an invertible matrix as argument, and 
#   returns a list with functions to
#       (1) set the value of the matrix 
#       (2) get the value of the matrix
#       (3) set the value of the matrix inverse
#       (4) get the value of the matrix inverse

#   The second function (cacheSolve) takes the output list from makeCacheMatrix as an argument, 
#       and returns the inverse.
#     If the inverse has already been calculated, it retrieves the matrix from the
#       cache, otherwise it solves and then sets it.

# EXAMPLE:
# > M <- matrix(rnorm(25),5,5)
# > C <- cacheSolve(M)
# > outinverseM <- cacheSolve(C)

# ...if cacheSolve is run a second time, the solution is retrieved from the cache.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inverse <<- solve
        getsolve <- function() inverse
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getsolve()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setsolve(inverse)
        inverse
}
