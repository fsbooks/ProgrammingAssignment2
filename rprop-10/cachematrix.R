# cachematrix.R

# This file has two functions providing caching capabilities for inverting matrices

#  The first function (makeCacheMatrix) has an invertible matrix as argument, and 
#  returns a list with functions to
#       (1) set the value of the matrix 
#       (2) get the value of the matrix
#       (3) set the value of the matrix inverse
#       (4) get the value of the matrix inverse

# The second function (cacheSolve) takes the list created by makeCacheMatrix as argument, 
#     and returns the inverse.
#   If the inverse has already been calculated, it retrieves the inverse from the
#      cache, otherwise it calculates the inverse and stores it in the cache.

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
