## These functions calculate the inverse of a Matrix and utilize cacheing to speed up the calculations.


## makeCacheMatrix creates a list containing functions to
# 
# A. set the value of the vector
# B. et the value of the vector
# C. et the value of the mean
# D. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invs <<- inverse
        getinverse <- function() invs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## The following function calculates the inverse of the Matrix. 
## It will first check to see if the inverse has already been calculated. 
##If so, it gets the value from the cache and skips the computation. 
## If it has not been calculated it calculates the inverse 
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

                invr <- x$getinverse()
                if(!is.null(invr)) {
                        message("getting cached data")
                        return(invr)
                }
                data <- x$get()
                invr <- solve(data, ...)
                x$setinverse(invr)
                invr
}

