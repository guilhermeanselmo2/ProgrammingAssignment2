## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a matrix from the given argument 'x' and saves it in the cache memory
## It also creates a inverse matrix set as NULL so it can be saved latter in the cache
## A list is returned cointaing a set and get function for the original matrix and also
## a setInverse and getInverse for the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

## This function gets the inverser matrix of 'x' if it's already been calculated
## if not, the inverse is calculated, saved in the cache and returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached inverse matrix")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
