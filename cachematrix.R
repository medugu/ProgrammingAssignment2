
## The script below  demonstrates  how to Cache the Inverse of a Matrix:
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The functions below are used to create a special object that 
## stores a matrix and caches its inverse.
## The makeCacheMatrix creates  a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix.If the inverse has already been calculated (and the 
## matrix has not changed), Otherwise it calculates the mean of the data and
## sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data") 
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
