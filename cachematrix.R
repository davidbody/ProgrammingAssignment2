## This pair of functions demonstrates the use of R's lexical scoping
## to create a function closure that caches the result of computing
## the inverse of a matrix.
##
## For example, suppose the matrix to be computed is m:
##
## > m <- matrix(c(4, 2, 7, 6), nrow=2, ncol=2)
## > m
##      [,1] [,2]
## [1,]    4    7
## [2,]    2    6
##
## We first create the function closure by passing m to
## makeCacheMatrix, storing the result in cache.m:
##
## > cache.m <- makeCacheMatrix(m)
##
## Then we compute the inverse of m by passing cache.m to cacheSolve:
##
## > inv1 <- cacheSolve(cache.m)
## > inv1
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
##
## If we calculate the inverse a second time, the cached result is
## returned:
##
## > inv2 <- cacheSolve(cache.m)
## getting cached data
## > inv2
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
##


## makeCacheMatrix creates an environment (closure) for caching the
## inverse of a matrix.
##
## The argument x is the matrix to be inverted.
##
## makeCacheMatrix returns a list of functions:
##
##   setm - sets the matrix and clears any cached inverse
##   getm - gets the matrix
##   getinv - gets the inverse
##   setinv - sets the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setm <- function(y) {
                m <<- y
                inv <<- NULL
        }
        getm <- function() m
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(setm = setm, getm = getm, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of a matrix.
##
## The argument x is the list of functions returned by
## makeCacheMatrix for the matrix to be inverted.
##
## If the inverse has already been computed, cacheSolve
## returns a cached copy of the inverse.
##
## Note: It is assumed that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$getm()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}
