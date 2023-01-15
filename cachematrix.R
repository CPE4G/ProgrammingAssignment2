## The following pair of functions cache the inverse of a matrix
## The arguments for this assignment were all assumed a square matrix
## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

## This function computes the inverse of the special
## matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
## NOTE: Solve() needs a matrix that is square and invertable
## i.e. square matrixes with non-zero determinant.
## Check matrix argument for non-zero determinant with det().

cacheSolve <- function(x=matrix(), ...) {
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}

## To use example:
## > cacheSolve(makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2)))
##
## Outputs the following:
##
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## Another example passing in a diagnol matrix just for fun:
## cacheSolve(makeCacheMatrix(diag(c(1,2,3))))
##
## Outputs the following:
##
## [,1] [,2]      [,3]
## [1,]    1  0.0 0.0000000
## [2,]    0  0.5 0.0000000
## [3,]    0  0.0 0.3333333
