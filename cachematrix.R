## This file creates two functions makeCacheMatrix and cacheSolve
## which enables you to create a special form of matrix with 
## makeCacheMatrix and cache (and return) the inverse of the
## matrix with cacheSolve.

## takes a square matrix as input and creates a special matrix with a cacheable inverse.
## this function assumes a square/invertable matrix is provided.

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## checks for a cached version of the inverse of the supplied special matrix
## and returns that value if present.  if not, computes the inverse and
## caches it for future reference.

cacheSolve <- function(x, ...) {

    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data...")
        return(inv)
    }
    mat <- x$get
    inv <- solve(mat)
    x$setinv(inv)
    inv
}
