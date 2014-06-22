## Compute the inverse of invertible matrix and cache the inverse
## functions do

## Create a special "matrix" object that can cache its inverse
# This object has 4 functions:
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse matrix
# 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinv <- function(mat) m <<- mat
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## Computes the inverse of the matrix returned by function "makeCacheMatrix"
# If the inverse has already been calculated, return matrix in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
