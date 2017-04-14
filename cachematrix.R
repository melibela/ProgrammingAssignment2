## makeCacheMatrix and cacheSolve will solve for the inverse of 
## a square invertible matrix, saving the solved matrix in the cache for further use
## or replacing it when a new matix is inverted

## makeCacheMatrix creates matrix object that can be inverted, 
## defines setters and getters

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL               ##i for inverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve inverts a matrix x (only from makeCacheMatrix()!), saves it in the cache
## OR retrieves an existing inverted matrix from the cache

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
