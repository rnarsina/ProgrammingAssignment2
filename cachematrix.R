## Put comments here that give an overall description of what your
## functions do

## This function will hold the global scope of cache.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function first will check the cache, 
## if available, will return from the cache
## if not available in cache, reverses the matrix and stores in the cache

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    message("Cached data did not found, will get the inverse and stored in cache")
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}