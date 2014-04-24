## Put comments here that give an overall description of what your
## functions do

## Creates a cacheable matrix to be used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Uses solve to invert a cacheable matrix if not present in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

## Example for testing
c<-rbind(c(1, -1/4), c(-1/4, 1))
d<-makeCacheMatrix(c)
d
e<-cacheSolve(d)
e
cacheSolve(makeCacheMatrix(c))
c %*% e




