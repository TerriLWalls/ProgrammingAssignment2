## Put comments here that give an overall description of what your
## functions do

## a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
        
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
        setmean = setmean,
        getmean = getmean)
    
}



## computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
    if(is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <-x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
