## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    # initialize cache for inverse matrix to NULL
    i <- NULL
    
    # set a matrix into our special "matrix"
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # return the matrix inside our special "matrix"
    get <- function() x
    
    # set a matrix "inverse" as the cached inverse matrix
    setinverse <- function(inverse) i <<- inverse
    
    # return the cached inverse matrix of our matrix
    getinverse <- function() i
    
    # makeCacheMatrix return the list of functions that let us to operate
    # with the special "matrix" object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function

# cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed),
# then cachesolve retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # check if there is a calculated inverse matrix
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting inverse matrix")
        # a cached inverse matrix exits, so we return it
        return(i)
    }
    # there is no cache, so we must calculate inverse matrix
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
