## Put comments here that give an overall description of what your
## functions do

## To create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    cachedInv <- NULL
    set <- function(y){
        x <<- y
        cachedInv <<- NULL
    }
    get <- function()x
    setinv <- function(inverse) cachedInv <<- inverse
    getinv <- function() cachedInv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## To compute the inverse of the special "matrix" returned by makeCacheMatrix.If the inverse has already been calculated, then the cachesolve retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    funcInv <- x$getinv()
    if(!is.null(funcInv)){
        message("getting cached data")
        return(funcInv)
    }
    data <- x$get()
    funcInv <- solve(data, ...)
    x$setinv(funcInv)
    funcInv
}
