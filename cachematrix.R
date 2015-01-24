## This function caches the inverse of a matrix. If the inverse was not calculated
## before, then the function calculates the inverse and store it in the cache.

## The makeCacheMatrix function creates a list that contains four functions:
## set() to set the matrix, get() to fetch the matrix, setinv() to set the inverse
## of the matrix in the cache, and getinv() to fetrch the inverse stored in the cache

makeCacheMatrix <- function(x=matrix()){
        invx <- NULL
        set <- function(y){
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) invx <<- inverse
        getinv <- function() invx
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
}


## The cacheSolve function first checks if the inverse has been calculated or not.
## If yes, then the function returns the cached inverse and also print message
##"getting cached data."
## If not, then the function calculates the inverse by using solve(), store this
## inverse to cache for future use, and return the inverse. 

cacheSolve <- function(x,...){
        invx <- x$getinv()
        if(!is.null(invx)){
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data,...)
        x$setinv(invx)
        invx
}