## This is a code that shows a pair of functions which purpose is to avoid 
## costly time-consuming computations when calculating matrix inversion.
## This will be achieved by caching the inverse of a matrix rather 
## than compute it repeatedly

## This function creates a special "matrix" object that can cache its invers

makeCacheMatrix <- function(x = matrix()){
        inverse <- NULL
        set <- function(y){
                x <<-y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x,...){
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setinverse(inverse)
        inverse
}
