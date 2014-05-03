## Put comments here that give an overall description of what your
## functions do

## i, initialized as a matrix of NAs, is used to cache the inverse of x.

makeCacheMatrix <- function(x = matrix()) {
    i <- matrix(NA,nrow(x),ncol(x))
    set <- function(y){
        x <<- y
        i <<- matrix(NA,nrow(x),ncol(x))
    }
    get <- function() x
    setInverse <- function(inversed) i <<- inversed
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (!is.na(i[1,1])){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
}
