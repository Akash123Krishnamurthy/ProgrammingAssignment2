
## This function makeCacheMatrix takes a matrix object as input and stores it inside the variable x. It also contains
## nested functions set() which can mutate the matrix, get() which returns the matrix,
##  setInverse() which caches the computed value of the inverted matrix and getInverse() which returns the inverse.
##The makeCacheMatrix returns a list so the nested functions can be accessed using the '$' operator

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function cacheSolve computes the inverse of the matrix and caches the inverse by calling the nested functions
## from makeCacheMatrix setInverse. If there already exists an inverted matrix (provided the matrix has not changed) 
##in the cache it just returns the value of 'i' which saves computation time

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)){
                message("Getting cached data.")
                return(i)
                }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
        }
