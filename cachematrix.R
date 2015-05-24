## The 2 functions cache the inverse operation on a matrix.  
## A special matrix object is created with the ability to store its own cache
## The inverse will be computed only the first time the cacheSolve function is called
## and stored in the special matrix cache
## If it has been previously solved, the answer is retrieved from the cache

## makeCacheMatrix:creates a special matrix object that can cache its 
## own inverse

makeCacheMatrix <- function(x = matrix()) {
	  i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve: returns the inverse of the input matrix.  
## If the inverse was calculated previously,
## the result is returned from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
