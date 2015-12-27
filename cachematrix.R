##" [...]This function is able to cache potentially time-consuming computations.
##For example, taking the mean of a numeric vector is typically a fast
##operation. However, for a very long vector, it may take too long to
##compute the mean, especially if it has to be computed repeatedly (e.g.
##in a loop). If the contents of a vector are not changing, it may make
##sense to cache the value of the mean so that when we need it again, it
##can be looked up in the cache rather than recomputed. Scoping rules of
##the R language will be used and shown how they can be manipulated to 
##preserve state inside of an R object."

## Create a cacheMatrix object for an invertale matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                cachedInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverse <<- inverse
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Return the inverse of an cacheMatrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invFunc <- x$getInverse()
        if(!is.null(invFunc)) {
                message("getting cached data")
                return(invFunc)
        }
        data <- x$get()
        invFunc <- solve(data, ...)
        x$setInverse(invFunc)
        invFunc
}
