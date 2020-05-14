## Takes an input matrix, computes the inverse, and
## stores both the matrix and the inverse as
## 'cached' data

makeCacheMatrix <- function(m = matrix()) {
 # Stores an input matrix, and computes and stores
 # inverse matrix as well. Data stored as list of
 # functions that can be called.
        i <- NULL
        set <- function(n) {
                m <<- n
                i <<- NULL
        }
        get <- function() m
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Call up inverse matrix from 'makeCacheMatrix' cache

cacheSolve <- function(m, ...) {
 # Returns the inverse of a matrix list produced
 # using function 'makeCacheMatrix'
        i <- m$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- m$get()
        i <- solve(data, ...)
        m$setinv(i)
        i
}
