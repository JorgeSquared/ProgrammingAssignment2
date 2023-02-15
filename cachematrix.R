## The first of the two functions called "makeCacheMatrix"
## is a function together with some "member" functions
## inside, that basically enable the user to cache a matrix and its
## inverse

makeCacheMatrix <- function(x = matrix()) {
    ## we first initialize the inverse matrix as NULL
    i <- NULL
    
    ## then we create the set and get methods for the matrix
    ## and its inverse
    
    getMatrix <- function() {
        x
    }
    
    setMatrix <- function(m) {
        x <<- m
        i <<- NULL
    }
    
    getInverse <- function() {
        i
    }
    
    setInverse <- function(i) {
        i <<- i
    }
    
    list(
        setMatrix = setMatrix,
        getMatrix = getMatrix,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## This function either computes the inverse of a matrix, or returns
## a cached inverse that has been already computed via makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## first we try to get the inverse if it exists
    i <- x$getInverse()
    
    ## check whether the inverse has already been computed and return it if so
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## otherwise, compute the inverse and cache it
    m <- x$getMatrix()
    m <- solve(m)
    x$setInverse(m)
    
    return(m)
}
