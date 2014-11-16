## Put comments here that give an overall description of what your
## functions do

## Creates a special vector containing a function to 1) set the value
## of the matrix, 2) get the value of the matrix, 3) set the inverse of 
## the matrix, and 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Return the inverse matrix of x by first searching for cashed inverse
## by looking up "getinv" in the vector created by makeCacheMatrix. If 
## there is no cached data (m = Null), calculates the inverse and store
## the computed value using "setinv".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
