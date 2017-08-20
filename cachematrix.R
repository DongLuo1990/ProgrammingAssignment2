## This create a matrix object which cache the result of the inverse, if the 
## matrix does not change, then we direct return the inverse matrix that is
## stored, if the matrix change, then we recompute the inverse matrix and update
## the stored one

## Create a matrix object with input x, by default, this will create an empty
## matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL # when matrix update, set caching inverse matrix to be NULL
    }
    get <- function() x
    setSolve <- function(inverse) m <<- inverse
    getSolve <- function() m
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)

}


## compute the inverse at the first time, or return the caching inverse matrix
## if the matrix does not change

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if (!is.null(m)) {
        print("return the cache Inverse matrix")
        return(m)}
    m <- solve(x$get(), ...)
    x$setSolve(m)
    m
}
