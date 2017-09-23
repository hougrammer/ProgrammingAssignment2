## We're creating a weird pseudo object to cache matrix inverses.
## Honestly, this is a horribly cumbersome way to perform such a function
## and we really should be using a hashtable.

makeCacheMatrix <- function(x = matrix()) {
    ## Make our weird "cache matrix"
    inv = NULL
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    get = function() x
    setinv = function(newinv) inv <<- newinv
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of x
    ## I handled the singular matrix case though we were not asked to
    
    if (!is.null(x$getinv())) {
        message('getting cached data')
        return(x$getinv())
    }
    
    A = x$get()
    if (nrow(A) != ncol(A)) stop('matrix is not square')
    tryCatch(
        {
            x$setinv(solve(A))
            return(x$getinv())
        }, 
        error = function(cond) stop('matrix is singular')
    )
}
