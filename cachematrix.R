## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse
makecachecatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}
# `cacheSolve` function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
#If the inverse has already been calculated (and the matrix has not changed), then`cacheSolve` should retrieve the inverse from the cache.

cachesolve <- function(x, ...) { 
        inv<- x$getinv()
        if(!is.null(inv)) {
                message("-- from the Cache")
                return(inv)
        }
        mdata <- x$get()
        inv<- solve(mdata, ...)
        x$setinv(inv)
        inv
}
