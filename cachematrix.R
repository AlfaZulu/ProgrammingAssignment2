## The two functions below create a special "matrix" object that stores a matrix
## and caches its inverse

## makeCacheMatrix creates a special "matrix" object, which is a list containing 
## functions to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve calculates the inverse of the special "matrix" object created by 
## makeCacheMatrix. It first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the 
## calculation. If not, it calculates the inverse and sets the value of the
## inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
