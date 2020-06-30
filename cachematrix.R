## Stores a matrix and allows for the getting/setting of its value and the
## value of its inverse.

library(digest)

## Creates a list that contains four separate functions that sets/gets the value
## of a matrix and sets/gets the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Creates a global key/value map cache where the key is the hash value of the
## matrix and the value is the inverse of that matrix. The intention is to
## calculate the inverse of an unique matrix one time, as long as the 
## 'inversemap' object persists.
cacheSolve <- function(x, ...) {
    if (!exists("inversemap")) {
        inversemap <<- list(matrix())
    }
    
    inverse <- x$getinverse()
    if (is.null(inverse)) {
        key <- digest(x$get())
        if (is.null(inversemap[[key]])) {
            print("Updating cache...")
            inversemap[[key]] <<- solve(x$get())
        }
        x$setinverse(inversemap[[key]])
    }    
    x$getinverse()
}
