## First we call makeCacheMatrix function to create a special matrix (list of functions)
## that operate on the matrix that was passed to it. Special matrix can then be passed
## to cacheSolve function as an argument that checks whether the matrix has been cached or not.
## If it has been cached then cached content is returned otherwise it will run a solve function
## and cache the inverse.

## Example call:
## 
## sm <- makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2, ncol = 2))
## cacheSolve(sm)
##
## Result after running first time:
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## Result after running second time:
## getting cached data
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## This functions creates a special matrix which is a list containing the following functions:
## set - sets the value of the matrix and resets inv vector
## get - gets the value of the matrix
## setinverse - sets inverse of the matrix
## getinverse - gets inverse of the matrix
## 
## inv vector is used to cache inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This functions accepts the special matrix (list of functions) as the first argument. Then it calls 
## getinverse function stored in the list to get inverse value. If the value is null then solve 
## function is called with matrix as a parameter (that we get by calling "get" function in the list)
## and result is cached. Otherwise we just return cached content.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
