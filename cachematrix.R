## Put comments here that give an overall description of what your
### The objective of this lexical scope task is to write a pair of functions,
### such that the state of the first function inside an object R is preserved,
### remaining available for the calculation of the second function

## functions do
### the inverse of a matrix

## Write a short comment describing this function
### makeCacheMatrix is a function that creates a special "matrix" object,
### which can be kept in cache, being an invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
### cacheSolve is a function which computes the inverse of the special "matrix"
### returned by the funcion makeCacheMatrix above
### If the inverse has already been calculated (and the matrix has not changed),
### then the cachesolve should retrieve the inverse from the object in cache

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
### Checking the program

### m <- matrix(rnorm(9), 3, 3)
### m1 <- makeCacheMatrix(m)
### cacheSolve(m1)

### or, with this simpler matrix
### m <- matrix(c(1, 2, 1, 3),2, 2)
### m1 <- makeCacheMatrix(m)
### cacheSolve(m1)