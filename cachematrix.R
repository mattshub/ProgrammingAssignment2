## R Functions : Solve and Cache inverse of a matrix. These functions will
## solve and cache the inverse of a matrix. 

## makeCacheMatrix : This function takes in a matrix as input and creates
## a list that holds the matrix itself, the inverse of same if already 
## generated once and functions needed to access same
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ##  function to set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        ##  function to set the inverse matrix
        setinverse <- function(inverse) inv <<- inverse
        ##  function to get the inverse matrix
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve : This function takes in a "list" that was created using
## the "makeCacheMatrix" fucntion and gets the inverse of the matrix if
## its already cached else will solve & cache same
cacheSolve <- function(x, ...) {
        ## Returns a matrix that is inverse of 'x'
        ## Tryning to get the inverse from cache
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Getting inverse of the matrix from cache")
                return(inv)
        }
        ## Solving for inverse
        data <- x$get()
        inv <- solve(data, ...)
        ## storing in cache
        x$setinverse(inv)
        inv
}