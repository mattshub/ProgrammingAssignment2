## R Functions : Solve and Cache inverse of a matrix. These functions will
## solve and cache the inverse of a matrix. 

## makeCacheMatrix : This function takes in a matrix as input and creates
## a list that holds the matrix itself, the inverse of same if already 
## generated once and functions needed to access same
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve : This function takes in a "list" that was created using
## the "makeCacheMatrix" fucntion and gets the inverse of the matrix if
## its already cached else will solve & cache same
cacheSolve <- function(x, ...) {
        ## Returns a matrix that is  inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting inverse of the matrix from cache")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}