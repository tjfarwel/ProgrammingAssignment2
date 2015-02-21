##Created by tjfarwel
## Put comments here that give an overall description of what your
## functions do
##The functions create a cached value so if already solved will not
## need to resolve. This saves computation time


## Write a short comment describing this function
#This function creates a list of function call variables and 
# a cached variable i
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#This function 1st checks if the inverse has already been called
#if it has, it returns i immediately, if not it solves the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i))
        {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
