## These functions cache the inverse of a matrix so it can be recalled later
## by another function to save processing time

## This first function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {     # This says that makeCacheMatrix is a function involving x, which in turn is a matrix
        inversematrix <- NULL                   # clears the inversematrix
        set <- function(y)      {               # defines the set function
                x <<- y                         # caches value for x as being y
                inversematrix <<- NULL
        }
        get <- function()       {               # defines the get function
                x                               # ...to simply recall the matrix x
        }
        setinverse <- function(inverse) {       # defines the setinverse function  
                inversematrix <<- inverse       
        }
        getinverse <- function()        {       # defines the getinverse function
                inversematrix                   # to simply recall the inverse matrix
        }
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  # return vector type list of functions
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then this function
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inversematrix <- x$getinverse()         # this tries to retrieve the inverse matrix from the cache
        if(!is.null(inversematrix))     {       # if there is a value for inversematrix in the chache...
                message("getting cached data")  # this returns message "getting cached data"
                return(inversematrix)
        }
        data <- x$get()                         # this retrieves the matrix and calls it "data"
        inversematrix <- solve(data, ...)       # this calculate the inverse of the matrix now called "data"
        x$setinverse(inversematrix)             # this stores the inverse matrix in the object generated in makeCacheMatrix
        inversematrix                           # this returns the inverse of the matrix x
        }
