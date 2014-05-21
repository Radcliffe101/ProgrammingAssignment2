## This function creates a special matrix object that can cache its inverse.
## It uses the << operator to assign the matrix to a different environment.

## This function accepts a matrix, x, as its sole arguement. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, 
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function, which drove me crazy, will calculate the inverse of a matrix using the solve 
## function. First it checks to see whether the inverse has already been calculated. If so
## it retrieves it from the cache. Otherwise it will calculate it. 
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
         if (!is.null(m)) {
                 message("getting cached data")
                 return(m)
         }
         data <- x$get()
         m <- solve(data, ...)
         x$setsolve(m)
         m
        ## Return a matrix that is the inverse of 'x'
}

## To test this you could first define a matrix. For example:
## z <- matrix(c(1,1,1,3,4,3,3,3,4), nrow=3, ncol=3)
## a <- makeCacheMatrix()
## a$set(matrix(z))
## a$get
## cachesolve(a)
