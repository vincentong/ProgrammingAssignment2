## Written by Vincent Ong

## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly  


## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix('numeric'))
{
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        setInverseM <- function(inverse = matrix('numeric')) m <<- inverse
        
        getInverseM <- function() m
        
        list(set = set, get = get,
             setInverseM = setInverseM,
             getInverseM = getInverseM)
}

## cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve  <- function(x, ...){
        m <- x$getInverseM()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverseM(m)
        m
}
 
