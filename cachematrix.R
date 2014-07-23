## Caching the inverse of a matrix

## makeCacheMatrix function creates a special  matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        
CacheMatrix <- NULL
        set <- function(y) { ## Set the value of the matrix and cache its value
                x <<- y
                CacheMatrix <<- NULL
        }
        get <- function() x ## Get the value of the matrix
                
        setmatrix <- function(solve) CacheMatrix <<- solve ##Set the value of the inverse 
                
        getmatrix <- function() CacheMatrix##Get the value of the inverse
              
        
        list(set=set, get=get,
        setmatrix=setmatrix,
        getmatrix=getmatrix)
}

## CacheSolve function computes the inverse of the special matrix 
## returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
       
        InverseMatrix <- x$getmatrix() 
        if(!is.null(InverseMatrix)){
                message("getting cached data")
                return(InverseMatrix)## Return a new matrix that is the inverse of 'x'
                                     ## if inverse has not been calculated already 
        }
##matrix is invertible and if the inverse has been calculated 
##Then the cacheSolve function should retrieve the inverse from the cache
        data <- x$get()
        CacheMatrix <- solve(data, ...)
        x$setmatrix(CacheMatrix)
        
        CacheMatrix ## Return the inverse of the matrix
}
