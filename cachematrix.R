## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    aMatrix <- NULL
    
    # save the original version of the matrix
    set <- function(inMatrix) {
        x <<- inMatrix
        aMatrix <<- NULL
    }
  
    get <- function() x
    setinverse <- function(solve) aMatrix <<- solve
    getinverse <- function() aMatrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    aMatrix <- x$getinverse()
    
    
    if(!is.null(aMatrix)) {
          # The inverse has already been computed. Just return it.
          
          #message("retrieving cached inverse")
          return(aMatrix)
      }
    

    theData <- x$get()

    # Compute the matrix inverse
    aMatrix <- solve(theData,...)
    
    x$setinverse(aMatrix)
  
  	aMatrix
}

# Test function for the cached matrix problem. The default matrix is a 750x750 
# matrix. When this funtion runs, it creates a special cached matrix. Then it shows
# the time to create the inverse and then it shows the time to retrieve the inverse
# from the cache. (This is interesting with matrices of 1m or more elements.)
cacheTest <- function(inSquareInvertibleMatrix = matrix(rnorm(562500),nrow=750,ncol=750)) {
    
    aCacheMatrix <- makeCacheMatrix( inSquareInvertibleMatrix )
    
    startTime <- Sys.time()
    cacheSolve(aCacheMatrix)
    anInterval1 <- Sys.time() - startTime
    startTime2 <- Sys.time()
    cacheSolve(aCacheMatrix)
    anInterval2 <- Sys.time() - startTime2
    
    print(paste("Interval 1", anInterval1, sep=" "))
    print(paste("Interval 2", anInterval2, sep=" "))
}
