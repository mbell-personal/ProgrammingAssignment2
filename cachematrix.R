## Function used to get the inverse of a matrix.  It takes a matrix and returns
## the inverse.
##
## NOTE: Because the computation of a matrix inverse can be an expensive computation,
##       the inverse is cached and return from memory.
makeCacheMatrix <- function(x = matrix()) {
    #Initialize the local inverse to null
    inverse <- NULL
    
    #Function used to set the matrix value
    setMatrix <- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    #Function used to get the matrix value
    getMatrix <- function() x
    
    #Function used to set the inverse of the matrix
    setInverse <- function(inverse) inverse <<- inverse
    
    #Function used to get the inverse of the matrix
    getInverse <- function() inverse
    
    #Return a list of the defined functions
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Takes a function containing a matrix and gets the inverse (from the function).
## If the inverse exists, it is pulled from the cahce of the function.  Otherwise,
## the inverse is created and added to the case of the function.  Either way, the
## inverse of the matrix is returned.
cacheSolve <- function(x, ...) {
    #Get the inverse (if existent) from the passed in function
    inverse <- x$getInverse()
    
    #If the inverse exists, use it - no need to compute the inverse
    if (!is.null(inverse)){
        message("Getting matrix inverse from cache...")
        return(inverse)
    }
    
    #At this point, the inverse of the matrix does not exist.  Compute it and cache it for future reference.
    message("Computing the inverse of the specified matrix...")
    matrix <- x$getMatrix()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    
    #Return the inverse that was computed and cached
    inverse
}
