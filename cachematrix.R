## A object that is used to cache the inverse matrix
##
## Args:
##    x		Matrix over which we want to calculate
##              and cache its inverse 
makeCacheMatrix <- function(m = matrix()) {
   invM <- NULL
   
   getMatrix <- function() m 
   setMatrix <- function(mtx)  {
     m <<- mtx
     invM <<- NULL
   }
   getInverseMatrix <- function() invM
   setInverseMatrix <- function(mtx) invM <<- mtx
   list(getMatrix = getMatrix,
        setMatrix = setMatrix,
        getInverseMatrix = getInverseMatrix,
        setInverseMatrix = setInverseMatrix)
}

## Return the inverse matrix.
##
## If the inverse matrix was calculated before, it returns
## the cached inverse matrix, in other case it calculates 
## the inverse matrix and returns it  
cacheSolve <- function(x, ...) {
    iM <- x$getInverseMatrix()
    if( !is.null(iM )) {
       #The inverse matrix has alredy been calculated
       message( "cached inverse matrix");
       return (iM)
    }

    ## Calculate the inverse matrix
    m <- x$getMatrix()
    iM <- solve(m)
    x$setInverseMatrix(iM)
    iM
}
