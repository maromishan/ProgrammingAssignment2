##The following pair of functions cache the inverse of a matrix. 
##The aim is to compute the inverse of a matrix. If the matrix inverse
##has already been computed then it must be cached so that it may be
##ready to use again if needed, without further computation. This is
##what the following code accomplishes.

## makeCacheMatrix is a function which creates a matrix object. This is used
##in caching the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      invers <- NULL
      set <- function(y){
        x <<- y
        invers <<- NULL
        
        get <- function() x
        
        setInv <- function(inv) invers <<- inv
        getInv <- function() invers
        list(set = set, get = get, setInv = setInv, getInv = getInv)
      }
}


## cacheSolve is a function that coputes the inverse of a matrix.
##If the matrix has already been computed then it is returned by 
##the function makeCacheMatrix above, Otherwise it computes the 
##inverse of the matrix and sets the computed value in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invers <- x$getInv()
  if(!is.null(invers)){
    message("getting cached matrix inverse data")
    return(invers)
  }
  invData <- x$get()
  invers <- solve(invData)
  x$setInv(invers)
  invers
}
