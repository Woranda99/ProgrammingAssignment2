## Functions to store matrix and 
## cache its inverse

## makeCacheMatrix take matrix as an input and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setM <- function(y){            # set the value of the Matrix
                x <<- y
                i <<- NULL
                }
        getM <-function() x             # get the value of the Matrix
        setInv <- function(inverse) i <<- inverse
        getInv <- function() i
        list(setM = sertM,getM = getM,setInv=setInv,getInv=getInv)
      

}


## cacheSolve computes the inverse of the matrix created by the previous function(makeCacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if (!is.null(i)) {                     # if inverse matrix is not NULL get a message              
          message("Getting cached data")
          return(i)                      # return invertible matrix       
  }
  # if inverse matrix is NULL    
  data <- x$getM()
  i <- solve(data, ...)
  x$setInv(i)                       # return invertible matrix    
  i
}
