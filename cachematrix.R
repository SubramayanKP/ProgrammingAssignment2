## Program to create pair of functions that cache
## the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                inverse1 <- NULL
                
                setting <- function(w) {
                  x <<- w
                  inverse1 <<- NULL
                  
                }
              
              getting <- function() x
              setupInverse <- function(inverse) inverse1 <<- inverse
              getupInverse <- function() inverse1
              list(setting = setting,
                   getting = getting,
                   setupInverse = setupInverse,
                   getupInverse = getupInverse)

  }




##The Below function computes the inverse of the special "matrix" created by 
##makeCacheMatrix above. Note that if the inverse has been calculated already,
## and if the said matrix has remained unchanged,the inverse of the said matrix
##is to be retrieved from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
        inverse1 <- x$getupInverse()
        if (!is.null(inverse1)) {
            message ("relax and sit back! we are getting the cached data")
            return(inverse1)
       
        }
       
       matching <- x$getting()
       inverse1 <- solve(matching, ...)
       x$setupInverse(inverse1)
       inverse1
       
    }

