## Assignment 2 for R programming course
## illustrates use of scoping rules to cache the state of an object
##

## creates a special "matrix" object that can cache its inverse
##  (adapted from makeVector) 
makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL

    ## set the value of x
    set <- function(y) {
      x <<- y
      x_inv <<- NULL
    }
    ## get the value of x
    get <- function() x

    ## set the value of the inverse
    setinv <- function(solve) x_inv <<- solve 

    ## get the value of the inverse
    getinv <- function() x_inv

    ## return the list of these functions we just defined
    list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  (adapted from cachemean)

cacheSolve <- function(x, ...) {
    ##check to see if the inverse has been calculated
    ## NB: this assumes mtx has not changed since inv calculated!
    x_inv <- x$getinv()
    if(!is.null(x_inv)) { 
      ## inverse exists, so get from cache
      message("getting cached data")
      return(x_inv)
    }
    else { 
        ## need to compute and then cache inverse
        data <- x$get()
        x_inv <- solve(data, ...)
        x$setinv(x_inv)
    }
    ## return the inverse
    x_inv  
}
