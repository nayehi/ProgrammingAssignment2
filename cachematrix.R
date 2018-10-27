## These functions cache the inverse of a matrix and then return
## it.

## This function creates a special matrix to cache the inverse
## of the 'x' matrix.


makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
}
## This function identifies what will be done in order to
## generate the output (inverse) matrix, based on whether that 
## matrix has previously been cached or needs to be created.
       
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, 
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function checks to see if an inverse has already
## been cached. If so, it returns it. If not, it calls the 
## makeCacheMatrix's setsolve function to create it, 
## and then returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
