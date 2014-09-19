## The purpose of the two functions is to cache the computed inverse to avoid computing the same matrix inverse repeatedly 
## if the need occurs.

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
      m <- NULL
      
      ## Set value of matrix
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      ## Output the x matrix
      get <- function() x
      
      ## Store in inverse matrix
      setsolve <- function(solve) m <<- solve
      
      ## Get the inverse matrix
      getsolve <- function() m
      
      list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache using getsolve() function.

cacheSolve <- function(x, ...) {
  
          m <- x$getsolve()
          
          ## Check if the inverse has already been computed and stored
          if(!is.null(m)) {
                message("getting cached data")
                return(m)
          }
          
          ## If not stored already, get the matrix, calculate the inverse and store inverse into cache
          data <- x$get()
          m <- solve(data, ...)
          x$setsolve(m)
          m
}
