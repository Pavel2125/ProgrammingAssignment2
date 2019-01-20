## makeCacheMatrix function is a list of 4 function (set,get,setsolve,getsolve) 
## set: make x equal to our matrix and m equal to zero.
## get: returt x (equal to our matrix)
## setsolve: set the inversed matrix
## getsolve: get the inversed matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## CacheSolve function first take 'getsolve' parameter and if it's not empty print it. 
## Otherwise, inversed a matrix, put inversed matrix to m and print m. 

cacheSolve <- function(x, ...) {
  
  m <- x$getsolve()
  if(!is.null(m)) {     ## if x was already inversed then take it from cache
    message("getting cached data")
    return(m)
  }
  data <- x$get()        ## Take data from x
  m <- solve(data, ...)  ## Inverse of 'data'
  x$setsolve(m)          ## Set the inversed matrix
  m                      ## A matrix that is the inverse of 'x'
}


