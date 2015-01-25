## Erin Comparri 2015 R Programming Programming Assignment 2: Lexical Scoping

## These functions (makeCacheMatrix and cacheSolve) together return the inverse of an 
## invertable square matrix. To reduce computing time, 
## it will first check the cache to see if the inverse of that matrix has already been computed.
## If so, it will fetch the inverse from the cache. Otherwise, it will compute and then store the 
## inverse for future use. 

## makeCacheMatrix creates a special object which can cache a matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  store <- function(y) {
    x <<- y
    m <<- NULL
  }
  retrieve <- function() x
  storeinverse <- function(invert) m <<- invert
  getinverse <- function() m
  list(store = store, retrieve = retrieve,
       storeinverse = storeinverse,
       getinverse = getinverse)
}


## cacheSolve will return the cached inverse if already computed and the given matrix is unchanged. 
## Otherwise, it will compute and store the inverse for future use. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$retrieve()
  m <- solve(data, ...)
  x$storeinverse(m)
  m      
}
