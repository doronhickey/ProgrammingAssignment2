## Put comments here that give an overall description of what your functions do

## These functions allow a number of different stored functions to be applied to an invertible matrix, 
## including inversion. The first time the matrix is inverted it is stored to cached memory
## for quick retrieval if needed again. 


## Write a short comment describing this function

## 'makeCacheMatrix' takes an invertible matrix and allows a number of different functions 
## to be applied to that matrix, such as modifying it (and resetting m to be NULL) and 
## returning the inverse. The different functions are stored as a list. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {  
    x <<- y
    m <<- NULL
  }
  get <- function() x  
  setinverse <- function(solve) m <<- solve  
  getinverse <- function() m 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

## 'cacheSolve' tests whether 'm' is not null and if it is not it returns the cached
## value of m (inverted matrix). If it is null then it inverts the matrix and returns it. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse() 
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)  
  m 
}
