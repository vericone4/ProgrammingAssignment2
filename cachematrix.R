#################################################################
## Functions to take a invertible matrix, compute inverse matrix
## check if inverse matrix is already cached for the given matrix
## else generate the inverse matrix, cache it & return it.

## makeCacheMatrix function
## ------------------------
## this function is first called to create a list of functions
## to generate and return given matrix & its inverse matrix
## it returns the list of following functions
## These functions are setter /getter methods for both matrix and inverse
## matrix 

## cacheSolve function
## --------------------
## this function is called next to generate inverse matrix
## it takes list object and options to 'solve' function.
## it checks if inverse matrix exists,
## else generate one with solve function and returns the inverse matrix.

#################################################################

makeCacheMatrix <- function(x = matrix()) {
  
  ## instaniate the empty matrix which will be eventually be used
  ## to store the inverse
  inverse_matrix <- NULL
  
  ## function to set the matrix and inverse matrix 
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  
  ## function to return the matrix
  get <- function(){ 
    x
  }
  
  ## function to set the value of inverse of the matrix
  setinverse <- function(inverse){
    inverse_matrix <<- inverse
  }
  
  ##function to return the value of inverse of the matrix
  getinverse <- function(){
    inverse_matrix
  }
  
  ## creation of the list with the above methods which will be returned 
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function
## -------------------
## cacheSolve function takes list object and required solve funtion options if
## necessary, checks if a inverse matrix is already cached and returns it.
## If inverse matrix does not exist,
## gets the original matrix through list object get function,
## computes the inverse using solve function
## caches the inverse matrix using setsolve function
## and returns the inverse matrix 

cacheSolve <- function(x, ...) {        
  
  ## Return a matrix that is the inverse of 'x' stored in cache
  m <- x$getinverse()
  
  ## check if the inverse stored in cache is not null and return the 
  ## inverse if it is not
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## else get the matrix and store it in data 
  data <- x$get()
  
  ## compute the inverse of the data and store it in m
  m <- solve(data, ...)
  
  ## set the inverse which is computed above in the cache
  x$setinverse(m)
  
  ## return the inverse
  m
}