## The two functions below will cache the inverse of a matrix.  The first function
## will contain a list of 4 functions used to store matrix to cache, store the inverse
## of a matrix to cache, retrieve the matrix from cache, and retreive the inverse of the 
## matrix from cache.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ##initiate i for later use
  i <- NULL
  
  ## 1. Create a funtion to store the matrix.  The matrix is passed into the 
  ## function as y. The matrix is then sent to cache and stored in the variable x.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## 2. Store a function used to return x from cache
  get <- function() x
  
  ## 3. Create a function to store the inverse of the matrix into cache as the 
  ## variable i
  setinverse <- function(inverse) i <<- inverse
  
  ## Store a function used to return i -the inverse- from cache
  getinverse <- function() i
  
  ##Create a list of all 4 functions above  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve function should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## First check to see if the inverse of the matrix already exist in cache
  
  ## Call for the inverse stored in cache using the getinverse function from
  ## the makeCacheMatrix function
  i <- x$getinverse()
  
  ##If i - the inverse- is not null, return it and end.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## If i is null - the inverse does not exist.
  ## Retreive the matix, then calculate the 
  ## inverse of the matrix, then store it in cache. 
  
  ## 1. Retreive the matrix and store it in a variable called data
  data <- x$get()
  
  ## 2. Compute the inverse of the matrix stored in data and store it
  ## in the variable i.
  i <- solve(data, ...)
  
  ## 3. Store the inverse of the matrix into cache
  x$setinverse(i)
  
  ## 4. Print the inverse of the matrix stored in the variable i
  i
}