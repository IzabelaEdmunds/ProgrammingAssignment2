## The first function, 'makeCacheMatrix' creates  a special "matrix" object that 
## can cache its inverse which is really a matrix containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    }
## set the matrix
  get <- function() x
## get the matrix
  setinverse <- function(inverse) inv <<- inverse
## set the inverse of the matrix
  getinverse <- function() inv
## get the inverse of the matrix
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
# a list of the methods
}

## The second function 'cacheSolve' calculates the inverse of the special "matrix" 
## created with the above function 'makeCacheMatrix'. 
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
## Return a matrix that is the inverse of 'x'
  if(!is.null(inv)) {
    message("getting cached data.")
## check to see if the inverse has already been calculated
    return(inv)
## if true get the inverse from the cache and skip the computation
  }
  data <- x$get()
## if false calculate the inverse of the data 
  inv <- solve(data)
## and set the value of the inverse in the cache
x$setinverse(inv)
## via the setinverse function.
inv
## return inverse
}