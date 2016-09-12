# Function to cache the inverse of a matrix.

# Function : makeCacheMatrix :

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
## First : set the value of the matrix and get the value of the matrix
  set <- function(y) {  
    x <<- y
    inv <<- NULL
  }
  get <- function() x
## Second: set the value of inverse of the matrix and get the value of the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
## Third:: Return a list if function
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# Function to return the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# Function cacheSolve
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
## First: Only returns cached matrix inverse if the inverse has already been computed  
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
## Second: Compute the inverse of matrix
  data <- x$get()
  inv <- solve(data)
## Third: It caches inverse
  x$setinverse(inv)
## Then return inverse of matrix
  return (inv)
}

## EXEMPLE
## the matrix
# x<- matrix (c(1, 2, 3, 4), nrow=2, ncol=2, byrow=TRUE)
# x
#      [,1] [,2]
#[1,]    1    2
#[2,]    3    4
# m<-makeCacheMatrix(x)
# cacheSolve (m)
#       [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# cacheSolve (m)
# getting cached data.
#       [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
