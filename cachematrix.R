## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes an invertible square matrix as an argument 
## initially sets the inverse to null, then adds the original matrix and functions
## to a list

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve takes the list from makeCacheMatrix and checks if the inverse has been calculated,
## if not, it calulates it via the solve() function, and calls setinverse() from the list, which caches
## the inverse, 
## if it does exist, it retrieves it from the cache via the function getinverse. this way the 
## inverse is only calculated once.
## checked per the following commands:
## matrix1 <-matrix(c(1,2,3,4,5,6,7.4,8,9),3,3)
## invmatrix1 <- makeCacheMatrix(matrix1)
## cacheSolve(invmatrix1)
## cacheSolve(invmatrix1) ## this duplicate command shows the inverse in indeed cached.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() # x[["getinverse()"]]
  if(!is.null(i)) {
    message("getting cached solution")
    return(i)
  }
  inputmatrix <- x$get()  
  i <- solve(inputmatrix, ...)
  x$setinverse(i)  
  i
}
