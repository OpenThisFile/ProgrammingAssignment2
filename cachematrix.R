## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
### 
#The first function, makeCacheMatrix is really a list, which creates a special matrix
#being consisted of 1. setting the value of the matrix(defined by set)
# 2. getting the value of the matrix(defined by get)
# 3. setting the value of inverse(setinverse)
# 4. getting the value of inverse (getinverse)
# This should be regarded as a kind of a list of functions
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
}
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Write a short comment describing this function
# The function below computes the inverse of the function above, but if there already 
#is a value(the inverse is already calculated and stored,
#then the response will be "getting cached data"
#with the value, of course
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
        ## Return a matrix that is the inverse of 'x'
}
d <- matrix(c(1:4),(2:3))
d

d2test <- makeCacheMatrix(d)
cacheSolve(d2test)
d2test
d2test
cacheSolve(d2test)
solve(d)
inverse(d)
# above is just an example of a typical matrix creation stored in
#d), which is used for the #testing of the functions created
