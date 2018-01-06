## to test how these functions work, we can create a matrix first
## and give it to the first function. this function creates a 
## special matrix which is a list. 
## then we can give the result to the second function 
## to calculate the inverse of matrix and set the cache or
## return the value of the inverse from cache if it's 
## been calculated before.

## this is an example to check the result for these two functions:
##      mymat<-matrix(c(3,4,6,2),nrow=2,ncol=2)
##      mylist <- makeCacheMatrix( mymat )
##      invmat<- cacheSolve(mylist)



## makeCacheMatrix creates a special "Matrix", which is really a list containing the following functions:

##1-set(): set the value of the matrix
##2-get(): get the value of the matrix
##3-setinverse(): set the value of the inverse
##4-getinverse(): get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function calculates the inverse matrix for special matrix x
##     Matrix x is the result of funtion makeCacheMatrix.
##however it checks if the inverse has already been calculated. 
##If so, it gets the inverse matrix from the cache.
##Otherwise it calculates the inverse and set the value of it in cache via setinverse() function.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  if( det(data) == 0 )
      print("this matrix is not invertable")
  else{
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
  }
}
