# To define a function namely CacheMatrix with the purpose of creating a "matrix" object which can cache its inverse 
CacheMatrix<-function(x=matrix()){
  # To define variable Matrix 
  Matrix <- NULL
  # To set the value of x
  set <- function(y) {
    x <<- y
    Matrix <<- NULL
  }
  # To utilize get function in searching for values of x
  get <- function() x
  # To define a function namely setinverse in setting the values of the inverse matrix
  setinverse <- function(solve) Matrix <<- solve
  # To define a function namely getinverse in getting the values of the inverse matrix
  getinverse <- function() Matrix
  # To use list function in listing down the existing sub-functions under the CacheMatrix function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# To define a function namely CacheSolve witht the purpose of computing the inverse of the "matrix" returned by CacheMatrix
CacheSolve <- function(x, ...) {
  # To get the inverse matrix that calculated previously
  Matrix <- x$getinverse()
  # To check the successfulness of the previous calculation
  # If it is not a null value, it will skip the computational steps below and return the values for Matrix
  if(!is.null(Matrix)) {
    message("Obtaining cached data...")
    return(Matrix)
  }
  # When the condition is not true, coputational steps stated below will be continued
  data <- x$get()
  Matrix <- solve(data, ...)
  x$setinverse(Matrix)
  Matrix
}