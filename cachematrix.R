## This functions helps to avoid wasteful computation of inverse
## of matrix if done already by using cached result


## This function takes a matrix and returns a special type of 
## cacheMatrix (i.e list) with operations set and get for matrix
## setMatInverse and getInverse for inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  matInverse <- NULL
  
  set <- function(m) {
    if(!identical(x, m)) {
      x <<- m
      matInverse <<- NULL
    }else {
      message("Matrix not updated. Old and new matrix are same")
    }
  }
  
  get <- function()
    x
  
  setMatInverse <- function(m) {
    dim_m <- dim(m)
    dim_x <- dim(x)
    
    if (identical(dim_x, dim_m)) {
      matInverse <<- m
    } else {
      message("Dimension mismatch between given matrix and inverse")
    }
  }
  
  getMatInverse <- function()
    matInverse
  
  list(
    set = set,
    get = get,
    setMatInverse = setMatInverse,
    getMatInverse = getMatInverse
  )
}


## This function take special cacheMatrix and return inverse
## from cache if exist already or else computes inverse and save to cache

cacheSolve <- function(x, ...) {
  matInverse <- x$getMatInverse()
  
  if(!is.null(matInverse)){
    message("Using cached data")
    return(matInverse)
  }
  
  mat <- x$get()
  matInverse <- solve(mat, ...)
  x$setMatInverse(matInverse)
  
  matInverse
}
