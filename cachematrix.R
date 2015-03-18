## makeCacheMatrix: Returns a list of functions that:
## Set the value of the matrix
## Get the value of the matrix
## Set the value of the matrix inverse
## Get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  ## object to store the cached inverse and set to null
  store_inv <- NULL
  ## Set the Matrix function
  ## can be used to set a new matrix, default is that
  ## the matrix is passed as originating function arguement
  ## if new matrix provided, also clear any cached inverse
  setmatrix <- function(y) {
     x <<- y
     store_inv <<- NULL
  }
  
  ## Get Matrix function (passes matrix as arguement)
  getmatrix <- function() x
  
  ## Set Matrix Inverse function (inverse passed as arguement)
  setinv <- function(solved_inv) store_inv <<- solved_inv
  
  ## Get the Matrix inverse function (passes inverse as arguement)
  getinv <-function() store_inv
  
  ## Return the list of defined functions
  list(set=setmatrix, get=getmatrix, setinv=setinv, getinv=getinv)
}


## CacheSolve: Returns the inverse of the matrix if already solved,
## Otherwise solves and caches the matrix inverse

cacheSolve <- function(x, ...) {
  ## call the getinv() function to get the inverse cache object
  store_inv <-x$getinv()
  
  ## Check value to see if inverse is already cached and 
  ## return cache if exists
  if(!is.null(store_inv)) {
    message("getting cached data")
    return(store_inv)
  }
  
  ## Otherwise if not already cached
  ## call the get() function to get the matrix
  data_matrix <- x$get()
  ## create the inverse using solve() function
  store_inv <- solve(data_matrix, ...)
  
  ## call setinv() function to cache the inverse
  x$setinv(store_inv)
  
  ## Return solved inverse
  store_inv
}
## :Test case: This is the test script executed to validate
## Define a matrix
## > x <- rbind((1:2),(3:4))
## call makeCacheMatrix function with matrix as arguement
## > tst <- makeCacheMatrix(x)
## Check object returns the four functions
## > tst
## $set
## function (y) 
## {
##   x <<- y
##   store_inv <<- NULL
## }
## <environment: 0x11080d88>
##   
##   $get
## function () 
##   x
## <environment: 0x11080d88>
##   
##   $setinv
## function (solved_inv) 
##   store_inv <<- solved_inv
## <environment: 0x11080d88>
##   
##   $getinv
## function () 
##   store_inv
## <environment: 0x11080d88>
## 
## Check the object get function returns the Matrix as passed
## > tst$get()
##      [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## Call the cacheSolve function the first time
## > cacheSolve(tst)
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
##
## Call the cacheSolve function again
## Shows inverse came from cache second time round
## > cacheSolve(tst)
## getting cached data
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5