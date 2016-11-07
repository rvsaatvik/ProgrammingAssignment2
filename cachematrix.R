## Put comments here that give an overall description of what your
## functions do

## Return list that contains all the functions like setting, getting matrix and inverse...
## List is eventually used as the input to cacheSolve



makeCacheMatrix <- function(x = matrix())
{
  inverse = NULL
  setMat = function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  
  getMat = function() x
  setInv = function(inv) inverse <<- inv
  getInv = function() inverse
  list(setMat = setMat,getMat = getMat,setInv=setInv,getInv=getInv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse = x$getInv()
  
  if(!is.null(inverse))
  {
    message("Already computed. Getting the cached Data...")
    return(inverse)
  }
  
  matrixdata = x$getMat()
  inverse = solve(matrixdata,...)
  
  x$setInv(inverse)
  
  return(inverse)
  
}

## This test function can be used to check the reduction in computation time due to caching..
test = function(mat){
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}
