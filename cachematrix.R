## Overall description:
## This two functions give back the inverse matrix for the input matrix.
## The first function (makeCacheMatrix) intends that the calculation in the second function is cached 
## and the calculation will be only done once. 
## The second function (cacheSolve) computes the inverse matrix, if there is nothing saved in the cache
## If the inverse matrix is already calculated, the cached matrix will be the output. 

## "makeCacheMatrix" uses the specified input matrix and creates a vector,
## which contains 4 functions, which specify, what the second function will do:
##   1. set the value of the matrix 
##      (saves the input matrix and sets the varible for the inverse matrix to NULL)
##   2. get the value of the matrix (input matrix)
##   3. set the value of the inverse matrix
##      (uses function "solve" for calculating the inverse matrix)
##   4. get the value of the inverse matrix (output matrix)
##

makeCacheMatrix <- function(input = matrix()) {
  cache <- NULL                                  ## sets the cache with the inverse output matrix to NULL
  set <- function(y) {                           ## new function set:
    newinput <<- y                               ##    1. saves the new input matrix in a variable in another 
                                                 ##       environment
    cache <<- NULL                               ##    2. sets the cache with the inverse output matrix to NULL 
                                                 ##       in another environment
  }
  get <- function() input                        ## new function, which gives back the value of the input matrix
  setinverse <- function(solve) cache <<- solve  ## new function, which calculates the inverse matrix and saves it
  getinverse <- function() cache                 ## gives back the inverse matrix in the cache
  list(set = set, get = get,                     ## output: a list with the four functions 
       setinverse = setinverse,
       getinverse = getinverse)
}


## "cacheSolve" uses the output list of the first function as input and returns a matrix 
## which is the inverse of the input.
## It checks wether the inverse of the matrix is already calculated. 
## If not, it computes the inverse matrix and gives it as output.
## If the inverse is already in the cache, this is given back as output.

cacheSolve <- function(inputlist, ...) {
  inverseofinput <- inputlist$getinverse()       ## assigns the cache to a variable, using the function getinverse
  if(!is.null(inverseofinput)) {                 ## tests if the inverse of the matrix is already in the cache
    message("getting cached data")                       
    return(inverseofinput)                       ## if it is in the cache, the cache is returned as output and 
                                                 ## the function is quitted
  }
  data <- inputlist$get()                        ## if there is nothing in the cache, the input matrix is saved 
                                                 ## in a variable
  inverseofinput <- solve(data, ...)             ## and the inverse is calculated 
  inputlist$setinverse(inverseofinput)           ## saved in the cache  
  inverseofinput                                 ## and given as output
}
