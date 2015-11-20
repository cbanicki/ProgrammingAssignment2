## makeCacheMatrix stores the cached matrix value at the parent scope
## It also handles capturing, storing, and returning a new cached matrix
## cachesolve performs the inverse operation (solve) as well as calls makeCachMatrix to 
## both retrieve and store the values at the parent scope.


##Declare function 'makeCacheMatrix' that takes a matrix as an argument.

makeCacheMatrix <- function(x = matrix()) {  
  
  ##Instantiate a local variable, 'i', and sets it to NULL
  i <- NULL  
  
  ##Declare a function 'set' that takes an argument 'y' and sets it's value to 'x' 
  ##at the parent scope, also set variable 'i' to NULL at the parent scope
  set <- function(y) { 
    x <<- y
    i <<- NULL
  }
  
  ##Define a function 'get' that returns the value of variable 'x'
  get <- function() x 
  
  ##Define a function 'setInv' that sets 'i' to the value in the passed argument 'inverse' at the parent scope.
  setInv <- function(inverse) i <<- inverse   
  
  ##Define a function 'getInv' that returns 'i'
  getInv <- function() i
  
  ##Returns a vector that contains the functions defined above.
  list(set = set, get = get, 
       setInv = setInv,
       getInv = getInv)
}



## This function calculates the provided matrix's inverse, caches it's value, and returns matrix inverse

cacheSolve <- function(x, ...) {
  
  ## Checks to see if 'i', was set globally already in the makeCacheMatrix function
  ## i.e. it returns the parent (cached) value of the matrix, if there is one.
  i <- x$getInv()
  
  ## if a cached value exists, that value is returned                                             
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## If a cached value does not exist already, then the defined matrix (x) is assigned to 'data'
  ## and then the 'solve' function is used to calculate the inverse, which is in turn assigned it 'i'.
  ## Then 'i' is passed to the makeCachmatrix function where it gets assigned to 'i' at the parent scope
  ## This will be the cached matrix value until it is replaced by a new call to makeCachMatrix
  ## Which will set the cache to null until cacheSolve is called to return the inverse again
  
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}


# ## Examples
## >    source("cachematrix.R")
## >    myMatrix = makeCacheMatrix(matrix(c(10,9,8,7), nrow=2, ncol=2))  #Creates new matrix inverse
## >    myMatrix$get()         # Returns matrix
## >    cacheSolve(myMatrix)   # Creates matrix inverse
## >    myMatrix$getInv()      # Returns the new (inversed) matrix 

