makeCacheMatrix <- function(x = matrix()) {
    
  s  <- NULL
  
  # Sets the value of the matrix
  
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  # Gets the value of the matrix
  
  get <- function() x
  
  # Sets the value of inverse of the matrix
  
  set_inverse <- function(solve) s <<- solve
  
  # Gets the value of inverse of the matrix
  
  get_inverse <- function() s
  
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## This ficntion takes as an argument another function and uses its lexical scope to operate with data
## Assumes that the matrix is always invertible


cacheSolve <- function(x, ...) {
  
    #Gets inicial or previous value from the context
  
    s <- x$get_inverse()
    
    #Checks the meaning of initial value 
    
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
   
    data <- x$get()
    s <- solve(data)
    
    #Uses this method for saving the result in function enviroment 
    
    x$set_inverse(s)
    s
}

#Result of running a function returns us to an inverse matrix


## Testing:
## x <- rbind(c(1,-3), c(-3,1))

## func <- makeCacheMatrix(x)
## func$get()

##      [,1]  [,2]
## [1,]    1   -3
## [2,]   -3    1

## First run
## cacheSolve(func)

##       [,1]   [,2]
## [1,] -0.125 -0.375
## [2,] -0.375 -0.125

## Second run

## cacheSolve(func)
## getting cached data

##       [,1]   [,2]
## [1,] -0.125 -0.375
## [2,] -0.375 -0.125



