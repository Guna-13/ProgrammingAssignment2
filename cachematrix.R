## I calculate the inverse of a matrix, keeping both (original and inverse) in cache for reuse. 

## For the inverse I use "solve(a, b, ...)", where I ommit 'b' to obtain the inverse of the matrix 'a'.

## Cache the matrix 'x' and its inverse
##--------------------------------------
## To calculate the inverse of a matrix can be time consumming for a large matrix 
## therefore I kept the a matrix and its inverse in a list to be reused.
## The values are only accessible through functions.
makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  #--------------------------
  # 1: About the input matrix
  #--------------------------
  
  # 1.1: When I introduce (in this case using the funtion "set") a new matrix, 
  #           I have to delete the inverse matrix, as it corresponds to a previous matrix.
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  
  # Part 1.2: I can get the matrix that I have introduce with "set"
  #           or when using "makeCacheMatrix".
  get <- function() x
  
  #----------------------------
  # 2: About its inverse matrix
  #----------------------------
  
  # 2.1: When I get the inverse of the matrix I keep it in "inv_x"
  setinverse <- function(inverse) inv_x <<- inverse
  
  # 2.2: I can get the inverse matrix that I have kept before with "setinverse"
  getinverse <- function() inv_x
  
  #-------------------------------
  # 3: Result of the main function
  #-------------------------------
  # 3.1: To be able to access the 4 funtions I have created here a list with them.
  list( set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Return the inverse of a matrix
##-------------------------------
## I return the inverse of a matrix with this function. If it was the first time I called it with
## a new mytrix I calculate the inverse and keep the value using the list defined before. 
## If I previously calculated the inverse I only need to retrieve the value I kept before using the 
## funtions of the list defined before.
cacheSolve <- function(x, ...) {
# 'x' is a list like the one I deliver in the previous function.
# 'x' is therefore not a matrix. The matrix is cached in 'x' 
#     and only through the function 'get' accessible.

  #Had I already calculated the inverse matrix before?
  
  inv_x <- x$getinverse()
  if(!is.null(inv_x)) {
  # Yes: I return the value I have kept before.
    message("getting cached data")
    return(inv_x)
  }
  
  # No: I calculate the inverse and kept the value for reuse.
  data <- x$get()
  inv_x <- solve(data, ...)
  x$setinverse(inv_x)
  inv_x
}
