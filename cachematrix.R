# In R language, time consuming computations can be cached instead of computing them repeatedly and this pre-calculated
#value can be retrieved whenever it is needed in the program. For example, calculating inverse of a matrix is costly 
#and if it has to be done repeatedly it may take too long to complete which may effect the performance. In such 
#scenarios, the inverse can be calculated once and stored in cache and that pre-calculated value can be pulled rather 
#than recomputing. Below pair of functions demostrates above scenario.


#makeCacheMatrix takes invertable matrix and creates a matrix object which caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mInverse <- NULL
  set <- function(y){
                      x <<- y # assigning value to an object in an environment which is
                                                # different from the current environment.
                      mInverse <<- NULL
                    }
  get <- function()x
  setInverse <- function(inverse) mInverse <<- inverse
  getInverse <- function() mInverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
              # list containing a function to
              # set the value of the Matrix
              # get the value of the Matrix
              # set the value of the inverse
              # get the value of the Inverse
  
}


# Below function takes output of makeCacheMatrix and returns inverse of the matrix. First it checks to see
# if the inverse is already calculated. If yes, it retrieves value from cache else it
# calculates the inverse, stores the inverse value in the cache and returns the computed inverse of the matrix.

cacheSolve <- function(x, ...) {
        
  mInverse <- x$getInverse()
  if (!is.null(mInverse)){ # check to see if the inverse is retrived from cache
    message("getting cached data")
    return(mInverse)       # return cached value
  }
  data <- x$get()          
  mInverse <- solve(data)  # Compute if the value is not cached
  x$setInverse(mInverse)   # Store the value
  return(mInverse)         # Return computed inverse.
}

#Sample run and output:

#x <- matrix(c(1:4), nrow = 2)  
#> x
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4

#m2 <- makeCacheMatrix(x)  --input

#> cacheSolve(m2)         --value is computed when function is called for first time
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5


#> cacheSolve(m2)
#getting cached data   -- retrieving cached value when the function is called for second time
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5




