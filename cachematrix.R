-## Put comments here that give an overall description of what your
 -## functions do
 +## This is programming assignment 2 of thhe R programming course
 +## The two functions cache the inverse of a matrix. The objective is it avoid repetitive computation of the Inverse
 +## IF the original matrix has not changed between two calls to the Solve() function
  
 -## Write a short comment describing this function
  
 -makeCacheMatrix <- function(x = matrix()) {
  
 +#This function creates a particular "matrix" object that can cache its inverse.
 +makeCacheMatrix <- function(x = matrix()) {
 +        inv <- NULL # initialize the inverse to NULL
 + 
 +        set <- function(y) {
 +                x <<- y
 +                inv <<- NULL
 +        }
 +        get <- function() x
 +        setInverse <- function(Val) inv <<- Val
 +        getInverse <- function() inv
 +        list(set = set, get = get,
 +                setInverse = setInverse,
 +                getInverse = getInverse )
  }
  
  
 -## Write a short comment describing this function
  
 +##Following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
 +# When the inverse has already been calculated (and the matrix has not changed),
 +# Then the cachesolve function will retrieve the inverse from the cache.
  cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
 +         inv <- x$getInverse()
 +         if(!is.null(inv)) {
 +                message("getting cached data")
 +                return(inv)
 +        }
 +        data <- x$get()
 +        inv <- solve(data)
 +        x$setInverse(inv)
 +        inv
  }
