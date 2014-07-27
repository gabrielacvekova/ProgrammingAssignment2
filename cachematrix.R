-## Put comments here that give an overall description of what your
 -## functions do
 +## Processed functions invole caching the invese of the matrix. The brief is to reduce recurring calculation of the inverse.
  
 -## Write a short comment describing this function
  
 -makeCacheMatrix <- function(x = matrix()) {
  
 +#Following function set up a particular object of the matrix that can cache its inverse.
 +makeCacheMatrix <- function(x = matrix()) {
 +        inv <- NULL # the inverse initialized to NULL
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
  
 +##Following function calculate the inverse of the particular object of matrix returned by makeCacheMatrix atop. 
 +# When the inverse has already been calculated (without change of the matrix),
 +# the inverse from the cache will be acquired by cachesolve function.
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
