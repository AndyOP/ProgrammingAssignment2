## Put comments here that give an overall description of what your
## functions do

### The functions are designed to reduce R processing time by: 
### 1. Creating a list of functions to be cached;   
### 2. Using the results derived/cached in 1. (where possible) rather than re-calculating;    
### 3. Using R's Lexical Scoping design to assign values to objects (in a different environment).  
### This significantly reduces processing time.

## Write a short comment describing this function

### makeCacheMatrix creates a matrix that caches it's inverse.
### It sets/(gets) the matrix/(inverse) and the list output is used in cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        i = NULL
        set <- function(y) {
                x <<- y
                i <<- NULL 
        }        
  
                get <- function() x
        
                seti <- function(inverse) i <<- inverse
                geti <- function() i
                list(set = set, get = get, seti = seti, geti = geti)
}


## Write a short comment describing this function

### cacheSolve retreves the inverse of the makeCacheMatrix 
### or will derive this if no inverse has been previously cached

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        i <- x$geti()
        if(!is.null(i)){
        message("getting cached data")
        return(i)
        }  
        
                data <- x$get()
                i <- solve(data, ...)
                x$seti(i)
                return(i)
}
