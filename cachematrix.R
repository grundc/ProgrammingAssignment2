

## The function "makeCacheMatrix" gives back function list
## to access a data matrix and their inverse matrix

makeCacheMatrix <- function(x = matrix()) {

      inv <- NULL
      
      ## internal functions to access and set the base matrix
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      
      ## internal object functions to access and set inverse matrix
      setinvert <- function(invert) inv <<- invert
      getinvert <- function() inv
      
      ## list object that represents the base matrix and inverse matrix access functions
      list(set = set, get = get,
           setinvert = setinvert,
           getinvert = getinvert)
      
}


## The function "cacheSolve" checkes if a cachedMatrix's inverse matrix has already been
## generated. IN case it wasn't , it initiates the inverse matrix by calling the appropriate function
## and sending a message. If the inverse matrix already exists, it will simply give back the stored value.

cacheSolve <- function(cachedMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      inv <- cachedMatrix$getinvert()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      message("Creating inverse matrix")
      data <- cachedMatrix$get()
      inv <- solve(data)
      cachedMatrix$setinvert(inv)
      inv
}


