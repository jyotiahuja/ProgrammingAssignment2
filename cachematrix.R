# usage:
# > x <- matrix(rnorm(9), nrow = 3)           // Create a matrix x
# > cm <- makeCacheMatrix(x)                  // Create a cachable matrix
# > cm$get()                                  // Return the matrix
# > cacheSolve(cm)                            // Return the inverse
# > cacheSolve(cm)                            // Call the 2nd time, so return
#                                             // the cached inverse and print "getting cached data"

# Following method returns a list of functions that set and get a matrix 
# and also set and get inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        # inv stores the cached inverse of matrix
        inv <- NULL
        # matrix setter
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # matrix getter
        get <- function() x
        # setter for matrix inverse
        setInverse <- function(inverse) inv <<- inverse
        #getter for matrix inverse
        getInverse <- function() inv
        # return the list of functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# cacheSolve takes a matrix as input and returns its inverse if its cached already 
# or else compute the inverse and return
cacheSolve <- function(x, ...) {
        # gets the current value of inverse
        inv <- x$getInverse()
        # if inverse is already computed then return the cached copy
        if (!is.null(inv)) {
             message("getting cached data")
             return(inv)
        }
        # we are here because the inverse is not yet calculated so lets compute it
        data <- x$get()
        inv <- solve(data, ...)
        
        # Cache the newly computed inverse
        x$setInverse(inv)
        
        # return inverse
        inv
}
