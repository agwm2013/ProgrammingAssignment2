## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix 

## Computing the inverse of a square matrix can be done with the `solve`
## function in R.. Therefore, it is assumed the supplied matrix is always invertible.

## Create special 'matrix' which is really a list containing functions to get/set the value
## of the matrix and get/set the vaulue of the inverse of the matrix using 'setsolve' and 'getsolve'
makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setsolve <- function(solve) m <<- solve
                getsolve <- function() m
                list(set = set, get = get,
                        setsolve = setsolve,
                        getsolve = getsolve)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse of the matrix has previously been calculated. If so, it `get`s the
## inverse from the cache and skips the computation. Otherwise, it calculates the 
## inverse of the the matrix using 'solve' and sets the value of the matrix inverse
## in the cache via the `setsolve`function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
