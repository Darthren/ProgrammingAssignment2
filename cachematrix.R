## The first function creates a matrix and calculates its inverse, which will store in the cache.
## The second function returns the inverse of the matrix from the cache

## makeCacheMatrix will take a matrix as its argument, will calculate its inverse, and it'll store
## it in the cache. It returns a list with 4 functions:
        ## set: this will set the values of the matrix
        ## get: it will get the values of the matrix
        ## setsolve: it will create the inverse of the matrix
        ## getsovlve: it will get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
                       s <- NULL ##creates the variable "s"
                set <- function(y) { ##function that sets the values of the matrix
                        x <<- y ## stores the input in a variable called "x"                        
                        s <<- NULL ## creates variable "s" in the parent directory
                }                
                get <- function() x ## function that returns X as its value
                setsolve <- function(solve) s <<- solve ## calculates the inverse of the matrix
                getsolve <- function() s ## returns the inverse of matrix
                list(set = set, get = get, ## creates a list with the 4 functions described above
                     setsolve = setsolve,
                     getsolve = getsolve)
}


## cacheSolve looks for the inverse of the matrix stored in the cache
## if it doesn't exist, or if the matrix has changed, then cacheSolve
## will compute the inverse.

cacheSolve <- function(x, ...) {
                s <- x$getsolve() ## this will get the cache.
                
                if(!is.null(s)) { ## looks for the existence of the inverted matrix 
                message("Getting cached data") #if it exists, it will return the inverted matrix
                return(s)
        }
        data <- x$get() ## if the inverse doesn't exist, then it will calculate it
        s <- solve(data, ...)
        x$setsolve(s)
        s        
}
