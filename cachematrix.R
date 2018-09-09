## Set of 2 functions below together, create the inverse of a matrix, stores the inverse matrix in a cache and then retrieve 
## the prior calculated inverse of the base matrix from the cache skipping the need for a new calculation of the same base
## matrix. This saves processing and re calculation resource and time.

## MakeCacheMatrix does the following:
## Defines x to be a matix and I as a NULL vector
## Creates a function called 'set' which states that if y is given then store it as x and the Inverse matrix is a NULL matrix.
##'get' command returns the matrix x. 
## 'setinv' Accepts the inverse matrix and stores it as I
## 'getinv" Returns the inverse matrix I
## Final output is a list containing values of get, set, getinv, setinv

makeCacheMatrix <- function(x = matrix()) {
 I <- NULL
set <- function (y) {
        x <<- y
        I <<- NULL
}
get <- function() x
setinv <- function (solve) I <<- solve
getinv <- function () I
list (set= set, get=get,
setinv=setinv, getinv=getinv)
}

## Function first checks to see if the inverse already exists
## If the inverse exits, it retrieves from cache, else it calculates the new inverse and sends it to cache.
## First step calls for matrix I from getinv list
## Then checks if I is NULL or not. If its not NULL then it returns I
## Else uses get function to return x matrix
## Then solves for the new inverse 
## And finally sets the new inverse in the cache

cacheSolve <- function(x, ...) {
     I <- x$getinv()
     if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinv(I)
        I
}
}
