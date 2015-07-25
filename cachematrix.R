##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly 
##To serve this, a pair of functions been written which will cache the inverse of a matrix with assumption the matrix supplied is always invertible.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse which is really a list containing a function to 
## set the value of matrix
## get the value of matrix
## set the value of inverse
## get the value of inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        set <- function(y){
                x <<- y
                inv<<- NULL
        }
        get <- function ()x
        set_inv <- function (inv_input) inv <<- inv_input
        get_inv <- function ()inv
        
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inv()
        if (!is.null(inv)){
                message ("getting cached inverse")
                return (inv)
        }
        data <- x$get()
        inv <- solve (data, ...)
        x$set_inv (inv)
        inv
}

