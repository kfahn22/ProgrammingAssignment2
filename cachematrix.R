## Collectively, makeCacheMatrix() and cacheSolve():
## --take a matrix m and determine whether the inverse of m is stored in memory
## --if the inverse is found, cacheSolve retrieves the value of the inverse and 
## assigns the value to inv
## --if it is not found, cacheSolve uses solve() to caclulate the inverse and 
## assigns the value to inv


## This function returns a list with four named functions, set(), get(),
## setinverse(), and getinverse():
## set() assigns the value of matrix to the parent environment and clears 
## any previously cached value of inv
## get() retrieves the matrix m
## setinverse() assigns the inverse of m to inv
## getinverse() retrieves the value of inv
## The function returns a list of the  4 named functions so that they can be
## referenced in the cacheSolve function.

makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        set <- function(matrix) {
                m <<- matrix
                inv <<- NULL
        }
        get <- function() {m}
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function() {inv}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}

##The cacheSolve function calculates the inverse of a matrix.  It takes as an 
##argument the list of functions defined in makeCacheMatrix. 
## It first checks to see if the inverse has already been calculated by
##calling x$getinverse.  If it has, it retrieves the inverse from the cache.
##Otherwise, it calls x$get() to retrieve the inverse, solve() to find the 
##inverse, and x$setinverse() to set the value of inv. When cacheSolve is called it
##returns the inverse of the matrix m, which was inputed as an argument to 
##makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        }

##Generate sample matrix to test makeCacheMatrix() and cacheSolve()
set.seed(22)
msample <- sample(1:1000, 100)
m1 <- matrix(msample, nrow=10, nrow=10)
fn_list <- makeCacheMatrix(m1)
inverse_matrix <- cacheSolve(fn_list)
