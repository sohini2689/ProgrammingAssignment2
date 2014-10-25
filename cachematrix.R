## Together, the two functions ensure that a solve() function is applied to 
## a matrix only once. It is then cached and every subsequent call returns the  
## cached inverse-matrix instead of solving each time.

## This function caches the solution (inverse-matrix) and returns a special list
## The '<<-' operator ensures the values `x` and `i` are not "local" to each defined function
## and hence can be used across functions within the parent (makeCacheMatrix) environment
## Caching is enabled by the above.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() {
                x
        }
        setinv <- function(inv) {
                i <<- inv
        }
        getinv <- function() {
                i
        }
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve takes the cached list and invokes the getinv() function 
## which lexically ends up at the value of `i` in parent(makeCacheMatrix) environment 
## On the first invocation of getinv(), `i` has a 'null' value 
## and the inverse is calculated for the first time (line 35-37) 
## more importantly, it is then cached in the makeCacheMatrix environment (line 37)
## On every subsequent invocation, getinv() encounters a cached, non-null `i` 
## and returns that without calling the solve() function again

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
