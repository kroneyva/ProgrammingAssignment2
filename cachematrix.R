## Second R programming class assignment
## two function makeCacheMatrix and cacheSolve

## makeCacheMatrix is a functionto create a special matrix object that is able to cache the
## return by cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        #set the matrix
        minv<- NULL
        set<- function(y) {
                x<<-y
                minv<<-NULL
        }
        #get the matrix
        get <- function() x
        
        #set the inverse matrix 
        setinv <- function(inv) minv<<- inv
        
        #get the inverse matrix
        getinv <- function() minv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve uses the solve() function to compute inverse on a square matrix returned by the makeCacheMatrix fucntion

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getinv()
        if (!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        
        mdata <- x$get()
        minv <- solve(mdata, ...)
        x$setinv(minv)
        minv #returns the inverse matrix
}
