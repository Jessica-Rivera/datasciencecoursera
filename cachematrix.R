## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        set<-function(y){
                x<<-y
                inverse<<-NULL
        }
        get<-function()x
        setInverse<-function(SolMatrix)inverse<<-SolMatrix
        getInverse<-function()inverse
        list(set=set,get=get,
                setInverse=setInverse,
                getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$getInverse()
        if(!is.null(inverse)){
                message("Getting cached data.")
                return(inverse)
        }
        data<-x$get()
        inverse<-solve(data, ...)
        x$setInverse(inverse)
        inverse
}
