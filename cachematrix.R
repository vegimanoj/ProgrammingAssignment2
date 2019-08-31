## Put comments here that give an overall description of what your
## functions do

##The overall objective is to reduce the reptitve computation of matrix inversion
##and instead use cache to retreive the inverse.
##Functions:
##set-The  value of the input matrix and the initialization of the inverse matix will be cached.
##get-The cached value of the input matrix will be retreived
##setmatinv-computes the inverse of the matrix and caches the output value.
##getmatinv-retreive the cached inverse of the matrix.

## Write a short comment describing this function

##This function creates the cache of the special "matrix" object that can hold both the value and the inverse of the value.
##The input argument is a matrix that is invertible and the return value is a list of the executed function handlers.
makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setmatinv <- function(solve) matinv <<- solve
        getmatinv <- function() matinv
        list(set = set, get = get,
             setmatinv = setmatinv,
             getmatinv = getmatinv)

}

## Write a short comment describing this function

##The function retreives the inverse of the special "matrix" from the cache returned from makeCacheMatrix which is passed as an argument to this function.
##If the cache is already calculated,then the inverse value is retreived  or else the function computes the inverse of the matrix and sets the value in the cache using the setmatinv function.
##The return value is the inverse of the matrix. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv <- x$getmatinv()
        if(!is.null(matinv)) {
                message("getting cached inverse matrix")
                return(matinv)
        }
        data <- x$get()
        matinv <- solve(data, ...)
        x$setmatinv(matinv)
        matinv
}
