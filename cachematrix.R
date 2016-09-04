#Caching the inverse of a Matrix
#Matrix inversion is usually a costly computation and there may be some benefit
#to caching the inverse of a matrix rather than compute it repeatedly. 
#The following functions together takes the advatange of caching.

## The below function creates a special "matrix" object that can cache its inverse.
## The function returns a list object contains four elements which are functions
##      set - set the value of the matrix
##      get - get the value of the matrix
##      setinvmat - set the inverse matrix
##      getinvmat - get the inverse matrix 

## Usage: makeCacheMatrix(x)
## Arguments:
## x    a square numeric or complex matrix containing the coefficients of the
##      linear system. Logical matrices are coerced to numeric.



makeCacheMatrix <- function(x = matrix()) {
        if(class(x)=="matrix"){
        invmatrix <- NULL
        set <- function(y) {
                x <<- y
                invmatrix <<- NULL
        }
        get <- function() x
        setinvmat <- function(solved) invmatrix <<- solved
        getinvmat <- function() invmatrix
        list(set = set, get = get,
             setinvmat = setinvmat,
             getinvmat = getinvmat)
        } else {
                message("The Argument to this function should be a matrix")
        }
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache
## Usage: cacheSolve(x,...)
## Arguments:
## x    a list object created using the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvmat()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        if(dim(data)[1]==dim(data)[2]) {
                m <- solve(data)
                x$setinvmat(m)
                m
        } else {
                message("Matrix is not a square or complex matrix.")
                return(data)
        }
}
