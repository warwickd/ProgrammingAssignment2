## The two functions below can be used to create and re-use the inverse of an invertible matrix. 
## To use the functions first run makeCacheMatrix on an invertible matrix:
### Say we have matrix a
##      [,1] [,2] [,3]
##      [1,]    2    0    0
##      [2,]    0    4    0
##      [3,]    0    0    9
##       z <- makeCacheMatrix(a)
## Then run cacheSolve on z
##      cacheSolve(z)
##      > cacheSolve(z)
##             [,1]    [,2]   [,3]
##      [1,] 0.3333333 0.00 0.0000000
##      [2,] 0.0000000 0.25 0.0000000
##      [3,] 0.0000000 0.00 0.1111111


## makeCacheMatrix creates a special list that can be used to:
##      set the value of the matrix
##      get the value of the matrix
##      set the value of the matrix inverse
##      get the value of the matrix inverse

makeCacheMatrix <- function(x = numeric()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(solve) inv<<- solve
        get_inverse <- function() inv
        z<-list(set = set, get = get,set_inverse = set_inverse, get_inverse = get_inverse)
        z
 }


## cacheSolve returns the cached value of the inverse of a matrix passed from the result of makeCacheMatrix
## or, if the inverse is not yet cached, calculates  the inverse of 'x'

cacheSolve <- function(x, ...) {
        inv <- x$get_inverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inverse(inv)
        inv
}
