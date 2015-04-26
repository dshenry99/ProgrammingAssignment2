## Put comments here that give an overall description of what your
## functions do


# makeCacheMatrix creates a list of functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        } 
        get <- function() x
        setmatrix <- function(solve) m <<- solve

        getmatrix <- function() m
        list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}


## Note: the following lines of code were used to test the makeCacheMatrix() and cacheSolve() functions

# m <- matrix(c(-1, -2, 1, 1), 2,2)
# x <- makeCacheMatrix(m)
# m
# inv <- cacheSolve(x)
# inv
# inv <- cacheSolve(x)
# inv
# 
# ## produces the following results
# 
# > m <- matrix(c(-1, -2, 1, 1), 2,2)
# > x <- makeCacheMatrix(m)
# > m
# [,1] [,2]
# [1,]   -1    1
# [2,]   -2    1
# > inv <- cacheSolve(x)
# > inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1
# > inv <- cacheSolve(x)
# getting cached data
# > inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1
# > 
