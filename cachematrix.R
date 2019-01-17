## Creating two functions. the makeCacheMatrix function will output a list where each
## component of the list is a function. cacheSolve works with makeCacheMatrix to determine
## whether the inverse of a matrix has already been cached. If so, it returns that inverse,
## and if not, it uses the makeCacheMatrix to create and output the inverse.

## MakeCacheMatrix takes a matrix, solves for the inverse, and then caches that inverse in
## the variable m. It outputs a list of functions which can be used to either set or get 
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## CacheSolve first tries to get the inverse of the matrix from the makeCacheMatrix function.
## If the inverse has not already been calculated, then it will calculate the inverse and
## cache it using the setinverse list item in the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message ("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
