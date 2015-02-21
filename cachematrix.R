## This pair of functions cache the inverse of a matrix.

## The first function creates a 'special' matrix object that can cache its
## reverse.

makeCacheMatrix <- function(x = matrix()) {
        mtx <- NULL
        set <- function(y) {
                x <<- y
                mtx <- NULL
        }
        get <- function() x
        setmtx <- function(solve) mtx <- solve
        getmtx <- function() mtx
        list(set = set, get = get, setmtx = setmtx, getmtx = getmtx)

}

## The second function computes the inverse of the matrix returned by the first
## function. If the inverse has already been calculated, then it retrieves the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        mtx <- x$getmtx()
        if (!is.null(mtx)){
                message ("Getting cached data")
                return(mtx)
        }
        matrix <- x$get()
        mtx <- solve(matrix, ...)
        x$setmtx(mtx)
        mtx
}
