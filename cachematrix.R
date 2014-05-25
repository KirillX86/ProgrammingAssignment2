makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinvers <- function(solve) s <<- solve
        getinvers <- function() s
        list(set = set, get = get,
             setinvers = setinvers,
             getinvers = getinvers)
}

cacheSolve <- function(x, ...) {
        s <- x$getinvers()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinvers(s)
        s
}
