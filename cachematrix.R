##Create list with functions for setting and getting matrix and it's inverse
##Get matrix that will be inverted in function "cacheSolve"
##Return list of functions for setting and getting matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
		##set function for matrix
        set <- function(y) {
			##if matrix is same its inverse mustn't be reset and recalculated
            if(!identical(y, x)){
                x <<- y
                s <<- NULL
            }
        }
		##get function for matrix
        get <- function() x
		##set function for inverse matrix
        setsolve <- function(solve) s <<- solve
		##get function for inverse matrix
        getsolve <- function() s
        cache <- list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
		return(cache)
}

##Calculate inverse matrix in case it not cached in argument
##Get list with matrix and inverse matrix$
##Return inverse matrix
cacheSolve <- function(y, ...) {
	    s <- y$getsolve()
		##if inverse exists it's will not be recalculated
	    if(!is.null(s)) {
            message("getting cached data")
            return(s)
	    }
	    data <- y$get()
		##calculate inverse matrix
	    invS <- solve(data, ...)
	    y$setsolve(invS)
	    return(invS)
}
