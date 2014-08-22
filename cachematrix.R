makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
			if(!identical(y, x)){
                x <<- y
                s <<- NULL
			}
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        cache <- list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
		return(cache)
}

cacheSolve <- function(y, ...) {
	   s <- y$getsolve()
	   if(!is.null(s)) {
			   message("getting cached data")
			   return(s)
	   }
	   data <- y$get()
	   invS <- solve(data, ...)
	   y$setsolve(invS)
	   return(invS)
}
