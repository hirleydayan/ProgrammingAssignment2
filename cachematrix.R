#' This function creates a \code{matrix} object that can cache its 
#' inverse through \code{solve} function.
#' 
#' @param x An inversible matrix.
#' 
#' @examples
#' m <- makeCacheMatrix(replicate(3, rnorm(3)))
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


#' This function computes the inverse of the \code{matrix} returned by 
#' \code{makeCacheMatrix} function. If the inverse has already been 
#' calculated, and the matrix has not changed, it should retrieve the 
#' inverse from the cache.
#' 
#' @param x An inversible matrix.
#' @return The inverse of a \code{matrix}.
#' 
#' @examples 
#' cacheSolve(m)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}