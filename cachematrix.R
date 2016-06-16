makeCacheMatrix <- function(x = matrix(), ...) {
        
        i <- NULL
        set <- function(y = matrix()) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse 
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
             
}



cacheSolve <- function(x=makeCacheMatrix(1:9, nrow=3, ncol=3), ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        dat <- x$get()
        i <- solve(dat)
        x$setinv(i)
        i
}
