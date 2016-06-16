## The first function makes a list with methods that set and get a matrix and its inverse in an intrinsic environment variable
## The second function is passed the list from the first and attempts to calculate and set its inverse.  If the inverse is already set, the cached value is used

## makeCacheMatrix will create a matrix x, and expose three methods to set/get x and its inverse

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

## given the list variable from the first function, will first check to see if there's already a cached inverse and return
## otherwise will attempt to solve its inverse and set/return it

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
