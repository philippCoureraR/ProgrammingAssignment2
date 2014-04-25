## The first function, `makeCacheMatrix` creates a special "vector", which is
##really a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse
##4.  get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


##The following function calculates the inverse of the matrix
##created with the above function. However, it first checks to see if the
##inverse has already been calculated. If so, it `get`s the inverse from the
##cache and skips the computation. Otherwise, it calculates the inverse of
##the data and sets the value of the inverse in the cache via the `setsolve`
##function.


cacheSolve <- function(x, ...) {
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}


temp <-makeCacheMatrix(x<-matrix(1:4,2,2))
cacheSolve(temp)

