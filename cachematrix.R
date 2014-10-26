## Cache the inverse of the provided matrix to avoid repeating 
## the calculation for inverse

## make the list of functions that get 'x' and a matrix that is the
## inverse of 'x' calculated by cacheSolve, and set a new matrix 'y' 
## and a matrix that is the inverse of 'y' calculated by cacheSolve.
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(sol) s <<- sol
        getsolve <- function() s
        list(set = set, 
             get = get, 
             setsolve = setsolve, 
             getsolve=getsolve)
}

##return a matrix that is the inverse of 'x'
##if the value already exists, it shows the message and return the value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
