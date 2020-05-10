#Inverse of a matrix

#First function (makeCacheMatrix): 

#  This function is used to create a matrix object  
#  This matrix object will be used to cache the inverse of the matrix

makeCacheMatrix<- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


#Second function(cacheSolve):

#  This function will Return a matrix that is the inverse of the 'x' matrix
#  First it checks if the inverse of the matrix has been calculated before and if the matrix has changed
#  If the inverse of the matrix has been calculated before then the function will retrieve it from the cache

cacheSolve<- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
