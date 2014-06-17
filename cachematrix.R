## Initialization of a matrix which has to be inverted
## MyMatrix <- matrix(runif(4), 2, 2)

v <- makeCacheMatrix()

print ("Create a non singular matrix in your console typing for example:")
print ("MyMatrix <- matrix(c(5, 1, 7, 4), 2, 2) or other non singular matrix)")
## print (MyMatrix)

v$set(MyMatrix)

print ("Then type cacheSolve(x) to solve the inverse matrix first time")
print ("Next time you type cacheSolve(x) the same inverted matrix will be returned")
## print ("If you want to change the matrix source again the code for initialitation")

##  Caching the Inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
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

## Returns the inversion of a matrix x if it is not cached

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
        ## Return a matrix that is the inverse of 'x' or the cached inversed matrix
}


