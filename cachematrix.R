## Initialization of a matrix which has to be inverted

print ("******Creating a nonsingular Matrix *******")
print ("Creating MyMatrix and reseting i its inverted in cached memory")

        i <- makeCacheMatrix()
        MyMatrix <- matrix(c(1,5,9,6), 2, 2) 
        i$set(MyMatrix)

print (MyMatrix)

print ("**************************************************")
print ("If you want to change MyMatrix you should type")
print ("take care it is a nonsingular matrix")
print ("i <- makeCacheMatrix()")
print ("MyMatrix <- matrix (c(?, ?, ?, ?), 2, 2))")
print ("i$set(MyMatrix)")
print ("**************************************************")



print ("Type cacheSolve(i) to solve the inverse matrix first time")
print ("Next time you type cacheSolve(i) the same inverted matrix will be returned")
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


