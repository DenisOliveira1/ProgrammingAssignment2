## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Creates a object to control a matrix and its inverse value variable

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function
# Gets the cached inverse if it exists, otherwise it will be calculates. In both cases the inverse is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        inv = solve(x$get()) 
        x$setInv(inv)
        inv
}

# Example

a = makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
a$get()
a$getInv()

inverseA = cacheSolve(a)
a$getInv()
inverseA = cacheSolve(a)
