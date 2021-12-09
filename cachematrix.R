## The functions below cache the inverse of a given matrix


## The first function makeCacheMatrix create an object from an invertible matrix
## The object sets and gets the matrix and also sets and gets the inverse of matrix 

makeCacheMatrix <- function(x = matrix()) 
{
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                }
        get <- function() x
        setinverse <- function(z) inv <<-z
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The second function cacheSolve computes the inverse of the object created with the above function
## It checks to see if the inverse has already been calculated. If available, it gets the inverse from the cache and skips the computation. 
## Else, it computes the inverse of the matrix and sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
                }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inverse)
        inv
}
