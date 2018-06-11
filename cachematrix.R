## Put comments here that give an overall description of what your functions do

# Create a Cacheable matrix
# input:
#   x:  Optional matrix
# output:
#   list of functions for operating on cacheable matrix:
#       set:  Initialize matrix
#       get:  Return current matrix
#       setinv:  Initialize inverse matrix (used by cacheSolve for caching)
#       getinv:  Get inverse matrix
makeCacheMatrix <- function(x=matrix()) {
    # Local variable to store inverse matrix result
    inverseMatrix <- NULL

    # Initialize matrix
    set <- function(newMatrix) {
        x <<- newMatrix
        inverseMatrix <<- NULL
    }
    
    # Retrieve current matrix
    get <- function() x

    # Save inverse matrix (used by cacheSolve())
    setinv <- function(solution) inverseMatrix <<- solution

    # Retrieve inverse matrix
    getinv <- function() inverseMatrix

    # Return list of functions
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# Solve for inverse matrix of x, cache result, return result
# input:
#   x:  matrix from makeCacheMatrix()
# output:
#   inverse matrix of matrix stored in x
# Note:  This function will check if the inverse matrix has already been
#        computed and cached.  If yes it will return that.  If no, it will
#        computer and store (cache) the answer before returning it.
cacheSolve <- function(x, ...) {
    # See if solution already cached
    inverseMatrix <- x$getinv()
    if (!is.null(inverseMatrix)) {
        message("retrieving from cache...")
        return(inverseMatrix)
    }

    # Otherwise, retrieve matrix
    storedMatrix <- x$get()
    # Solve for inverse matrix
    inverseMatrix <- solve(storedMatrix, ...)
    # Cache result
    x$setinv(inverseMatrix)
    # And return
    inverseMatrix
}

