# Example - caching the mean of a vector

# Create a vector (list with embedded function) which:
# 1) Sets the value of the vector
# 2) Gets the value of the vector
# 3) Sets the value of the mean
# 4) Gets the value of the mean
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

# Calculate mean of vectors from makeVector()
# If mean cached, return cached value
# Otherwise calculate mean, cache it, and return it
cacheMean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

