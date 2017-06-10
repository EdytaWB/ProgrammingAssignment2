## It can happen that the same operations have to be recomputed many times which
# affects performance. Fortunately, given that the vector content is not changing,
# it is possible to cache those time-consuming calculations and improve performance. 
# Such an example are the below functions that cache the inverse of a matrix (under 
# the assumption that a given matrix is invertible).

## makeCacheMatrix: The aim of the function is to create a matrix that caches its
# inverse



makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) m <<- Inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## cacheSolve: The aim of the function is to calculate the inverse of the matrix 
# returned by the above makeCacheMatrix. The function should retrieve the inverse
# of the matrix from cache if it has already been calculated. Otherwise, it should
# compute the inverse of the matrix and set it in the cache.


cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInvers(m)
    m
}
