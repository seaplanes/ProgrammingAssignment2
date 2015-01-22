## Caches the inverse of a matrix and retrieves the inverse if 
## it has been already calculated and the matrix has not changed


## makes a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    set<- function(y) {
      x<<- y
      inv<<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## checks to see if the inverse has already been calculated, and if so,
## returns the stored value if inverse is not stored, calculates the inverse

cacheSolve <- function(x, ...) {
       inv <- x$getInverse()
       if(!is.null(inv)) {
         message("getting cached data")
         return(inv)
       }
       data<- x$get()
       inv <- solve(data)
       x$setInverse(inv)
       inv
}
