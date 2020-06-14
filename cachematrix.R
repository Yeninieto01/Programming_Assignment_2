# # Esta función crea un objeto.
# # El objeto almacena el inverso de una matriz en un caché.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# # La función cacheSolve calcula el inverso de la matriz dada.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv) }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## a <- matrix(rnorm(9),3,3)
## a1 <- makeCacheMatrix(a)
## cacheSolve(a1)
