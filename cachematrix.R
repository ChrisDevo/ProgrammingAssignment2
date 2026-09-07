## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cache_matrix <- NULL
  set <- function(y) {
    x <<- y
    cache_matrix <<- NULL
  }
  get <- function() x
  set_cache_matrix <- function(cache_matrix) cache_matrix <<- solve
  get_cache_matrix <- function() cache_matrix
  list(set = set, get = get,
       set_cache_matrix = set_cache_matrix,
       get_cache_matrix = get_cache_matrix)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_cache_matrix()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_cache_matrix(m)
  return(m)
}
