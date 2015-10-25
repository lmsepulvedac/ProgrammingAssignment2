## This functions calculate the inverse of a given matrix. If the inverse is already calculate it takes the cache and shows this result.
## If the inverse it is not calculate, its use the solve function

## The makeCacheMatrix creates a list with the functions set and get for the input matrix and set_inverse_matrix and get_inverse_matrix
## for the inverse

makeCacheMatrix <- function(x = matrix()){
  inverse_matrix<-NULL
  set <- function(y){
    x <<- y
    inverse_matrix <<-NULL
  }
  get <- function()x
  set_inverse_matrix <- function(solve) inverse_matrix <<- solve
  get_inverse_matrix <- function() inverse_matrix
  list(set = set, get = get,
       set_inverse_matrix = set_inverse_matrix,
       get_inverse_matrix = get_inverse_matrix)
  
  
}

## The function checks if the inverse is already calculated, if so, it returns that value, if not, it returns the answer with the solve function

cacheSolve <- function(x, ...) {
  inverse_matrix <- x$get_inverse_matrix()
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$set_inverse_matrix(inverse_matrix)
  inverse_matrix
    ## Return a matrix that is the inverse of 'x'
}
