## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## MakeCacheMatrix takes a matrix that is able to be inverted as a vector,
## then it makes initializes variables for both the current matrix and the cached converted matrix
## to be stored as variables in the global environment.
## While ensuring that the cached inverted matrix is cleared if a new
## normal matrix is given.
## It also makes getter and setter functions for these variables, 
## which will be passed to the next function.

makeCacheMatrix <- function(current_matrix = matrix()) {
  
    ## Initialize the cached variable.
  
    cached_inverted_matrix <- NULL
    
    ## Set the new matrix as a variable in the global environment.
    ## Clear the cached inverted matrix since the matrix has changed.
    
    set <- function(new_matrix) {
      current_matrix <<- new_matrix
      cached_inverted_matrix <<- NULL
    }
    
    # Get the new matrix.
    
    get <- function() current_matrix
    
    # Sets the passed inverted matrix as a variable in the global environment.
    setmatrix <- function(inverted_matrix) cached_inverted_matrix <<- inverted_matrix
    
    # Gets the inverted matrix.
    
    getmatrix <- function() cached_inverted_matrix
    
    # Returns the created functions.
    
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
  }

## Write a short comment describing this function

# CacheSolve takes the above function MakeCacheMatrix
# and checks if there is already a inverted matrix cached
# in the global environment, if not, it creates one.

cacheSolve <- function(passed_function, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    # Gets the cached inverted matrix.
    
    inverted_matrix <- passed_function$getmatrix()
    
    # Check if the cached inverted matrix exists, if not, create it and return it. 
    
    if(!is.null(inverted_matrix)) {
      message("getting cached data")
      return(inverted_matrix)
    }
    new_matrix <- passed_function$get()
    inverted_matrix <- solve(new_matrix, ...)
    passed_function$setmatrix(inverted_matrix)
    inverted_matrix
}

## Testing.

square_matrix <- c(20, 32, 526, 81, 22, 90, 84, 9231,10)
dim(square_matrix) <- c(3,3)
square_matrix

cacheSolve(makeCacheMatrix(square_matrix))
