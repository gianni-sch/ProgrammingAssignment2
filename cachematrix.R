# by Gianfranco Schilirò (21.08.2015)

#  makeCacheMatrix(square_matrix) -> special_vector, function create special vector, 
#  which is really  a list, containing several functions to:
#               - set the value of matrix elements
#               - get the values of matrix elements
#               - set the value of the inverse matrix
#               - get the value of the inverse matrix
# Example:
# m <- c(1,3,2,1,4,5,2,3,2)
# a<- matrix(m,3,3)        #==== create square matrix a, 3x3 elements
# b<- makeCacheMatrix(a)   #==== store a in special vector b
                           #==== inv in b is NULL!!!  

makeCacheMatrix <- function(x = matrix()) {
# control if the input matrix is square
  if(dim(x)[1]!=dim(x)[2]){
    message("Error: Square Matrix required!!!")
    return
  }
# check whether there are NA elements in the input array 
  if(is.na(sum(x))){
    message("Error: NA value in Matrix!!!")
    return
  }
# initialization 
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
# 
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#  cacheSolve(special_vector) -> inv_matrix, function calulates the inverse of matrix x 
#  created with makeCacheMatrix(x) and stores it in special_vector via setinverse. 
#  If the inverse matrix of x has already been calculated and the matrix x has not been 
#  changed, cacheSolve returns the value previously cached without recalculating the 
#  inverse.
#  Example:
# c <- cacheSolve(b)     # calculate inverse of matrix a previously stored in b.       
                         # store the result in c and in the variable inv of b.
                         # the variable inv in b now contains the inverse calulated,
# if recall the cacheSolve(b) function:
# cacheSolve(b)            # this visualize the value previously stored in inv of b.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
