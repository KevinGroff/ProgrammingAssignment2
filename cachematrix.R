##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
##rather than compute it repeatedly. These functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  
  ## Replace active matrix with new matrix
  SetActiveMatrix <- function(y)
  {
    ## Set new matrix as active matrix
    x <<- y
    
    ## Since active matrix is new, cached value is no longer valid to must be SetActiveMatrix to null
    m <<- NULL
  }
  
  ## Return value of active matrix
  GetActiveMatrix <- function()
  {
    x
  }
  
  ## Save value of inverse matrix to cache
  SaveCacheMatrix<- function(solve)
  {
    m <<- solve(x)
  }
  
  ## Return value of cached matrix
  ReadCacheMatrix <- function()
  {
    m
  }
  
  
  list	(SetActiveMatrix = SetActiveMatrix,
        GetActiveMatrix = GetActiveMatrix,
        SaveCacheMatrix = SaveCacheMatrix,
        ReadCacheMatrix = ReadCacheMatrix
  )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  
  ## SetActiveMatrix m to the current stored matrix
  m <- x$ReadCacheMatrix()
  
  print(m)
  ## Check if m has a cached matrix.  If yes, then returned the cached value.
  if(!is.null(m)) 
  {
    # Return value of cached matrix and notify with message
    message("getting cached matrix")
    return(m)
  }
  
  ## If matrix is NULL, then the inverse must be calculated.
  m <- solve(x$GetActiveMatrix())
  
  ## Cache the inversed matix
  x$SaveCacheMatrix(m)
  
  ## Return the inverse matrix
  m
}  

