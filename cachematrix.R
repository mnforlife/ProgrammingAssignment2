## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) { ##function creates a matrix
  m <- NULL 
  set <- function(y) { ##set is the funciton y
    x <<- y      ## y is X and is being stored outside of this environment
    m <<- NULL   ##m is still null outside of this environment
  }
  get <- function() x
  setinv <- function(solve) m <<- solve ## setinv is the function solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) ## here we've created a list which is returned when makeCachematrix is called
}

cacheSolve <- function(x, ...) { ##cacheSolve is the funciton x
  i <- x$getinv() ## Return a matrix that is the inverse of 'x'

  if(!is.null(i)) { ##if i is null, return i, if it is not null, print the cached data
    print("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...) ##i is calling the function solve and applying it to data which is getting the inverse
  x$setinv(i)
  return(i)
}





