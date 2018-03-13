## Kevin Lemieux solution
## Explain "makeCacheMatrix" 
## We need to make a cache matrix 
## 1) Initialize our cache matrix as "cacheMatrix" and assign it as NULL
## 2) Next we make the method "setMatrix" - in here with the <<- operator we 
## set y to the value of x in the parent environment
## 3) set value of cacheMatrix in parent environment to NULL as well
## 4) Next we make the method "getMatrix" - returning the value of x
## set y to the value of x in the parent environment
## 5) Next we make the method "setCache" - returning the value inverse to cacheMatrix
## 6) Next we make the method "getCache" - returning the inversed cache of of x
## 7) Finally we need to return all the methods needed: setMatrix, getMatrix, setCache = setCache,getCache

makeCacheMatrix <- function(x = matrix()) {

  cacheMatrix <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  
  getMatrix <- function() x
  
  setCache <- function(inverse) cacheMatrix <<- inverse
  
  getCache <- function() cacheMatrix
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)
  
}
  

## Explain "cacheSolve" which returns the inverse of a matrix using cache
## 1) Check content of cache
## 2) If not NULL result returned
## 3) If empty, we need to get the matrix, create it, set it, update and then return it
## 4) Next we make the method "getMatrix" - returning the value of x
## set y to the value of x in the parent environment
## 5) Next we make the method "setCache" - returning the value inverse to cacheMatrix
## 6) Next we make the method "getCache" - returning the inversed cache of of x
## 7) Finall

cacheSolve <- function(x, ...) {
  
  cacheMatrix <- x$getCache()
  
  if (!is.null(cacheMatrix)) {
    message("Please wait ... loading cache ...")
    return(cacheMatrix)
  }

  else {
    dMatrix <- x$getMatrix()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
  }
}

