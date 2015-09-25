## set changes matrix x stored in makeCacheMatrix
## get returns matrix x stored in makeCacheMatrix 
## getinvx is a function that returns the inverse matrix
## setinvx is the function that changes the stored inverse matrix invx

## makeCacheMatrix is the main function that stores matrix x and its
## inverse matirx invx.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
            
        set <- function(y){
            
            x <<- y
         invx <<- NULL
          }
        
        get <- function() x
        # poo <- solve(x)
        setinvx <- function(poo)  invx <<- poo
        getinvx <- function() invx 

        list(set = set, get = get, 
             setinvx = setinvx,
             getinvx = getinvx)

}


## cacheSolve finds inverse of x ie invx if it is not calculated already

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		 invx <- x$getinvx()
           if (!is.null(invx)){
               message("getting cached inverse matrix of x")
                return(invx)
            }
            matx <- x$get()
            invx <- solve(matx, b, ...) # b is the Identity matrix augmented with matx 
			                            # so that solve will return inverse of matx
            x$setinvx(invx)
            invx

}
