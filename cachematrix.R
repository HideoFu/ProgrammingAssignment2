##  To refer cache, some "global" variable is require.
##  Such global variable: array "A" is a special "matrix". 
##  Its first plane is original matrix, and second plane is inversed matrix.

##  To make the array, dimensions of the original matrix is obtained.
##  Then global variable A is set with NULL second plane.

makeCacheMatrix <- function(x = matrix()){

    rows <- nrow(x)
    cols <- ncol(x)
    
    A <<- array(,dim=c(rows,cols,2))
    A[,,1]<<-x
    A[,,2]<<-NULL
}


##  Call global variable A, and check if the second plane has values.
##  If the second plane includes NA, then the value will be calculated.

cacheSolve <- function(x, ...) {
        
    iv <- A[,,2]
    
    if(!anyNA(iv)){
        message("getting cached value")
        return(iv)
    }
    
    iv <- solve(x, ...)
    A[,,2]<<-iv
    iv
}
