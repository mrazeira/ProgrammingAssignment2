## 
# This code calculates the inverse of a square matrix and stores the result. 
# If the inverse matrix needs to be recalculated, the result is then 
# retrieved from memory (cache).
#


## This function,  makeCacheMatrix, creates a list containing a list of 
# functions that:
# 1. set the value of the matrix;
# 2. get the value of the matrix;
# 3. set the value of the inverse matrix;
# 4. get the value of the inverse matrix.
#

makeCacheMatrix <- function(x = matrix()) {

#  x must be a square matrix.
#  m_inv is the inverse matrix of x.
        
                m_inv <- NULL
        
        # This function is required only if the square matrix x is reset.
                
                set <- function(y){
                
                x <<- y
                m_inv <- NULL
        }
        
                
        get <- function() x
        
        setsolve <- function(solve) m_inv <<- solve
        
        getsolve <- function() m_inv
        
        
        # creates a list of functions.
        
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
        
}


## 
# This function, cacheSolve, calculates the inverse matrix of the square matrix 
# defined in the above function.
# If the inverse has already been calculated, the result is retrieved from memory.
#

cacheSolve <- function(x, ...) {
        
        m_inv <- x$getsolve()
      
        # If the inverse has already been calculated, do not recalculate. 
        # Recover from memory (cache).
        
        if(!is.null(m_inv)) {
                
                message("getting cached data")
                
                return(m_inv)
        } 
        
        # Calculation of the inverse matrix of x.
        
        data <- x$get()
        
        m_inv <- solve(data)
        
        ## Return a matrix that is the inverse of 'x'
        
        x$setsolve(m_inv)
        
        m_inv

}
