## Two functions that create a matrix then cache its inverse to save on repetitive inverse computations


## Part 1: makeCacheMatrix: creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        #sets the cached inverse named cachedI to NULL so it will be calculated once
        cachedI <- NULL
        
        #get the value of the matrix; this is added for completeness of the getter and setter methods
        get <- function() {   
                x  
        }
        
        
        set <- function(aMatrix){
                #set the value of the matrix to the value passed in
                x <<- aMatrix
                
                #setting m should reset cachedI to NULL so that cachedI will be recomputed if needed
                cachedI <<- NULL
        }
        
        #return cached inverse
        getInverse <- function() cachedI
        
        #set inverse and cache it
        setInverse <- function(solve) cachedI <<- solve
        
        #returns a list of the functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)    

}


## Part 2: cacheSolve: computes the inverse of the matrix above if not set, otherwise retrieves cached value

cacheSolve <- function(x, ...) {
        # Retrieve the most recent value for the inverse
        inverse <- x$getInverse()
        
        
        #check if the inverse is not NULL
        if(!is.null(inverse)){
                #the object is not NULL, return Inverse
                message("getting cached data")
                return(inverse)      
        }
        #Inverse is not cached so calculate Inverse
        message("calculating new inverse")
        
        #define solve
        inverse <- solve(x$get(), ...)
        
        # Sets inverse to the newly calculated value  
        x$setInverse(inverse)
        
        #Returns the new Inverse value
        inverse
}
