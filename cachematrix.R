
## The makeCacheMatrix function create variables, some of which are defined as functions, 
## some as nested functions, and they use the functions to move data,  
## in and out of the cache environment.

makeCacheMatrix <- function(x = matrix()) {
         ## Initialize the local_mtr to NULL so we can tell when cacheSolve has run at least once.   
         local_mtr <- NULL   
         ## Create set function to store the matrix passed in the call as x and NULL as mtr, both in cache. 
         set <- function(y) {      
             ## Put the initial matrix from the command line into cache as cache_x  
             cache_x <<- y    
             ## Initialize cache_mtr to NULL so we can tell when cacheSolve has run at least once.           
             cache_mtr <<- NULL                                      
         } 
         ## Create function to get/return the matrix passed in the command line call to '$set 
           get <- function() cache_x 
         ## Create function to set the value of cache_mtr in cache to the value of local_mtr passed in the call to '$set_cache_mtr. 
           set_cache_mtr <- function(local_mtr) cache_mtr <<- local_mtr            
         ## Create function to retrieve value of cache_mtr from cache and return cache_mtr to the caller so we can check it for NULL 
           get_cache_mtr <- function() cache_mtr                       
           list(set = set, get = get,
                set_cache_mtr = set_cache_mtr,
                get_cache_mtr = get_cache_mtr)
}         

## cacheSolve returns the inverted matrix submitted in makeCacheMatrix(). 
## When cacheSolve is called, it tries to find the inverted matrix in the cache.  
## If cacheSolve finds a non-NULL value for m existing in cache already, it returns that value.   
## If cacheSolve does not find an existing non-NULL value for mtr in cache, cacheSolve inverts the matrix  
## and sets the value of mtr in the cache environment to the just-computed inverted matrix. 
## cacheSolve then returns the last_matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Get the value for mtr in the cache environment and put it in a local_mtr. 
         local_mtr<- x$get_cache_mtr()  
         ## Check to see if mtr is NULL.
         if(!is.null(local_mtr)) { 
           ## If mtr is not NULL, return the value of mtr with a message.
                 message("Retrieving cached matrix...")   
                   return(local_mtr) 
         }         ## If we get here, mtr was NULL 
         ## Calling the function x$get in makeCacheMatrix to obtain the UNinverted matrix , and assign it to first_matrix.
         first_matrix <- x$get()   
         ## Use solve() to invert the first_matrix, then save it in last_matrix. 
         last_matrix <- solve(first_matrix)  
         ## Call x$set_cache_m() in makeCacheMatrix to set mtr in the cache environment to the local non-NULL inverted result in last_matrix 
         x$set_cache_mtr(last_matrix)   
         ## Return last_matrix if cache_mtr is not null.  
         last_matrix   
}

##Tests
mtr <- makeCacheMatrix()
mtr$set(matrix(c(0,2,2,0),2,2))
mtr$get()
cacheSolve(mtr)
cacheSolve(mtr)


