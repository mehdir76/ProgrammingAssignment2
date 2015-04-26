
## The makeCacheMatrix function create variables, some of which are defined as functions, 
## some as nested functions, and they use the functions to move data,  
## in and out of the cache environment, and perform operations on these data. 
## makeCacheMatrix receives a matrix as a variable, and sets variables and functions in cache.  
 
makeCacheMatrix <- function(x = matrix()) {
         ## Initialize the local m to NULL so we can tell when cacheSolve has run at least once.   
         local_m <- NULL   
         ## Create set function to store the matrix passed in the call as x and NULL as m, both in cache. 
         set <- function(y) {      
             ## Put the initial matrix from the command line into cache as cache_x  
             cache_x <<- y    
             ## Initialize caache_m to NULL so we can tell when cacheSolve has run at least once.           
             cache_m <<- NULL                                      
         } 
         ## Create function to get/return the matrix passed in the command line call to '$set 
           get <- function() cache_x 
         ## Create function to set the value of cache_m in cache to the value of local_m passed in the call to '$set_cache_m. 
           set_cache_m <- function(local_m) cache_m <<- local_m            
         ## Create function to retrieve value of cache_m from cache and return cache_m to the caller so we can check it for NULL 
           get_cache_m <- function() cache_m                       
           list(set = set, get = get,
                set_cache_m = set_cache_m,
                get_cache_m = get_cache_m)
}         

## cacheSolve is a function that receives a a matrix, 
## and then populated with an invertible matrix. 
## cacheSolve returns the inverted matrix submitted in makeCacheMatrix(). 
## When cacheSolve is called, it tries to find the inverted matrix in the cache.  
## If cacheSolve finds a non-NULL value for m existing in cache already, it returns that value.   
## If cacheSolve does not find an existing non-NULL value for m in cache, cacheSolve inverts the matrix  
## and sets the value of m in the cache environment to the just-computed inverted matrix. 
## cacheSolve then returns the ending matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Get the value for m in the cache environment and put it in a local m. 
         local_m<- x$get_cache_m()  
         ## Check to see if m is NULL.
         if(!is.null(local_m)) { 
           ## If m is not NULL, return the value of m with a message.
                 message("Retrieving cached matrix...")   
                   return(local_m) 
         }         ## If we get here, m was NULL 
         ## Calling the function x$get in makeCacheMatrix to obtain the UNinverted matrix , and assign it to first_matrix.
         first_matrix <- x$get()   
         ## Use solve() to invert the startingmatrix, then save it in last_matrix. 
         last_matrix <- solve(first_matrix)  
         ## Call x$set_cache_m() in makeCacheMatrix to set m in the cache environment to the local non-NULL inverted result in endingmatrix 
         x$set_cache_m(last_matrix)   
         ## Return last_matrix if cache_m is not null.  
         last_matrix   
}

##Tests
m <- makeCacheMatrix()
m$set(matrix(c(0,2,2,0),2,2))
m$get()
cacheSolve(m)
cacheSolve(m)


