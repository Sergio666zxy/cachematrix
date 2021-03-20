##create a matrix that is able to inverse
makeCacheMatrix <- function(x = matrix){
    inverse_matrix <- NULL                   #set it to null
    set <- function(y){ 
        x <<- y
        inverse_matrix <<- NULL
    }
    get <- function() x
    set_Inverse <- function(inverse) inverse_matrix <<- inverse
    get_Inverse <- function() inverse_matrix
    list(set = get, get = get,
    set_Inverse = set_Inverse,
    get_Inverse = get_Inverse
    )
}  
    
    
CacheSolve <- function(x, ...){
    inverse_matrix <- x$get_Inverse()
    if(!is.null(inverse_matrix)){
        message("the result is: ")
        return(inverse_matrix)
    }
    mat <- x$get()
    inverse_matrix <- solve(mat, ...)
    x$set_Inverse(inverse_matrix)
    inverse_matrix
}


#here is an example you may check
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
my_matrix$get_Inverse()
#NULL
CacheSolve(my_matrix)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5