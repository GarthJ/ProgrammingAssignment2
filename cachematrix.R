# last edited: 2018/ may/ 27

# makeCacheMatrix() receives a matrix as input and, when assigned, creates an obj with 4 methods
# (4 returned functions)
# set_matrix(): allows the matrix to be re-set/ changed
# get_matrix(): gets (returns) the current matrix
# set_inverse(): allows the inverse to be specified manually
# get_inverse(): returns the cached inverted matrix

makeCacheMatrix <- function(inp_matrix = matrix()) {
        return_inverse <- NULL
        
        # set_matrix()
        # if a new matrix is input, assign the new matrix to the parent env var, inp_matrix, and
        # NULL the val of return_version (clear the cache)
        set_matrix <- function(new_matrix) {
                inp_matrix <<- new_matrix
                return_inverse <<- NULL
        }
        
        # get_matrix()
        # return the current matrix obj, if the method get_matrix is called:
        get_matrix <- function() inp_matrix
        
        # set_inverse()
        # manually override the value of the inverse, when calling the $set_inverse() method:
        set_inverse <- function(matrix_inverse) return_inverse <<- matrix_inverse
        
        # get_inverse():
        # return the value of the last computed inverse (the cached inverse):
        get_inverse <- function() return_inverse
        
        # upon function completion, return list of functions associated with obj (method list):   
        return(list(
             set_matrix = set_matrix,   # $set_matrix()
             get_matrix = get_matrix,   # $get_matrix()
             set_inverse = set_inverse, # $set_inverse()
             get_inverse = get_inverse  # $get_inverse()
                )
        )
}


# cacheSolve() receives as input the object created by makeCacheMatrix()
# it then tests, using $get_inverse(), if a cached val for the current matrix exists
# if the inverse of the matrix exists in cache, the inverse is returned, ELSE
# the inverse is calculated for the matrix

cacheSolve <- function(cache_matrix, ...) {
        # assumption: arg passed into cacheSolve is a square-form matrix
        return_inverse <- cache_matrix$get_inverse()
        # test if inverse has been calculated for obj as currently set:
        # logical path, if inverse has been calculated and cached:
        if(!is.null(return_inverse)) {
                message("retrieving cached inverse...")
                return(return_inverse)  
        }
        # logical path, if inverse has not been calculated:
        # retrieve the uncomputed matrix, using the get method:
        data_new <- cache_matrix$get_matrix()
        # compute the inverse for the new data:
        # assumption: input matrix is invertible
        return_inverse <- solve(data_new, ...)
        # set the cached value to the newly-computed inverse:
        cache_matrix$set_inverse(return_inverse)
        # return desired output: the inverse of x:
        return_inverse
        
        # note: test for successful matrix inversion -- after executing cacheSolve(makeCacheMatrix obj):
        # round(cache_obj$get_inverse() %*% sample_matrix, 1)
        # value of diagonals should equal 1; all other vals 0
}
