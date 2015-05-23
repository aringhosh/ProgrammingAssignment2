makecache_matrixMatrix <- function(mat_x = matrix()) 
{
        cached_matrix <- NULL
        set <- function(mat_) 
        {
                mat_x <<- mat_
                cached_matrix <<- NULL
        }
        get <- function() mat_x
        set_ <- function(inv) cached_matrix <<- inv
        inv_ <- function() cached_matrix
        list(set = set, get = get, setMatrix = set_, getInverse = inv_)
}


cache_matrixSolve <- function(mat_x, ...) 
{
        cached_matrix <- mat_x$getInverse()
        if (!is.null(cached_matrix)) 
        {
                return(cached_matrix)
        }
        else{
            matrix <- mat_x$get()
            tryCatch( 
            {
                cached_matrix <- solve(matrix, ...)
            },
            error = function(e_) 
            {
                message(c("Something is wrong here... here is the clue ",e_))
                return(NA)
            },
            warning = function(w_) 
            {
                message(c("are you sure???? ",w_))
                return(NA)
            },
            finally = 
            {
                mat_x$setMatrix(cached_matrix)
            } )
            return (cached_matrix)
        } 
}
