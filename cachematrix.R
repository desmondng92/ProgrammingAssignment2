# Matrix inversion is a computation that consumes a lot of time. Rather than
# compute it repeatedly, the functions written below is used to cache 
# the inverse of a matrix. 

#The function, makeCacheMatrix creates a special "matrix", 
# and consist of functions that used to
#Step 1. set the value of the matrix
#Step 2. get the value of the matrix
#Step 3. set the value of the inverse
#Step 4. get the value of the inverse

# Create a special matrix
makeCacheMatrix <- function(x = matrix()) {
# To store cached inverse matrix
i <- NULL
# Step 1. set the value of the matrix
set <- function(y) {
x <<- y
i <<- NULL
}
# Step 2. get the value of the matrix
get <- function() x
# Step 3. set the value of the inverse
setinv <- function(inverse) i <<- inverse
# Step 4. get the value of the inverse
getinv <- function() i
# Return the matrix with newly defined function
list(set=set, get=get, setinv=setinv, getinv=getinv)
}


#cacheSolve is a function that used to compute the inverse of the matrix. 
#However, if the inverse is calcuted previously, the function will return 
#the cached inverse.

cacheSolve <- function(x, ...) {
i <- x$getinv()
#Case I: If the inverse is already calculated, return the cached inverse.
if(!is.null(i)) {
message("getting cached data.")
return(i)
}
#Case II: If the inverse is not calculated, calculate the inverse of matrix.
data <- x$get()
i <- solve(data)
#Cache the inverse
x$setinv(i)
# Return the inverse
i
}

# Example:
# > source ("cachematrix.R")                  # Source the script file
# > x <- matrix((1:4),2,2)                    # Create matrix, x
# > m <- makeCacheMatrix(x)                   # Create special matrix, m
# > m$get()                                   # Return the matrix m
# > cacheSolve(m)                             # Return the inverse
# > cacheSolve(m)                             # Call the 2nd time, return the cached inverse
                                             


