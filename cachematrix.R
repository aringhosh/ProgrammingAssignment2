# makeVector takes a numeric vector, saved in the private variable x
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


# cachemean takes a caching Vector created with makeVector
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
#mean already exists i.e. cached
    message("we have cached mean")
    return(m)
  }
  
  #mean is not cached
  data <- x$get()
  m <- mean(data, ...) #compute mean
  x$setmean(m)
  m #return m
}

## sample output

#> x <- makeVector()
#> x$set(matrix(1:8,4,2))
#> cachemean(x)
#[1] 4.5
#> cachemean(x)
#we have cached mean
#[1] 4.5
