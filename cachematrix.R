#makeCacheMatrix: This function creates a special "matrix" 
#object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
   cmc <- NULL
  
# criar a matriz
  set <- function(y) {
    x <<- y
    mc <<- NULL
  }
  
# obtendo os dados da matriz
  get <- function() x 
  
# invertendo a matriz e armazenado mc
  set_matriz<- function(inverse) mc <<- inverse
  
# obtendo a matriz inversa de mc
  get_matriz_invert <- function() mc
  
# retorno da função
  
  list(set = set, get = get,
       set_matriz = set_matriz,
       get_matriz_invert = get_matriz_invert)
}



#This function computes the inverse of the special "matrix"
#returned by makeCacheMatrix above. If the inverse has already
#been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## obtendo a amtriz inversa que esta armazenada em cache
  c <- x$getInverse()
  
  #retorna a matriz inversa caso exista
  if (!is.null(c)) {
    message("getting cached data")
    
    # mostra a matriz
    return(c)
  }
  
  # criando a matriz caso ela não exista
  matrix <- x$get()
  
  # retornando a matriz inversa
  c <- solve(matrix, ...)
  return(c)
}