## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Variable para almacenar la inversa en caché
  
  # Establecer una nueva matriz
  set <- function(y) {
    x <<- y      # Asignación en el entorno padre
    inv <<- NULL # Resetear la inversa en caché porque la matriz cambió
  }
  
  # Obtener la matriz actual
  get <- function() x
  
  # Establecer la inversa en caché
  setInverse <- function(inverse) inv <<- inverse
  
  # Obtener la inversa de la caché
  getInverse <- function() inv
  
  # Devolver una lista con todas las funciones para operar sobre la matriz
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Intentar obtener la inversa de la caché
  
  # Si la inversa ya está en caché, la devuelve
  if (!is.null(inv)) {
    message("Obteniendo la inversa desde la caché")
    return(inv)
  }
  
  # Si no está en caché, calcula la inversa
  mat <- x$get()
  inv <- solve(mat, ...)  # Calcular la inversa con 'solve'
  x$setInverse(inv)       # Almacenar la inversa en caché
  
  inv  # Devolver la inversa
}