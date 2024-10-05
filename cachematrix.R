## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# 创建一个特殊的“矩阵”对象，它可以缓存其逆
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # 用来存储逆矩阵的变量
  
  # 设置矩阵的函数
  set <- function(y) {
    x <<- y
    inv <<- NULL  # 当矩阵改变时，重置逆矩阵缓存
  }
  
  # 获取矩阵的函数
  get <- function() x
  
  # 设置逆矩阵的函数
  setInverse <- function(inverse) inv <<- inverse
  
  # 获取逆矩阵的函数
  getInverse <- function() inv
  
  # 返回包含以上四个方法的列表
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# 计算矩阵的逆，如果已经缓存过则直接返回缓存结果
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # 如果已经缓存了逆矩阵，直接返回
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # 如果没有缓存，则计算逆矩阵并缓存
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  inv
}
