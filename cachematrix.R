 
1 # makeCacheMatrix is a function that returns a list of functions 
2 # Its puspose is to store a martix and a cached value of the inverse of the  
3 # matrix. Contains the following functions: 
4 # * setMatrix      set the value of a matrix 
5 # * getMatrix      get the value of a matrix 
6 # * cacheInverse   get the cahced value (inverse of the matrix) 
7 # * getInverse     get the cahced value (inverse of the matrix) 
8 # 
9 
10 # makeCacheMatrix function 
11 makeCacheMatrix <- function(x = numeric()) { 
12          
13         # holds the cached value or NULL if nothing is cached 
14         # initially nothing is cached so set it to NULL 
15         cache <- NULL 
16          
17         # store a matrix 
18         setMatrix <- function(newValue) { 
19                 x <<- newValue 
20                 # since the matrix is assigned a new value, flush the cache 
21                 cache <<- NULL 
22         } 
23 
 
24         # returns the stored matrix 
25         getMatrix <- function() { 
26                 x 
27         } 
28 
 
29         # cache the given argument  
30         cacheInverse <- function(solve) { 
31                 cache <<- solve 
32         } 
33 
 
34         # get the cached value 
35         getInverse <- function() { 
36                cache 
37         } 
38          
39         # return a list. Each named element of the list is a function 
40         list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse) 
41 } 
42 
 
 
 
43 # The following function calculates the inverse of a "special" matrix created with  
44 # makeCacheMatrix 
45 cacheSolve <- function(y, ...) { 
46         # get the cached value 
47         inverse <- y$getInverse() 
48         # if a cached value exists return it 
49         if(!is.null(inverse)) { 
50                 message("getting cached data") 
51                 return(inverse) 
52         } 
53         # otherwise get the matrix, caclulate the inverse and store it in 
54         # the cache 
55         data <- y$getMatrix() 
56         inverse <- solve(data) 
57         y$cacheInverse(inverse) 
58          
59         # return the inverse 
60         inverse 
61 } 
