
n <- as.numeric( commandArgs( trailingOnly = TRUE ) )

t1 <- proc.time()
M <- matrix( rnorm(n*n), n, n )
t2 <- proc.time()
B <- M %*% t(M)
t3 <- proc.time()
C <- chol(B)
t4 <- proc.time()
D <- t(C)
t5 <- proc.time()

print( t2 - t1 )
print( t3 - t2 )
print( t4 - t3 )
print( t5 - t4 )
