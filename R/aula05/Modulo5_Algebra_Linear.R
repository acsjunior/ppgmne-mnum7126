## Tutorial Álgebra Linear: R base, Matrix e RcppArmadillo ---------------------
## Autor: PhD Wagner Hugo Bonat LEG/UFPR ---------------------------------------
## Data: 09/2021 ---------------------------------------------------------------
## MCEO: Métodos Computacionais em Estatística e Otimização --------------------

# Diretório de trabalho
setwd("~/Dropbox/Cursos/MCEO")

## Loading extra packages
library(Rcpp)
library(Matrix)
library(RcppArmadillo)

## Iniciando vetores R básico
x <- c(1,2,3, rep(0, 7))
y <- c(4,5,6, rep(0, 7))

## Convertendo para a classe Matrix (aqui não faz grande diferença)
x_mat <- Matrix(x, sparse = TRUE)
class(x_mat) ## dgCMatrix significa uma matriz esparsa genérica
y_mat <- Matrix(y, sparse = FALSE)
class(y_mat) ## dgeMatrix é uma matriz densa genérica


## Operações com vetores #######################################################

# Multiplicação por escalar ----------------------------------------------------
alpha <- 10
alpha*x # R básico
alpha*x_mat # Mantem a esparcidade
alpha*y_mat

# C++ vetor denso
sourceCpp("C++/escalar_vector.cpp")
escalar_vector(alpha = 10, y = x) ## arma:vec é um vetor denso
escalar_vector(alpha = 10, y = y)

escalar_vector(alpha = 10, y = x_mat) ## Errro

# C++ vetor esparso interface com o pacote Matrix
sourceCpp("C++/escalar_vector_sparse.cpp")
escalar_vector_sparse(alpha = 10, y = x_mat)
escalar_vector_sparse(alpha = 10, y = y_mat) # Pq não funcionou?


## Soma ou subtração -----------------------------------------------------------

# R básico
soma <- x + y
sub <- x - y

# Matrix
soma_esparsa <- x_mat + y_mat
soma_esparsa # Note que ele automaticamente escolheu qual classe manter!!
class(soma_esparsa)

sub_esparsa <- x_mat - y_mat
sub_esparsa

# C++ (dois vetores densos)
sourceCpp("C++/vector_vector.cpp")
vector_vector(x = x, y = y) # saída é densa


# C++ (dois vetores esparsos)
sourceCpp("C++/vector_vector_sparse.cpp")
vector_vector_sparse(x = x_mat, y = x_mat) # saída é esparsa
vector_vector_sparse(x = x_mat, y = y_mat) # Pq não funciona?

## Produto interno t(x)%*y -----------------------------------------------------

# R básico
t(x)%*%y

# Matrix
t(x_mat)%*%y_mat # Note que virou dgeMatrix

# C++ (denso)
sourceCpp("C++/prod_interno.cpp")
produto_interno(x = x, y = y)

# C++ (esparso)
sourceCpp("C++/prod_interno_sparse.cpp")
produto_interno_sparse(x = x_mat, y = y_mat) # pq não funcionou?
y_mat2 <- Matrix(y_mat, sparse = TRUE)
produto_interno_sparse(x = x_mat, y = y_mat2)

## Tamanho do vetor ------------------------------------------------------------

# R básico
sqrt(t(x)%*%x)

# Matrix
sqrt(t(x_mat)%*%x_mat)

# C++  (denso)
sourceCpp("C++/vector_size.cpp")
vector_size(x)

# C++ (esparso)
sourceCpp("C++/vector_size_sparse.cpp")
vector_size_sparse(x_mat)

## Ângulo entre dois vetores ---------------------------------------------------

# R básico
(t(x)%*%y)/(sqrt(t(x)%*%x) * sqrt(t(y)%*%y))

# Matrix
(t(x_mat)%*%y_mat)/(sqrt(t(x_mat)%*%x_mat) * sqrt(t(y_mat)%*%y_mat))

# C++ (dens0)
sourceCpp("C++/angulo_theta.cpp")
angulo_theta(x = x, y = y)

# C++ (esparso)
sourceCpp("C++/angulo_theta_sparse.cpp") ## ATENÇÃO!!
angulo_theta_sparse(x = x_mat, y = y_mat2)

## Matrizes --------------------------------------------------------------------

# Inicializando matrizes 
# Diagonal

# Densa
I <- diag(1, 5, 5) # Diagonal
UM <- matrix(1, 5, 5)
ZERO <- matrix(0, 5, 5)

# Esparsa
I <- Diagonal(5, 1)
UM <- Matrix(1, 5, 5)
ZERO <- Matrix(0, 5, 5)

# Preenchendo algumas matrizes direto no C++
# Mostrar tabela 1: Armadillo: An Open Source C++ Linear Algebra Library for
# Fast Prototyping and Computationally Intensive Experiments

# Exemplo: Criando funções exportando e usando dentro do C++
sourceCpp("C++/init_matrix.cpp")
init_diag(linha = 5, coluna = 5)
init_um(linha = 5, coluna = 5)
init_zero(linha = 5, coluna = 5)
minha_funcao(linha = 5, coluna = 5)

# Criando bloco de matrizes
sourceCpp("C++/block_diag.cpp")
A <- Matrix(c(1,2,3,4), 2, 2, sparse = TRUE)
block_mat(A = A, p = 2, q = 3) # repita a matriz A duas vezes na linha e 3 na coluna

# Multiplicação escalar por matriz ---------------------------------------------
alpha = 10

# Densa
A <- diag(1, 5, 5)
alpha*A

# Esparsa
A_sp <- Diagonal(5, 1)
alpha*A

# C++ (densa)
sourceCpp("C++/escalar_matrix.cpp")
escalar_mat(alpha = 10, A = A)

# C++ (esparsa)
sourceCpp("C++/escalar_matrix_sparse.cpp")
escalar_mat_sparse(alpha = 10, A = A_sp)

## Multiplicação matricial -----------------------------------------------------

# R básico
A <- matrix(c(1,2,3,4), 2, 2)
B <- matrix(c(5,6,7,8), 2, 2)
A%*%B

# Matrix
A_sp <- Matrix(c(1,2,3,4), 2, 2, sparse = TRUE) # Ineficiente!!!
B_sp <- Matrix(c(5,6,7,8), 2, 2, sparse = TRUE)
A_sp%*%B_sp


# C++ densa
sourceCpp("C++/prod_matrix.cpp")
prod_mat(A = A, B = B)

# C++ esparsa
sourceCpp("C++/prod_matrix_sparse.cpp")
prod_mat_sparse(A = A_sp, B = B_sp)

## Solução de sistemas lineares ------------------------------------------------
x1 <- rnorm(100)
x2 <- rbinom(100, size = 1, prob = 0.3)
X <- model.matrix(~ x1 + x2)
beta <- c(2, 5, 2)
mu <- X%*%beta
y <- rnorm(100, mean = mu, sd = 5)

## Sistema de minimos quadrados ordinário --------------------------------------

# R básico
solve(t(X)%*%X, t(X)%*%y)

# Matrix
X_sp <- Matrix(X, sparse = TRUE)
solve(t(X)%*%X, t(X)%*%y)

# C++ (densa)
sourceCpp("C++/linear_system.cpp")
linear_system(A = t(X)%*%X, B = t(X)%*%y)

# C++ (sparse)
Sys.setenv("PKG_LIBS"="-lsuperlu") # Precisa linkar uma biblioteca extra
sourceCpp("C++/linear_system_sparse.cpp") # Importante habilitar a SUPERLU
BB <- as.numeric(t(X)%*%y)
linear_system_sparse(A = t(X_sp)%*%X_sp, B = BB)

## Matriz inversa --------------------------------------------------------------

# Criando uma matriz simétrica positiva definida densa
x1 <- seq(0, 1, l = 30)
x2 <- x1
grid <- expand.grid(x1,x2)
D <- as.matrix(dist(grid, diag = TRUE, upper = TRUE))
M <- exp(-D)

# R básico
inv_M <- solve(M)

# Matrix
M_mat <- Matrix(M)
class(M_mat) # Note que é simétrica
inv_M_mat <- solve(M_mat)

# C++
sourceCpp("C++/inverse.cpp")
inv_M_arma <- inverse(M)

## Benchmark
require(rbenchmark)
benchmark("Basic" = solve(M), "Arma" = inverse(M), "Mat" = solve(M_mat), 
          replications = 10)

## Matriz esparsa
n <- 1000
Z_mat <- bandSparse(n, k = 1, symmetric = TRUE)
Z_mat <- -1*Z_mat
diag(Z_mat) <- 2.1
Z_mat[1,1] <- 1.1
Z_mat[n,n] <- 1.1

# R basico
Z <- as.matrix(Z_mat)
inv_Z <- solve(Z)

# Matrix
class(Z_mat)
inv_Z_mat <- solve(Z_mat)

# C++
# Armadillo não suporta inversa para matrizes esparsas. 
# Ver RcppEigen para uma alternativa.

benchmark("Basic" = solve(Z), "Mat" = solve(Z_mat), 
          replications = 10)
