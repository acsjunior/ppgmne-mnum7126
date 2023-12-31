## Script 2: Tutorial do Rcpp para iniciantes ----------------------------------
## Prof. Wagner Hugo Bonat -----------------------------------------------------

## Instalando o pacote
install.packages("Rcpp")

## Carregando o pacote
library(Rcpp)

## Função trivial em C++ para somar três números
my_fn <- 
'int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}'

## Chamando a função em R e deixando disponível no globalEnv
cppFunction(my_fn)

## O que é que foi criado?
class(add)
add

## Usando a função
add(x = 1, y = 2, z = 4)

## Transformar funções R em C++ temos que estar atentos ao tipo da entrada e
# da saída: Algumas possibilidades

# Scalar input and scalar output
# Vector input and scalar output
# Vector input and vector output

# Função sem argumento e retorna sempre um número ------------------------------

# R note que retorna um integer
one <- function() 1L

# C++
ex1 <- 
'int one() {
     return 1;
}'
# Copilando
cppFunction(ex1)
one()

# Comentários: 
# Sintaxe é parecida; precisa declarar tipo de entrada e saída.
# Para escalares: double, int, String, and bool.
# Para vetores: NumericVector, IntegerVector, CharacterVector, and LogicalVector.
# return é obrigatório.
# Cada linha termina com ';'.

# Função recebe escalar e retorna escalar --------------------------------------

# R - retorna  sinal de um número
signR <- function(x) {
  if (x > 0) {
    1
  } else if (x == 0) {
    0
  } else {
    -1
  }
}

# C++
cppFunction('int signC(int x) {
  if (x > 0) {
    return 1;
  } else if (x == 0) {
    return 0;
  } else {
    return -1;
  }
}')

## Função recebe um vetor e retorna um escalar ---------------------------------

## Soma em R
sumR <- function(x) {
  total <- 0
  for (i in seq_along(x)) {
    total <- total + x[i]
  }
  total
}

## Soma em C++
cppFunction('double sumC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}')

sumR(seq(1:100))
sumC(seq(1:100))

## Comentários
# Em C++ o index começa em ZERO!
# Substituição in place -> total += x[i] é equivalente a total = total + x[i].
# modify in place perators -=, *=, and /=.

x <- runif(1e3)
bench::mark(
  sum(x),
  sumC(x),
  sumR(x)
)[1:6]

## Função vetor de entrada e vetor de saída ------------------------------------

# Distância Euclidiana entre um escalar e um vetor

# R
pdistR <- function(x, ys) {
  sqrt((x - ys) ^ 2)
}

pdistR2 <- function(x, ys) {
  out <- c()
  for(i in 1:length(ys)) {
    out[i] = sqrt( (ys[i] -x)^2)
  }
  return(out)
}

# C++
cppFunction('NumericVector pdistC(double x, NumericVector ys) {
  int n = ys.size();
  NumericVector out(n);

  for(int i = 0; i < n; ++i) {
    out[i] = sqrt(pow(ys[i] - x, 2.0));
  }
  return out;
}')

pdistR(x = c(2), ys = seq(0, 1, l = 5))
pdistR2(x = c(2), ys = seq(0, 1, l = 5))
pdistC(x = c(2), ys = seq(0, 1, l = 5))

# Comparando o tempo
y <- runif(1e6)
bench::mark(
  pdistR(0.5, y),
  pdistR2(0.5, y),
  pdistC(0.5, y)
)[1:6]

## Note que a versão R é full vetorizada, por isso rápida!

## Vetor de entrada e escalar na saída: Exemplo mais realista
# Arquivo media.cpp

# Copila e carrega a função escrita em C++
sourceCpp("R/aula04/C++/media.cpp")
meanC(x = rnorm(1000))

## Comparação
x <- runif(1e5)
bench::mark(
  mean(x),
  meanC(x)
)

## Objetos especiais: Listas e data.frames

# Acessando objetos em uma lista
sourceCpp("R/aula04/C++/acess_list.cpp")
#undebug(lm)
mod <- lm(mpg ~ wt, data = mtcars)
mpe(mod)

# Passando uma função para o C++
sourceCpp("R/aula04/C++/funcao.cpp")
callWithOne(function(x) dnorm(x, 0, 1), n = 1)

## STL Structures --------------------------------------------------------------

# The standard template library (STL) provides a set of extremely useful data 
# structures and algorithms.

## Exemplo 1: Reescrevendo a função soma
x <- rnorm(100000)
sourceCpp("R/aula04/C++/soma_stl.cpp")
sum3(x)

## Exemplo 2: Usando uma versão mais simples
sourceCpp("R/aula04/C++/soma_stl_simples.cpp")
sum4(x)

## Exemplo 3: Usando alguns features do STL (accumulator)
sourceCpp("R/aula04/C++/soma_accumulator.cpp")
sum5(x)

## Comparando as implementações da soma
x <- runif(1e3)
bench::mark(
  sum(x),
  sumC(x),
  sum3(x),
  sum4(x),
  sum5(x),
  sumR(x)
)[1:6]


## Estudo de caso 1: Amostrador de Gibbs ---------------------------------------

## Mostrar slides

# R
gibbs_r <- function(N, thin) {
  mat <- matrix(nrow = N, ncol = 2)
  x <- y <- 0
  for (i in 1:N) {
    for (j in 1:thin) {
      x <- rgamma(1, 3, y * y + 4)
      y <- rnorm(1, 1 / (x + 1), 
                 1 / sqrt(2 * (x + 1)))
    }
    mat[i, ] <- c(x, y)
  }
  mat
}

# C++
sourceCpp("R/aula04/C++/gibbs.cpp")
bench::mark(
  gibbs_r(100, 10),
  gibbs_cpp(100, 10),
  check = FALSE
)

## Estudo de caso 2: Gerador de números aleatórios da GammaCount
## Agradecimento: Eduardo Ribeiro e Walmes M. Zeviani
sourceCpp("R/aula04/C++/rgammacount.cpp")
replicate(100, .rgcnt_one(lambda = 10, alpha = 1))

## FIM -------------------------------------------------------------------------