## Script 3: Paralelização em R ------------------------------------------------
## Prof. Wagner Hugo Bonat -----------------------------------------------------
rm(list=ls())

## Instalando pacotes adicionais (rode apenas se não tiver os pacotes instalados)
# install.packages(c("foreach", "parallel", "doParallel"))

# Carregando pacotes adicionais
library(foreach)
library(parallel)
library(doParallel)


# Passo 1: Detectar quantos cores vc tem disponível
detectCores()

# Passo 2: Especificar quantos cores vc quer usar
registerDoParallel(6)

# Passo3: Implemente seu for usando a sintaxe do foreach
loop_output <- foreach(i = 1:9) %dopar% { i^2  }
class(loop_output) # note que sai uma lista
unlist(loop_output) # Isso tem um custo :(

loop_output <- foreach(i = 1:9) %dopar% { c(i, i^2) }
matrix(unlist(loop_output), 9, 2, byrow = TRUE)

## Caso de loop aninhados é mais complicado: Roda o externo em paralelo o interno não!
## Em geral, só vale a pena paralelizar se cada passo do for demorar um pouco pra rodar

# Exemplo: Avaliando o erro tipo I de diferentes alternativas ao teste t
# Retirado de Modern Statistics with R Section 10.2.1
# http://modernstatisticswithr.com

## Testes de permutação
library(MKinfer)

## Versão não paralelizada
simulate_type_I <- function(n1, n2, distr, level = 0.05, B = 999,
                            alternative = "two.sided", ...) {
  # Create a data frame to store the results in:
  p_values <- data.frame(p_t_test = rep(NA, B),
                         p_perm_t_test = rep(NA, B),
                         p_wilcoxon = rep(NA, B))
  
  for(i in 1:B) {
    # Generate data:
    x <- distr(n1, ...)
    y <- distr(n2, ...)
    # Compute p-values:
    p_values[i, 1] <- t.test(x, y,
                             alternative = alternative)$p.value
    p_values[i, 2] <- perm.t.test(x, y,
                                  alternative = alternative,
                                  R = 999)$perm.p.value
    p_values[i, 3] <- wilcox.test(x, y,
                                  alternative = alternative)$p.value
    }
  # Return the type I error rates:
  return(colMeans(p_values < level))
}

## Versão paralelizada
simulate_type_I_parallel <- function(n1, n2, distr, level = 0.05,
                                     B = 999,
                                     alternative = "two.sided", ...) {
  
  results <- foreach(i = 1:B)  %dopar% {
      # Generate data:
      x <- distr(n1, ...)
      y <- distr(n2, ...)
      
      # Compute p-values:
      p_val1 <- t.test(x, y,
                       alternative = alternative)$p.value
      p_val2 <- perm.t.test(x, y,
                            alternative = alternative,
                            R = 999)$perm.p.value
      p_val3 <- wilcox.test(x, y,
                            alternative = alternative)$p.value
      
      # Return vector with p-values:
      c(p_val1, p_val2, p_val3)
    }
  # Each element of the results list is now a vector
  # with three elements.
  # Turn the list into a matrix:
  p_values <- matrix(unlist(results), B, 3, byrow = TRUE)
  # Return the type I error rates:
  return(colMeans(p_values < level))
}

## Comparando o tempo computacional (demora cerca de 17 segundos)
system.time(simulate_type_I(20, 20, rlnorm, B = 999, sdlog = 3))

registerDoParallel(2)
system.time(simulate_type_I_parallel(20, 20, rlnorm, B = 999, sdlog = 3))

registerDoParallel(4)
system.time(simulate_type_I_parallel(20, 20, rlnorm, B = 999, sdlog = 3))

registerDoParallel(6)
system.time(simulate_type_I_parallel(20, 20, rlnorm, B = 999, sdlog = 3))

## Paralelizando functionals ------------------------------------------------------

# Pacote parallel tem versões paralelizadas das funções da familia apply
# Non-parallel version:
system.time(lapply(airquality, function(x) { (x-mean(x))/sd(x) }))

# Parallel version for Linux/Mac (Windows não sei como que faz :( )
system.time(mclapply(airquality, function(x) { (x-mean(x))/sd(x) },
                     mc.cores = 6))

# Pacote furr paraleliza as funções do pacote purr
install.packages(c("future", "furrr"))

library(furrr)
plan(multisession, workers = 6)
library(magrittr)
system.time(airquality %>% future_map(~(.-mean(.))/sd(.))) ## Bem mais lento!

## FIM -------------------------------------------------------------------------

