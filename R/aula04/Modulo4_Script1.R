## Script 1: Recursos para programação eficiente em R --------------------------
## Autor: Prof. Wagner Hugo Bonat ----------------------------------------------

## Fixando o diretório de trabalho (veja o seu local!!)

## Localizando onde um erro ocorreu -------------------------------------------

# Exemplo 1: Retirado do livro Advanced R (Capítulo 22)

## Função a ser avaliada
f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) {
  if (!is.numeric(d)) {
    stop("`d` must be numeric", call. = FALSE)
  }
  d + 10
}

# Avaliando em um ponto
f("a")
# RStudio -> Clique Show Traceback

# De forma geral
f("a")
traceback()

# Aumentando ainda mais o número de chamadas de funções
j <- function() k()
k <- function() stop("Oops!", call. = FALSE)
f(j())

# Traceback
traceback()

##---------------------------------------------------------------------------

# Debug interativo
f("a")
# Clique: Rerun with Debug

# Exemplo 2: Pausa preparada
g <- function(b) {
  browser()
  h(b)
}
f(10)

# Exemplo 3: Pausa condicional
g <- function(b) {
  if (b < 0) {
    browser()
  }
  h(b)
}
g(-1)
g(10)

# Debug direto (rodar a função passo-a-passo)
x <- rnorm(10)
y <- rnorm(10)
debug(lm)
lm(y ~ x)
undebug(lm)

# Outra opção debugonce()

## Volta slides

## Profilling --------------------------------------------------------------

# Exemplo 1: Prof_Example.R 
# Foco no tempo computacional
require(profvis)
source("R/aula04/Prof_example.R")
profvis(f())

# Exemplo 2: Memoria.R
# Foco no uso de memória
# Usar o Menu do RStudio
x <- integer()
for (i in 1:1e4) {
  x <- c(x, i)
}

# Exemplo 3: Comparando tempo computacional
library(bench)
library(ggbeeswarm)

x <- runif(100)
(lb <- bench::mark(
  sqrt(x),
  x ^ 0.5
))

# Visualizando
plot(lb)

# 1 ms, then one thousand calls take a second.
# 1 µs, then one million calls take a second.
# 1 ns, then one billion calls take a second.

## Volta slides

## Melhorando a performance ----------------------------------------------------

# Exemplo 1: Estratégias para calcular a média
mean1 <- function(x) mean(x)
mean2 <- function(x) sum(x) / length(x)
x <- runif(1e5)
bench::mark(
  mean1(x),
  mean2(x)
)[c("expression", "min", "median", "itr/sec", "n_gc")]

## Exemplo 2: Crescendo um objeto
random_string <- function() {
  paste(sample(letters, 50, replace = TRUE), collapse = "")
}
strings10 <- replicate(10, random_string())
strings100 <- replicate(100, random_string())

collapse <- function(xs) {
  out <- ""
  for (x in xs) {
    out <- paste0(out, x)
  }
  out
}

bench::mark(
  loop10  = collapse(strings10),
  loop100 = collapse(strings100),
  vec10   = paste(strings10, collapse = ""),
  vec100  = paste(strings100, collapse = ""),
  check = FALSE
)[c("expression", "min", "median", "itr/sec", "n_gc")]

## Exemplo 3: Teste t

# Passo 1: Gerar as bases de dados
m <- 1000
n <- 50
X <- matrix(rnorm(m * n, mean = 10, sd = 3), nrow = m)
grp <- rep(1:2, each = n / 2)
X[1:5, 1:15]

# Passo 2: função t.test usando formula ou passando vetores.

# Estratégia usando formula
system.time(
  for (i in 1:m) {
    t.test(X[i, ] ~ grp)$statistic
  }
)

## Passando vetores
system.time(
  for (i in 1:m) {
    t.test(X[i, grp == 1], X[i, grp == 2])$statistic
  }
)

## Passo 3: Precisamos guardar as saídas (gasta tempo)

# Estratégia 1: cria um vetor e guarda a saída (praticamente o mesmo tempo)
out <- c()
system.time(
  for (i in 1:m) {
    out[i] <- t.test(X[i, grp == 1], X[i, grp == 2])$statistic
  }
)

## Estratégia 2: Programação funcional usando o pacote purr
compT <- function(i){
  t.test(X[i, grp == 1], X[i, grp == 2])$statistic
}
system.time(t1 <- purrr::map_dbl(1:m, compT)) # Pouco mais eficiente. Vale a pena?

# Overhead: dependencia extra do pacote purr. Dentro de um pacote pode ser ruim!
# Quebra de código se o pacote mudar ou se o colega não tiver o purr.

## Estratégia do preguiçoso: Faça só o necessário!!
my_t <- function(x, grp) {
  t_stat <- function(x) {
    m <- mean(x)
    n <- length(x)
    var <- sum((x - m) ^ 2) / (n - 1)
    list(m = m, n = n, var = var)
  }
  
  g1 <- t_stat(x[grp == 1])
  g2 <- t_stat(x[grp == 2])
  se_total <- sqrt(g1$var / g1$n + g2$var / g2$n)
  (g1$m - g2$m) / se_total
}

# Isso foi uma melhora considerável!
system.time(t2 <- purrr::map_dbl(1:m, ~ my_t(X[.,], grp))) 

## Ainda melhor se vetorizar
rowtstat <- function(X, grp){
  t_stat <- function(X) {
    m <- rowMeans(X)
    n <- ncol(X)
    var <- rowSums((X - m) ^ 2) / (n - 1)
    
    list(m = m, n = n, var = var)
  }
  
  g1 <- t_stat(X[, grp == 1])
  g2 <- t_stat(X[, grp == 2])
  
  se_total <- sqrt(g1$var / g1$n + g2$var / g2$n)
  (g1$m - g2$m) / se_total
}

system.time(t3 <- rowtstat(X, grp))

## FIM ------------------------------------------------------------------------