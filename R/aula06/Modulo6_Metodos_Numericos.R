## Métodos numéricos -----------------------------------------------------------
## Autor: Prof. Wagner Hugo Bonat ----------------------------------------------

## Método da bisseção
bissecao <- function(fx, a, b, tol = 1e-04, max_iter = 100) {
  fa <- fx(a);  fb <- fx(b);  if(fa*fb > 0) stop("Solução não está no intervalo")
  solucao <- c(); sol <- (a + b)/2; solucao[1] <- sol; 
  limites <- matrix(NA, ncol = 2, nrow = max_iter)
  for(i in 1:max_iter) {
    test <- fx(a)*fx(sol)
    if(test < 0) { 
      solucao[i+1] <- (a + sol)/2 
      b = sol }
    if(test > 0) { 
      solucao[i+1] <- (b + sol)/2 
      a = sol }
    if( abs( (b-a)/2) < tol) break
    sol = solucao[i+1]
    limites[i,] <- c(a,b) }
  out <- list("Tentativas" = solucao, "Limites" = limites, "Raiz" = solucao[i+1])
  return(out)
}


## Exemplo
ftheta <- function(theta){ ## Implementando a função
  dd <- 2*length(y)*(log(theta.hat/theta) + mean(y)*(theta - theta.hat))
  return(dd - 3.84)
} 

set.seed(123) ## Resolvendo numericamente
y <- rexp(20, rate = 1)
theta.hat <- 1/mean(y)
Ic_min <- bissecao(fx = ftheta, a = 0, b = theta.hat)
Ic_max <- bissecao(fx = ftheta, a = theta.hat, b = 3)
c(Ic_min$Raiz, Ic_max$Raiz) ## Solução aproximada


## Método de Newton
newton <- function(fx, f_prime, x1, tol = 1e-04, max_iter = 10) {
  solucao <- c()
  solucao[1] <- x1
  for(i in 1:max_iter) {
    solucao[i+1] = solucao[i] - fx(solucao[i])/f_prime(solucao[i])
    if( abs(solucao[i+1] - solucao[i]) < tol) break
  }
  return(solucao)
}

## Derivada da função a ser resolvida
fprime <- function(theta){2*length(y)*(mean(y) - 1/theta)}
## Solução numerica
Ic_min <- newton(fx = ftheta, f_prime = fprime, x1 = 0.1)
Ic_max <- newton(fx = ftheta, f_prime = fprime, x1 = 2)
c(Ic_min[length(Ic_min)], Ic_max[length(Ic_max)])


## Método gradiente descendente
grad_des <- function(fx, x1, alpha, max_iter = 100, tol = 1e-04) {
  sol <- c()
  sol[1] <- x1
  for(i in 1:max_iter) {
    sol[i+1] <- sol[i] - alpha*fx(sol[i])
    if(abs(fx(sol[i+1])) < tol) break
  }
  return(sol)
}

Ic_min <- grad_des(fx = ftheta, alpha = -0.02, x1 = 0.1)
Ic_max <- grad_des(fx = ftheta, alpha = 0.01, x1 = 4)
c(Ic_min[length(Ic_min)], Ic_max[length(Ic_max)])


## Método de Newton: Sistemas de equações não lineares
newton <- function(fx, jacobian, x1, 
                   tol = 1e-04, max_iter = 10) {
  solucao <- matrix(NA, ncol = length(x1), 
                    nrow = max_iter)
  solucao[1,] <- x1
  for(i in 1:max_iter) {
    J <- jacobian(solucao[i,])
    grad <- fx(solucao[i,])
    solucao[i+1,] = solucao[i,] - 
      solve(J, grad)
    if( sum(abs(solucao[i+1,] - solucao[i,])) 
        < tol) break
  }
  return(solucao)
}

## Sistema a ser resolvido
fx <- function(x){c(x[2] - 0.5*(exp(x[1]/2) + exp(-x[1]/2)), 
                    9*x[1]^2 + 25*x[2]^2 - 225 )}
## Jacobiano
Jacobian <- function(x) {
  jac <- matrix(NA,2,2)
  jac[1,1] <- -0.5*(exp(x[1]/2)/2 - exp(-x[1]/2)/2)
  jac[1,2] <- 1 
  jac[2,1] <- 18*x[1]
  jac[2,2] <- 50*x[2]
  return(jac)
}

sol <- newton(fx = fx, jacobian = Jacobian, x1 = c(1,1))
tail(sol,4) ## Solução
fx(sol[8,]) ## OK

## Gradiente descendente: Sistemas de equações não lineares
grad_des <- function(fx, x1, alpha, max_iter = 100, tol = 1e-04) {
  solucao <- matrix(NA, ncol = length(x1), nrow = max_iter)
  solucao[1,] <- x1
  for(i in 1:c(max_iter-1)) {
    solucao[i+1,] <- solucao[i,] - alpha*fx(solucao[i,])
    #print(c(i, solucao[i+1,]))
    if( sum(abs(solucao[i+1,] - solucao[i,])) <= tol) break
  }
  return(solucao)
}

fx <- function(x) {
  y <- c(5.15, 6.40, 2.77, 5.72, 6.25, 3.45, 5.00, 6.86, 4.86, 3.72)
  z <- c(0.28, 0.78, 0.40, 0.88, 0.94, 0.04, 0.52, 0.89, 0.55, 0.45)
  term1 <- - 2*sum(y - x[1] - x[2]*z)
  term2 <- -2*sum( (y - x[1] - x[2]*z)*z)
  out <- c(term1, term2)
  return(out)
}
sol_grad <- grad_des(fx = fx, x1 = c(5, 0), alpha = 0.05, max_iter = 140)
fx(x = sol_grad[137,])


## Otimizando funções perda
par(mfrow = c(1,3), mar=c(2.6, 3, 1.2, 0.5), mgp = c(1.6, 0.6, 0))
perda_quad <- function(mu, dd) { sum((dd-mu)^2) }
perda_quad <- Vectorize(perda_quad, "mu")
perda_abs <- function(mu, dd) { sum(abs(dd-mu)) }
perda_abs <- Vectorize(perda_abs, "mu")
perda_minimax <- function(mu, dd) { max(abs(dd-mu)) }
perda_minimax <- Vectorize(perda_minimax, "mu")
set.seed(123)
dd <- rgamma(100, shape = 1)
mu <- seq(0, 3, l = 1000)
plot(perda_quad(mu, dd) ~ mu, type = "l", 
     ylab = expression((x - mu)^2), xlab = expression(mu),
     main = "Perda quadrática")
plot(perda_abs(mu, dd) ~ mu, type = "l", 
     ylab = expression(abs(x - mu)), xlab = expression(mu),
     main = "Perda absoluta")
plot(perda_minimax(mu, dd) ~ mu, type = "l", 
     ylab = expression(max(x - mu)), xlab = expression(mu),
     main = "Perda minimax")

perda_quad <- function(mu, dd) { sum((dd-mu)^2) }
perda_abs <- function(mu, dd) { sum(abs(dd-mu)) }
perda_minimax <- function(mu, dd) { max(abs(dd-mu)) }

set.seed(123)
y <- rpois(100, lambda = 3)

# Perda quadrática
fit_quad <- optimize(f = perda_quad, interval = c(0, 20), dd = y)
# Perda absoluta
fit_abs <- optimize(f = perda_abs, interval = c(0, 20), dd = y)
# Perda minimax
fit_minimax <- optimize(f = perda_minimax, interval = c(0, 20), dd = y)

## Programação linear
require(lpSolve)
objective.in <- c(25, 20) # c's
const.mat <- matrix(c(20, 12, 1/15, 1/15), 
                    nrow=2, byrow=TRUE)
const.rhs <- c(1800, 8)
const.dir <- c("<=", "<=")
optimum <- lp(direction = "max", 
              objective.in, 
              const.mat,
              const.dir, const.rhs)
optimum$solution # Solução
optimum$objval # Lucro


## Comparando algoritmos
require(mvtnorm)
fx <- function(xx){-dmvnorm(xx)}

require(optimx)
res <- optimx(par = c(-1,1), fn = fx,  
              method = c("BFGS","Nelder-Mead","CG"))
res
