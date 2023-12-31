## Exemplo par mostrar o uso de memório dentro do profilling
x <- integer()
for (i in 1:1e4) {
  x <- c(x, i)
}
