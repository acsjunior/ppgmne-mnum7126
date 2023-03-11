### Módulo · Visualização de dados · Uma abordagem baseada no ggplot2 ----------
### Prof. Wagner Hugo Bonat ----------------------------------------------------

## Carregando pacotes adicionais
library(tidyverse)

## Conjunto de dados: Human Resources Data Set

## Endereço do conjunto de dados
# https://www.kaggle.com/rhuebner/human-resources-data-set
url <- "http://leg.ufpr.br/~wagner/data/people_analytics.csv"
dados <- read.table("people_analytics.csv", header = TRUE, sep = ";")

## Visão inicial
head(dados)
glimpse(dados)

################################################################################
## Analise univariada: Qualitativas nominais ###################################
################################################################################

# Tabela frequência absoluta: genero
dados %>%
  count(genero)

# Tabela frequência absoluta: estado civil
dados %>%
  count(estado_civil)

# Tabela de frequência relativa: departamento
dados %>%
  group_by(departamento) %>%
  summarise(n = n()) %>%
  mutate(freq = (n / sum(n))*100 ) %>%
  arrange(desc(freq))

## Tabela de frequência relativa: forma de recrutamento
dados %>%
  group_by(forma_recrutamento) %>%
  summarise(n = n()) %>%
  mutate(freq = (n / sum(n))*100 ) %>%
  arrange(desc(freq))

## Representações gráficas populares

# Gráfico de barras
ggplot(data = dados,
       mapping = aes(x = estado_civil)) +
  geom_bar() +
  xlab("Estado civil") +
  ylab("Frequência absoluta")

# Gráfico de pizza (não use!)
ggplot(data = dados,
       mapping = aes(x = factor(1),
                     fill = factor(estado_civil))) +
  labs(fill = "Estado civil") +
  geom_bar(width = 0.75,
           col = 1) +
  coord_polar(theta = "y") +
  xlab(NULL) +
  ylab(NULL)

## Ordenando os níveis do gráfico
tab_est_civil <- table(dados$estado_civil)
ordem <- order(tab_est_civil, decreasing = TRUE)
dados$estado_civil <- as.factor(dados$estado_civil)
dados$estado_civil <- factor(dados$estado_civil,
                             levels = levels(dados$estado_civil)[ordem])

## Gráfico de barras com frequência absoluta
ggplot(data = dados,
       mapping = aes(x = estado_civil)) +
  geom_bar() +
  xlab("Estado civil") +
  ylab("Frequência absoluta")

## Gráfico de barras com frequência relativa
ggplot(data = dados,
       mapping = aes(x = estado_civil)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Estado civil") +
  ylab("Frequência absoluta")

################################################################################
## Variáveis qualitativas ordinais #############################################
################################################################################

## Performance
dados %>%
  count(performance_score)

## Definindo a ordem
dados$performance_score <- factor(dados$performance_score, 
                                  levels = c("insuficiente",
                                             "precisa melhorar",
                                             "atende totalmente",
                                             "excede"))

## Tabela de frequência absoluta
dados %>%
  count(performance_score)

## Tabela de frequência acumulada
dados %>%
  group_by(performance_score) %>%
  summarise(n = n()) %>%
  mutate(Acum = cumsum((n / sum(n))*100) )

## Representações gráficas usuais
ggplot(data = dados,
       mapping = aes(x = performance_score)) +
  geom_bar() +
  xlab("Escore") +
  ylab("Frequência absoluta")

################################################################################
## Análise univariada: Quantitativa discreta ###################################
################################################################################

## Tabela de frequência: faltas
dados %>%
  count(faltas)

## Tabela de frequências absoluta, relativa e acumulada.
dados %>%
  group_by(faltas) %>%
  summarise(n = n()) %>%
  mutate(Freq = (n / sum(n))*100,
         Acu = cumsum(Freq))

## Representação gráfica
ggplot(data = dados,
       mapping = aes(x = faltas)) +
  geom_bar() +
  xlab("Faltas") +
  ylab("Frequência absoluta")


################################################################################
## Análise univariada: Quantitativa contínua ###################################
################################################################################

## Discretizando a variável salário
faixas <- c(0, 2090.01, 4180.01, 10450.01, 20900.01, Inf)
dados$sal_class <- cut(dados$salario/12,
                       breaks = faixas,
                       labels = c("E",
                                  "D",
                                  "C", "B", "A"))
# Gráfico de barras
ggplot(dados) +
  geom_bar(aes(x = sal_class))


# Histograma
ggplot(data = dados,
       mapping = aes(x = salario)) +
  geom_histogram() +
  xlab("Salário anual") + 
  ylab("Frequência absoluta")

## Verdadeiro histograma
ggplot(data = dados,
       mapping = aes(x = salario,
                     y = ..density..)) +
  geom_histogram() +
  xlab("Salário anual") + 
  ylab("Densidade")

## Histograma por genero
ggplot(data = dados,
       mapping = aes(x = salario)) +
  geom_histogram() +
  facet_wrap(facets = ~genero) +
  xlab("Engajamento") + 
  ylab("Freq. Absoluta")


## Medidas descritivas
dados <- dados %>%
  mutate(renda_mensal = salario/12)

dados %>%
  summarise("Media" = mean(renda_mensal),
            "Mediana" = median(renda_mensal),
            "Desvio_padrao" = sd(renda_mensal),
            "Min" = min(renda_mensal),
            "Max" = max(renda_mensal))

################################################################################
## Análise multivariada: Visualizando mais do que uma variável #################
################################################################################

## Qualitativa vs Qualitativa

# Tabelas de dupla entrada
dados %>%
  count(genero, forma_recrutamento) %>%
  pivot_wider(names_from = genero, values_from = n)

## Representação gráfica
idx <- order(table(dados$forma_recrutamento), decreasing = TRUE)
dados$forma_recrutamento <- as.factor(dados$forma_recrutamento)
dados$forma_recrutamento <- factor(dados$forma_recrutamento, 
                                   levels = levels(dados$forma_recrutamento)[idx])

# Barras empilhadas
ggplot(data = dados,
       mapping = aes(x = forma_recrutamento,
                     fill = genero)) + 
  geom_bar(position = "stack")

# Barras lado a lado
ggplot(data = dados,
       mapping = aes(x = forma_recrutamento,
                     fill = genero)) + 
  geom_bar(position = "dodge")


### Qualitativa vs Quantitativa
# Medidas descritivas por forma de recrutamento
tab <- dados %>%
  group_by(forma_recrutamento) %>%
  summarise("Media" = mean(renda_mensal),
            "Mediana" = median(renda_mensal),
            "Desvio_padrao" = sd(renda_mensal),
            "Min" = min(renda_mensal),
            "Max" = max(renda_mensal)) %>%
  arrange(desc(Media))
tab

## Gráfico para comparação
dados$forma_recrutamento <- factor(dados$forma_recrutamento,
                                   levels = tab$forma_recrutamento)
ggplot(dados) +
  geom_boxplot(aes(x = forma_recrutamento, y = renda_mensal)) +
  xlab("Forma de recrutamento") +
  ylab("Renda mensal") +
  coord_flip()

## Quantitativa vs Quantitativa

## Diagrama de dispersão
ggplot(dados) +
  geom_point(aes(x = engajamento, y = renda_mensal))

## Diagrama de dispersão + transformação
ggplot(dados) +
  geom_point(aes(x = engajamento, y = renda_mensal)) +
  scale_y_log10()

## Diagrama de dispersão + tranformação + grupo
ggplot(dados) +
  geom_point(aes(x = engajamento, y = renda_mensal, col = genero)) +
  scale_y_log10()

## Diagrama de dispersão + tranformação + grupo + forma de recrutamento
ggplot(dados) +
  geom_point(aes(x = engajamento, y = renda_mensal, col = genero)) +
  scale_y_log10() +
  facet_wrap(~ forma_recrutamento)


## Diagrama de dispersão + tranformação + grupo + forma de recrutamento + stats
ggplot(dados) +
  geom_point(aes(x = engajamento, y = renda_mensal, col = genero)) +
  scale_y_log10() +
  facet_wrap(~ forma_recrutamento, scales = "free") +
  geom_smooth(aes(x = engajamento, y = renda_mensal, col = genero), se = FALSE)

### FIM ########################################################################