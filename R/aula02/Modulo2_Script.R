### Módulo · Lidando com dados · Uma abordagem baseada no tidyverse ------------
### Prof. Wagner Hugo Bonat ----------------------------------------------------

## Instalando o pacote tidyverse
install.packages("tidyverse")

## Carregando o pacote tidyverse
library(tidyverse)


################################################################################
## Importação de dados #########################################################
################################################################################

## Importando arquivo do tipo .txt
url <- "http://leg.ufpr.br/~wagner/scientificR/anovareg.txt"
dados <- read_tsv(url, col_names = TRUE)
head(dados)

## Importando dados do tipo .csv
url <- "http://leg.ufpr.br/~wagner/scientificR/reglinear.csv"
dados <- read_table(url, col_names = TRUE)
head(dados)

## Baixando e importando uma planilha eletrônica
# Se precisar instalar o readxl ou o httr descomente os códigos abaixo
# install.packages("readxl")
# install.packages("httr")

library(readxl)
library(httr)
url <- "http://leg.ufpr.br/~wagner/scientificR/meus_dados.xlsx"
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
tb <- read_excel(tf, sheet = "mtcars")
tb

## Conexão com banco de dados relacionais
## O banco usado para exemplo é público e pode sair do ar.

## Se não tiver instalado os pacotes DBI e RMySQL descomente as linhas abaixo
# install.packages("DBI")
# install.packages("RMySQL")

library(DBI)
library(RMySQL)

# Criando a conexão.
db <- dbConnect(
  RMySQL::MySQL(),
  user = "rfamro", password = "",
  port = 4497, dbname = "Rfam",
  host = "mysql-rfam-public.ebi.ac.uk")

# Lista as tabelas do BD.
dbListTables(db)

# Listas as colunas em uma tabela.
dbListFields(db, "keywords")

# Importanto a tabela.
tb <- RMySQL::dbFetch(
  RMySQL::dbSendQuery(
    db, "SELECT * FROM keywords"))
str(tb)
# Desconecta
dbDisconnect(db)

################################################################################
## Arrumação de dados ##########################################################
################################################################################


## Variáveis nas colunas
tb1 <- data.frame("city" = 
                    c("C1", "C2", "C3"),
                  '2011' = c(5, 7, 3),
                  '2012' = c(6, 2, 5),
                  '2013' = c(6, 9, 7), 
                  check.names = FALSE)

tb1

## Formato longo
tb1_long <- tb1 %>% 
  pivot_longer(names_to = 'ano', 
               values_to = 'resposta', 
               cols = -city)

tb1_long

## Formato wide
tb1_long %>% 
  pivot_wider(names_from =  'ano', 
              values_from = 'resposta')

## Separando variáveis
tb <- data.frame(US = c("US1", "US2", "US3"),
                 cidade_ano = c("Curitiba/2012", "Santos/2012", "Viçosa/2016"),
                 local = c("Curitiba-PR", "Santos-SP", "Viçosa-MG"))

tb
## Separando Cidade e ano
tb_nova1 <- tb %>% separate(col = cidade_ano,
                            into = c('Cidade', 'Ano'),
                            sep = '/')
tb_nova1


## Separando local em municipio e UF.
tb_nova2 <- tb %>% separate(col = local,
                            into = c('Municipio', 'UF'),
                            sep = '-')
tb_nova2

## Unindo variáveis
tb <- data.frame(dia = c(1, 5, 23, 16),
                 mes = c(3, 6, 2, 9),
                 ano = 2018)
tb


tb <- tb %>% unite(col = 'data',
                   sep = '/',
                   c('dia', 'mes', 'ano'),
                   remove = FALSE)

tb


## Dados faltantes
tb <- data.frame(Paciente = 1:5,
                 N_con = c(0, 1, 3, 1, 2),
                 N_trat = c(NA, 0, 0, 2, 1),
                 N_rem = c(NA, 1, 1, 0, 0))
tb

## Substitui por 0
tb %>% replace_na(list(N_trat = 0,
                       N_rem = 0))

## Remove todos os NAs
drop_na(tb)


################################################################################
## Manipulação de dados ########################################################
################################################################################

## Criando um tibble
# Tabela com alunos do curso de Matemática e de Estatística.
df1 <- tibble(
  matricula = c(256, 487, 965,
                125, 458, 874, 963),
  nome = c("João", "Vanessa", "Tiago",
           "Luana", "Gisele", "Pedro",
           "André"),
  curso = c("Mat", "Mat", "Est", "Est",
            "Est", "Mat", "Est"),
  prova1 = c(80, 75, 95, 70, 45, 55, 30),
  prova2 = c(90, 75, 80, 85, 50, 75, NA),
  prova3 = c(80, 75, 75, 50, NA, 90, 30),
  faltas = c(4, 4, 0, 8, 16, 0, 20))

# Informações de cadastro dos alunos em outra base de dados.
df_extra <- tribble(
  ~mat,     ~nome, ~idade, ~bolsista,
  256,  'João'  ,     18,       "S",
  965,  'Tiago' ,     18,       "N",
  285,  'Tiago' ,     22,       "N",
  125,  'Luana' ,     21,       "S",
  874,  'Pedro' ,     19,       "N",
  321,  'Mia'   ,     18,       "N",
  669,  'Luana' ,     19,       "S",
  967,  'André' ,     20,       "N",
)

## Ordenação por uma variável
df1 %>% 
  arrange(prova1)

## Ordenação por duas ou mais (desc -> decrescente)
df1 %>% 
  arrange(prova1, desc(faltas))

## Seleção por nome da coluna
df1 %>%
  select("nome", "prova1", "faltas")

## Seleção por posição
df1 %>%
  select(c(2, 4, 7))

## Seleção por fatias
df1 %>%
  slice(3:5)

## Topo e cauda
df1 %>% 
  slice_head(n = 3)
df1 %>% 
  slice_tail(n = 3)


## Seleção por condição
df1 %>% 
  select_if(is.numeric)

# Negando
df1 %>%
  select_if(negate(is.numeric))

## Filtrando pelo valor de uma coluna
df1 %>% 
  filter(prova1 > 90 )

## Filtrando pelo resumo de duas ou mais colunas
df1 %>% 
  filter((prova1 + prova2 + prova3)/3 > 80)

## Filtro por valor
df1 %>% 
  filter(curso == "Mat")

## Combinando filtros
df1 %>% 
  filter(curso == "Est" & prova2 > 80)

## Filtrando observações com NA em uma coluna
df1 %>% 
  filter(is.na(prova2))

## Filtro por grupo
df1 %>% 
  filter(nome %in% c("João", "Pedro"))

## Filtrando NAs por colunas
df1 %>% 
  filter(!is.na(prova2) & !is.na(prova3))

## Renomeando colunas
df1 <- df1 %>% rename(mat = 'matricula',
                      nome = 'nome',
                      curso = 'curso',
                      p1 = 'prova1',
                      p2 = 'prova2',
                      p3 = 'prova3',
                      fal = 'faltas')

## Realocando colunas pelos seus nomes
df1 %>%
  relocate(p1:p3, fal)

df1 %>%
  relocate(mat, nome,
           .after = last_col())

## Criando uma nova variável
df1 <- df1 %>% 
  replace_na(list(p1 = 0, p2 = 0, p3 = 0))
df1 <- df1 %>% 
  mutate(media = (p1 + p2 + p3)/3)
df1


## Criando uma nova variavel condicionado
breaks <- c(0, 40, 70, 100)
df1$classificacao <- cut(df1$media, 
                         breaks = breaks, 
                         labels = c("Baixa", 
                                    "Média", 
                                    "Alta"))
df1 %>% select(media, classificacao)


## Resumo e summarização
df1_temp <- df1 %>% select(-c(mat, media, classificacao, fal))
df1_long <- df1_temp %>% pivot_longer(names_to = 'prova', 
                                      values_to = 'valor', 
                                      cols = -c(nome, curso))
df1_long


## Resumo por curso
df1_long %>% 
  group_by(curso) %>% 
  summarise(media = mean(valor))

## Resumo por curso e prova
df1_long %>%
  group_by(curso, prova) %>% 
  summarise(media = mean(valor))

## Mais que uma estatística resumo
df1_long %>% 
  group_by(curso) %>% 
  summarise(media = mean(valor),
            desvio_padrao = sd(valor))

## Combinando estatísticas
df1_long %>% 
  group_by(curso) %>% 
  summarise(media = mean(valor),
            desvio = sd(valor),
            CV = desvio/media)

## Tabela de frequência
df1 %>% 
  group_by(curso) %>% 
  summarise("N_alunos" = n())

## Outra opção
df1 %>%
  count(curso)

## Frequência por grupo
df1 %>% 
  group_by(curso) %>%
  count(classificacao)

################################################################################
## Combinação de dados #########################################################
################################################################################

# Concatenação na vertical (pilha).
bind_rows(df1[1:3, c(1, 3, 5)],
          df1[5:7, c(1, 3, 5, 4)],
          df1[4,   c(1,    5, 4)])

# Concatenação na horizontal (fila).
bind_cols(df1[, c(1:3)],
          df1[, c(6:7)])

# Full join = união.
full_join(df1, df_extra,
          by = c("mat" = "mat", "nome"))

# Inner join = intersecção.
inner_join(df1,
           df_extra,
           by = c("mat" = "mat",
                  "nome"))

# Todos os que estão na 1º tabela
left_join(df1, df_extra,
          by = c("mat" = "mat",
                 "nome"))

# Todos os que estão na 2º tabela
right_join(df1, df_extra,
           by = c("mat" = "mat",
                  "nome"))

# Os da 2º que não aparecem na 1º.
anti_join(df1, df_extra,
          by = c("mat" = "mat",
                 "nome"))

################################################################################
## Exportação de dados #########################################################
################################################################################

## Texto pleno
write_csv(df1, 
          file = "Nome_do_arquivo.csv")

## Binário padrão R
save(df1, file = "Nome_do_arquivo.RData")

## Carregando arquivo .RData
load("Nome_do_arquivo.RData")

## Planilha eletrônica
library(writexl)
write_xlsx(df1, "Nome_do_arquivo.xlsx")

### FIM ########################################################################