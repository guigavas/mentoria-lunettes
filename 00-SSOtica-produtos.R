## Observando dados Lunettes
## 29-Out
## Guiga Vasconcelos

# Carregando pacotes
library(readxl) # ler excel
library(dplyr) # union


## Passo 0, função que otimiza busca do arquivo recente

# Função: devolve o caminho do arquivo mais recente com dado prefixo
latest_file <- function(prefixo, file = "dados"){
  files <-
    list.files(
      path = file, #onde procurar
      pattern =
        paste0(
          "^",
          prefixo,
          ".*[0-9]{4,4}-[01][0-9]-[0-3][0-9]",
          "_[0-2][0-9]-[0-5][0-9]-[0-5][0-9]\\.xlsx$"
        ), #só entra arquivos cujo nome casa com o pattern
      full.names = TRUE
    ) |>
    sort(decreasing = TRUE) |>
    head(n = 1)
  # não deixe mais do que 1 informação por linha
  # [0-9], qualquer 1 único caracter que seja ou 0 a 9
  # {n,m}, no mínimo n repetições do caracter que vem antes, e no máximo m
  # entra qualquer caracter a partir do ponto em ".*"
  # regex é o texto concatenado, e \\. é um escape em R
  # $ faz o fim do nome concatenado
  # full.names signfinica trazer o diretório todo
  # head n = 1

}

file_loja_lunettes <- latest_file("exportacao-produtos-lunettes_by_lari-")
file_loja_nova <- latest_file("exportacao-produtos-nova_loja-")

#Tratamento de erro
if (length(file_loja_lunettes) == 0 || length(file_loja_nova) == 0) {
  stop("arquivo(s) não encontrado(s)")
}

dados_loja_lunettes <- read_excel(file_loja_lunettes)
dados_loja_nova <- read_excel(file_loja_nova)

View(dados_loja_lunettes)
View(dados_loja_nova)
glimpse(dados_loja_lunettes)
glimpse(dados_loja_nova)

## Passo 1, Agregação e processando novas variáveis de controle

# QC 1- Mesma quantidade de colunas e mesmos nomes:
if (!identical(names(dados_loja_lunettes),names(dados_loja_nova))) {
  # QC 2- Checar e dizer o que difere:
  lunettes_only <- setdiff(names(dados_loja_lunettes), names(dados_loja_nova)) #somente na lunettes
  nova_only <- setdiff(names(dados_loja_nova), names(dados_loja_lunettes)) #somente na loja nova
  stop(
    "Colunas divergem entre lojas.\n",
    "Só em Lunettes: ", paste0(lunettes_only, collapse = ","), "\n", # "\n" é quebra de linha pro text do diagnóstico
    "Só em loja_nova: ", paste0(nova_only, collapse = ",")
    )
}

# Union dos dados
# Extrair nome da loja a partir do nome do arquivo

extract_SKU_loja <- function(path) {
  # de "dados/exportacao-produtos-lunettes_by_lari-2026-10-29_...xlsx"
  # para "lunettes_by_lari"
  sub(
    "^exportacao-produtos-(.*)-\\d{4}-\\d{2}-\\d{2}_.*\\.xlsx$",
    "\\1",
    basename(path)
  )
}

dados_loja_lunettes$loja <- extract_SKU_loja(file_loja_lunettes)
dados_loja_nova$loja     <- extract_SKU_loja(file_loja_nova)

dados <- bind_rows(dados_loja_lunettes, dados_loja_nova)

# Confere pós-union
stopifnot(nrow(dados) == nrow(dados_loja_lunettes) + nrow(dados_loja_nova))
print(table(dados$loja))


## Passo 2, Verificar se temos SKUs unique, via concat com countunique
#names(dados)

dados <- dados %>%
  mutate(chave_ref = paste(`Referência`, `Descrição`, `Unidade`, sep = "_"))
# sep é o separador da concat, e por isso, usa paste ao invés de paste0

# resumo de unicidade
dados %>%
  summarise(
    n_linhas = n(),
    n_chaves = n_distinct(chave_ref),
    flag_unique = n_linhas - n_chaves
  )

# QC 1- Investigando os flag_unique

dados |>
  count (chave_ref) |>
  filter (n > 1) |>
  arrange (desc(n))

#QC 2- Observar apenas linhas com duplicata
# verificar se há algo diferente (preço, etc)

colisoes <- dados %>%
  count(chave_ref) %>%
  filter(n > 1) %>%
  pull(chave_ref)

dados %>%
  filter(chave_ref %in% colisoes) %>%
  arrange(chave_ref) %>%
  View()

#QC 3- criar base_catalogo — produtos únicos, sem dimensão loja
# remove duplicata considerando só essas colunas, mantendo a 1ª linha de cada grupo

base_catalogo <- dados %>%
  distinct(chave_ref, .keep_all = TRUE)

nrow(base_catalogo)


