## Observando dados Lunettes
## 29-Out
## Guiga Vasconcelos

# Carregando pacotes
library(readxl) # ler excel
library(dplyr) # union

########################
## Passo 0, função que otimiza busca do arquivo(s) mais recente(s) importado(s)
########################

# Função: devolve o caminho do arquivo mais recente, a partir de um prefixo padrão
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
  # Explicando racional:
    # [0-9], significa qualquer 1 único caracter que seja de 0 a 9
    # {n,m}, significa no mínimo n repetições do caracter que vem antes, e no máximo m
    # entra qualquer caracter a partir do ponto em ".*"
    # $ faz o fim do nome concatenado
    # full.names signfinica trazer o diretório todo
    # head n = 1, significa pegar apenas 1 file no caso a mais recente

}

file_loja_lunettes <- latest_file("exportacao-produtos-lunettes_by_lari-") # prefixo
file_loja_nova <- latest_file("exportacao-produtos-nova_loja-") # prefixo

# Função: Tratamento de erro
if (length(file_loja_lunettes) == 0 || length(file_loja_nova) == 0) {
  stop("arquivo(s) não encontrado(s)")
}

# Função: Trazer os dados
dados_loja_lunettes <- read_excel(file_loja_lunettes)
dados_loja_nova <- read_excel(file_loja_nova)

# View(dados_loja_lunettes)
# View(dados_loja_nova)
glimpse(dados_loja_lunettes)
glimpse(dados_loja_nova)

########################
## Passo 1, Agregação e categorizando as variáveis de controle
########################

nomes_lunettes <- dados_loja_lunettes |> names() |> sort()
nomes_nova <- dados_loja_nova |> names() |> sort()
# Explicando racional
  # names extrai os nomes das colunas do dataframe (retorna um vetor de caracteres)
  # fizemos o sort, evitando falso QC por mismatch de variáveis em posições diferentes

# QC 1- Valida se temos a mesma quantidade de colunas e mesmos nomes
if (!identical(nomes_lunettes,nomes_nova)) {
  # QC 2- if TRUE, checa e diz qual(is) diferem, seja comparando A com B,
  # seja B com A:
  stop(
    "Colunas divergem entre lojas.\n",
    "Só em Lunettes: ",
    setdiff(nomes_lunettes, nomes_nova) |>
      paste0(collapse = ","), "\n",
    "Só em loja_nova: ",
    setdiff(nomes_nova, nomes_lunettes) |>
      paste0(collapse = ",")
  )
}

# Função pré-union: Extrair o nome da loja a partir do arquivo importado

extract_SKU_loja <- function(path) {
  sub(
    "^exportacao-produtos-(.*)-\\d{4}-\\d{2}-\\d{2}_.*\\.xlsx$",
    "\\1",
    basename(path)
  )
}
# Explicando racional:
  #
  # de "dados/exportacao-produtos-lunettes_by_lari-2026-10-29_...xlsx"
  # para "lunettes_by_lari"

dados_loja_lunettes$loja <- extract_SKU_loja(file_loja_lunettes)
dados_loja_nova$loja     <- extract_SKU_loja(file_loja_nova)



# Função: Union dos dados
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


