## Observando dados Lunettes
## 29-Out
## Guiga Vasconcelos

# Carregando pacotes
library(readxl) # ler excel
library(dplyr) # union


## Passo 0, função que otimiza busca do arquivo recente

# Função: devolve o caminho do arquivo mais recente com dado prefixo
latest_file <- function(prefixo, file = "dados"){
  files <- list.files(
    path = file, #onde procurar
    pattern = paste0("^", prefixo, ".*\\.xlsx$"), #só entra arquivos cujo nome casa com o pattern
    full.names = TRUE
  )
  # timestamp entra a partir do .*
  # regex é o texto concatenado, e \\. é um escape em R
  # $ faz o fim do nome concatenado
  # full.names signfinica trazer o diretório todo

  if (length(files) == 0) stop("Nenhum arquivo com prefixo: ", prefixo)
  files[order(files, decreasing = TRUE)][1]
}
# IF, se não achar nada, falha cedo com mensagem clara
# order: compara as strings e ordena os arquivos com maior timestamp
# [1]: pega o primeiro que, após a ordenação decrescente, é o mais recente

file_loja_lunettes <- latest_file("exportacao-produtos-lunettes_by_lari-")
file_loja_nova <- latest_file("exportacao-produtos-nova_loja-")

dados_loja_lunettes <- read_excel(file_loja_lunettes)
dados_loja_nova <- read_excel(file_loja_nova)

View(dados_loja_lunettes)
View(dados_loja_nova)
str(dados_loja_lunettes)
str(dados_loja_nova)

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


