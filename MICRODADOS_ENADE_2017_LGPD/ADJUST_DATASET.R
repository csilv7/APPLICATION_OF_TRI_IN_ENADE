# ======================================
# DESCRIÇÃO DA FUNCIONALIDADE DO ARQUIVO
# ======================================
# --------------------------------------
# Este arquivo está destinada a junção
# dos dados do ENADE em um único arquivo
# Para que, posteriomente, possa se fazer
# o devido uso de todas as variáveis que
# compoem o conjunto de dados
# --------------------------------------
# ======================================

# Definir o Diretório do Dados
setwd("C:/Users/usuario/Documents/github_2025/TRI_ENADE_2017/MICRODADOS_ENADE_2017_LGPD")

# Pacotes Carregados
library(dplyr)
library(stringr)

# Definir Caminho dos Dados
path.for.data <- "2.DADOS/"

# Listar todos os arquivos .txt na pasta
files <- list.files(
  path = path.for.data, 
  pattern = "\\.txt$", 
  full.names = TRUE
)

# Ler todos os arquivos de forma otimizada com fread e armazenar em uma lista.
list.of.dataframes <- lapply(files, function(file) {
  data.table::fread(
    file,
    sep = ";",            # O delimitador
    dec = ",",            # O separador decimal
    encoding = "Latin-1", # A codificação do arquivo
    quote = "",           # Equivalente a escape_double = FALSE, não trata aspas de forma especial
    strip.white = TRUE    # Equivalente a trim_ws = TRUE (comportamento padrão, mas explícito aqui)
  )
})


# Join dos Dados
complete.data <- bind_cols(list.of.dataframes) %>% tibble()

# Ajustar os rótulos das colunas
new.names <- colnames(complete.data) %>% str_remove_all(pattern = fixed('"')) %>% str_remove(pattern = "\\.\\.\\.\\d+$")

# Alterando os rótulos das colunas
colnames(complete.data) <- new.names

# Colunas a serem excluídas (TRUE/FALSE)
drop.columns <- !duplicated(colnames(complete.data))

# Colunas selecionas
selected.columns <- new.names[drop.columns]

# Eliminar colunas duplicadas
complete.data.final <- complete.data %>% select(all_of(selected.columns))

# Salvar em RDS
saveRDS(complete.data.final, file = "DADOS_COMPLETOS.rds")