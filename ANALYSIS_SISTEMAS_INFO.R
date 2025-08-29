# ======================================
# DESCRIÇÃO DA FUNCIONALIDADE DO ARQUIVO
# ======================================
# --------------------------------------
# Este arquivo está destinada a análise
# dos Dados do ENADE, Curso Sistemas de
# Informação do Brasil
# --------------------------------------
# ======================================

# --------------------------
# [1] CONFIGURAÇÕES INICIAIS
# --------------------------

# Definir o Diretório do Dados
setwd("~/DETRAN/TRI_PROJECT_DIEGO/")

# Pacotes Carregados
library(dplyr)
library(stringr)
library(ggplot2)

library(gt)

# Gabarito oficial da Prova
template.oficial <- c(
  "C", "C", "B", "B", "C", "E", "A", "D", "C", "E", "A", "C", "E", "ANULADA (E)", 
  "A", "D", "B", "B", "B", "E", "B", "D", "D", "B", "B", "E", "A", "E", "A", 
  "C", "B", "D", "C", "C", "C"
)

# Número de Itens
n.itens.FG <- 8
n.itens.CE <- 27
n.itens <- n.itens.FG + n.itens.CE

# Index
index <- c(paste0(1:n.itens.FG, "ª FG"), paste0(1:n.itens.CE, "ª CE"))

# ----------------------------------------------
# [2] CARREGAMENTO E POSSÍVEIS AJUSTES DOS DADOS
# ----------------------------------------------

# Caminho do arquivo.rds
path <- "MICRODADOS_ENADE_2017_LGPD/DADOS_GERAIS_SISTEMAS.rds"

# Leitura do arquivo.rds
SIS.INFO <- readRDS(file = path)

# Ajustes das Colunas de Vetores
SIS.INFO.AUX <- SIS.INFO %>% mutate(
  DS_VT_GAB_OFG_FIN_ADJ = strsplit(str_remove_all(DS_VT_GAB_OFG_FIN, "[^A-Z]"), ""),
  DS_VT_GAB_OCE_FIN_ADJ = strsplit(str_remove_all(DS_VT_GAB_OCE_FIN, "[^A-Z]"), ""),
  DS_VT_ESC_OFG_ADJ = strsplit(str_remove_all(DS_VT_ESC_OFG, "[^A-Z\\.\\*]"), ""),
  DS_VT_ESC_OCE_ADJ = strsplit(str_remove_all(DS_VT_ESC_OCE, "[^A-Z\\.\\*]"), "")
)

# ------------------------------------------------
# [3] ANÁLISE VIA TEÓRIA CLÁSSICA DOS TESTES (TCT)
# ------------------------------------------------

# ---------------------------------------------------------
# [3.1] TABELA COM FREQUÊNCIA DAS ALTERNATIVAS DE RESPOSTAS
# ---------------------------------------------------------

# Criando Data Frame de Armazenamento
tbl.freq.FG <- tbl.freq.CE <- data.frame()

# Iteração de preenchimento das informações FG
for (k in 1:n.itens.FG) {
  # Distribuição de Frequências para k-ésima resposta
  frequencys <- table(sapply(SIS.INFO.AUX$DS_VT_ESC_OFG_ADJ, `[`, k))
  
  # Total de válidas
  valid <- sum(frequencys[LETTERS[1:5]])
  
  # Contagem de brancos (se resposta em branco ".")
  blank <- as.numeric(frequencys["."])
  
  # Contagem de múltiplas respostas (ex: "*")
  multiple <- as.numeric(frequencys["*"])
  
  # Atualiza o Data Frame que armazena as informações
  tbl.freq.FG <- bind_rows(
    tbl.freq.FG,
    data.frame(
      Gabarito = template.oficial[k],
      Branco = blank,
      Multiples = multiple,
      A = frequencys["A"],
      B = frequencys["B"],
      C = frequencys["C"],
      D = frequencys["D"],
      E = frequencys["E"],
      Valids = valid
    )
  )
}

# Iteração de preenchimento das informações
for (k in 1:n.itens.CE) {
  # Distribuição de Frequências para k-ésima resposta
  frequencys <- table(sapply(SIS.INFO.AUX$DS_VT_ESC_OCE_ADJ, `[`, k))
  
  # Total de válidas
  valid <- sum(frequencys[LETTERS[1:5]])
  
  # Contagem de brancos (se resposta em branco ".")
  blank <- as.numeric(frequencys["."])
  
  # Contagem de múltiplas respostas (ex: "*")
  multiple <- as.numeric(frequencys["*"])
  
  # Ajuste do índice
  k.adj <- k + 8
  
  # Atualiza o Data Frame que armazena as informações
  tbl.freq.CE <- bind_rows(
    tbl.freq.CE,
    data.frame(
      Gabarito = template.oficial[k.adj],
      Branco = blank,
      Multiples = multiple,
      A = frequencys["A"],
      B = frequencys["B"],
      C = frequencys["C"],
      D = frequencys["D"],
      E = frequencys["E"],
      Valids = valid
    )
  )
}

# Tabela Final
tbl.freq <- bind_rows(tbl.freq.FG, tbl.freq.CE) %>%
  mutate(ITEM_ESPEC = index) %>%
  select(
    ITEM_ESPEC, Gabarito, Branco, Multiples, 
    A, B, C, D, E, Valids
  ) %>%
  `row.names<-`(1:length(index))

# Visualizar
tbl.freq

# ----------------------------
# [3.1.2] TABELA COM PACOTE gt
# ----------------------------

# Tabela com o Pacote gt
gt(tbl.freq) %>%
  cols_label(
    ITEM_ESPEC = md("**Item Especificado**"),
    Gabarito   = md("**Gabarito**"),
    Branco     = md("**Branco**"),
    Multiples  = md("**Mútiplas**"),
    A          = md("**A**"),
    B          = md("**B**"),
    C          = md("**C**"),
    D          = md("**D**"),
    E          = md("**E**"),
    Valids     = md("**Válidas**")
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  fmt_number(
    columns = 5:10,
    decimals = 0,
    sep_mark = "."
  )

# ---------------------------------------
# [3.1.3] TABELA DE FREQUÊNCIAS RELATIVAS
# ---------------------------------------

# Frequências Relativas
tbl.freq_relative <- tbl.freq[, -c(3, 4)]
tbl.freq_relative[, 3:7] <- (tbl.freq_relative[, 3:7] / tbl.freq_relative[, 8]) * 100
tbl.freq_relative[, 8] <- rowSums(tbl.freq_relative[, 3:7])

# ---------------------------------------
# [3.1.3.1] TABELA TABELA COM PACOTE gt
# ---------------------------------------

# Tabela com o Pacote gt
gt(tbl.freq_relative) %>%
  cols_label(
    ITEM_ESPEC = md("**Item Especificado**"),
    Gabarito   = md("**Gabarito**"),
    A          = md("**A (%)**"),
    B          = md("**B (%)**"),
    C          = md("**C (%)**"),
    D          = md("**D (%)**"),
    E          = md("**E (%)**"),
    Valids     = md("**Total (%)**")
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  fmt_number(
    columns = 3:8,
    decimals = 2,
    dec_mark = ","
  )
  # fmt_percent(
  #   columns = 3:8,
  #   decimals = 2,
  #   dec_mark = ","
  # )

# ---------------------------------------
# [3.2] TABELA COM A PROPORÇÃO DE ACERTOS
# ---------------------------------------

# Criando Data Frame de Armazenamento
tbl.prop_hit <- data.frame()

for (id in 1:n.itens) {
  # Total Válido
  valid.id <- tbl.freq[["Valids"]][id]
  
  # Extração do Gabarito
  gab <- as.character(tbl.freq %>% select(Gabarito) %>% slice(id))
  
  # Proporção de Acertos (Verificação)
  if (gab == "ANULADA (E)") {
    # Proporção de Acertos
    prop.hit <- NA
  } else {
    # Proporção de Acertos
    prop.hit <- tbl.freq[[gab]][id] / valid.id
  }
  
  # Atualização das Informações
  tbl.prop_hit <- bind_rows(
    tbl.prop_hit,
    data.frame(
      ITEM_ESPEC = tbl.freq[["ITEM_ESPEC"]][id],
      GABARITO   = gab,
      PROP_HIT   = prop.hit * 100
    )
  )
}

# Visualizar
tbl.prop_hit

# -----------------------------------
# [3.2.1] TABELA TABELA COM PACOTE gt
# -----------------------------------

# Tabela com o Pacote gt
gt(tbl.prop_hit) %>%
  cols_label(
    ITEM_ESPEC = md("**Item Especificado**"),
    GABARITO   = md("**Gabarito**"),
    PROP_HIT   = md("**Proporção de Acertos (%)**")
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  fmt_number(
    columns = everything(),
    decimals = 2,
    dec_mark = ","
  )

# -----------------------------
# [3.3] HISTOGRAMAS DOS ESCORES
# -----------------------------

# ---------------------------
# [3.3.1] MATRIZ DE RESPOSTAS
# ---------------------------

# Matriz de acertos (0/1) para todos os participantes
matrix.resp <- matrix(NA, nrow = nrow(df.aux), ncol = n.itens)

# Iteração de Armazenamento
for (k in 1:n.itens.FG) {
  # Extrai a Resposta e o Gabarito do item k e Armezena os Dados
  matrix.resp[, k] <- sapply(SIS.INFO.AUX$DS_VT_ESC_OFG_ADJ, `[`, k)
}

# Iteração de Armazenamento
for (k in 1:n.itens.CE) {
  # Ajuste do índice
  k.adj <- k + 8
  
  # Extrai a Resposta e o Gabarito do item k e Armezena os Dados
  matrix.resp[, k.adj] <- sapply(SIS.INFO.AUX$DS_VT_ESC_OCE_ADJ, `[`, k)
}

# -------------------------
# [3.3.2] MATRIZ DE ACERTOS
# -------------------------

# Matriz de acertos (0/1) para todos os participantes
matrix.hits <- matrix(NA, nrow = nrow(SIS.INFO.AUX), ncol = n.itens)

# Iteração de Armazenamento
for (k in 1:n.itens.FG) {
  # Extrai a Resposta e o Gabarito do item i
  response.item <- sapply(SIS.INFO.AUX$DS_VT_ESC_OFG_ADJ, `[`, k)
  
  # Acertos (Verificação)
  if (template.oficial[k] == "ANULADA (E)") {
    # Acertos
    hits <- NA
    
    # Armezenamento dos Dados
    matrix.hits[, k] <- hits
  } else {
    # Acertos
    hits <- as.integer(response.item == template.oficial[k])
    
    # Armezenamento dos Dados
    matrix.hits[, k] <- hits
  }
}


# Iteração de Armazenamento
for (k in 1:n.itens.CE) {
  # Extrai a Resposta e o Gabarito do item i
  response.item <- sapply(SIS.INFO.AUX$DS_VT_ESC_OCE_ADJ, `[`, k)
  
  # Ajuste do índice
  k.adj <- k + 8

  # Acertos (Verificação)
  if (template.oficial[k.adj] == "ANULADA (E)") {
    # Acertos
    hits <- NA

    # Armezenamento dos Dados
    matrix.hits[, k.adj] <- hits
  } else {
    # Acertos
    hits <- as.integer(response.item == template.oficial[k.adj])

    # Armezenamento dos Dados
    matrix.hits[, k.adj] <- hits
  }
}

# --------------------
# [3.3.3] ESCORE TOTAL
# --------------------

# Escore FG
score.FG <- rowSums(matrix.hits[, 1:n.itens.FG], na.rm = T)

# Escore CE
score.FG <- rowSums(matrix.hits[, (1 + n.itens.FG):(n.itens)], na.rm = T)

# Escore Total
score.tot <- rowSums(matrix.hits, na.rm = T)

# ------------------
# [3.3.4] HISTOGRAMA
# ------------------

# Histogrma com ggplot2
ggplot(data = NULL) +
  geom_histogram(aes(x = score.FG), bins = 35, color = "white", fill = "steelblue") +
  xlim(0, 35) +
  labs(x = "Escore", y = "Frequência") +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Histogrma com ggplot2
ggplot(data = NULL) +
  geom_histogram(aes(x = score.CE), bins = 35, color = "white", fill = "steelblue") +
  xlim(0, 35) +
  labs(x = "Escore", y = "Frequência") +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Histogrma com ggplot2
ggplot(data = NULL) +
  geom_histogram(aes(x = score.tot), bins = 35, color = "white", fill = "steelblue") +
  xlim(0, 35) +
  labs(x = "Escore", y = "Frequência") +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# -------------------------------------
# [3.3] PROPORÇÃO DE ACERTOS POR ESCORE
# -------------------------------------

# Converter para Data Drame
df.hits <- as.data.frame(matrix.hits)

# Nome dos Itens
name.itens <- paste0("ITEM_", 91:(90 + n.itens))

# Ajustar o nome das colunas com o `name.itens`
colnames(df.hits) <- name.itens

# Adiciona `score_total` à base
df.hits$SCORE <- score_total

# Função para calcular proporção de acertos por escore para cada item
get.curves_by_item <- function(data, itens_ids) {
  # Data Frame para Armazenar Resultados
  curves <- data.frame()
  
  # Iteração de Obtenção dos Dados
  for (item in itens_ids) {
    curve.by.item <- data %>%
      group_by(SCORE) %>%
      summarise(
        PROP_HIT = mean(get(item), na.rm = TRUE)
      ) %>%
      mutate(ITEM = item)
    
    # Join dos Dados
    curves <- bind_rows(curves, curve.by.item)
  }
  # Retornar os DadoS
  return(curves)
}

# Variável auxiliar
id.itens_group <- seq(1, n.itens, by = 10)

# Plotar os Curvas em Grupos de 10 Itens
for (i in id.itens_group) {
  # Grupo de Itens
  itens_group <- name.itens[i:min(i+9, n.itens)]
  
  # Data Frame que será usado no Gráfico
  df.plot <- get.curves_by_item(df.hits, itens_group)
  
  # Gráfico com ggplot2
  p <- ggplot(data = df.plot, aes(x = SCORE, PROP_HIT, color = ITEM)) +
    geom_line(size = 1) + geom_point() +
    xlim(0, 45) + ylim(0, 1) +
    labs(
      x = "Escore",
      y = "Proporção de Acertos (%)",
      color = paste0("ITENS ", i+90, " A ", min(i+9+90, n.itens+90)),
      
    ) +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 8, face = "bold"),
      legend.title = element_text(size = 10, face = "bold"),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    )
  
  # Imprimir Gráfico
  print(p)
}















