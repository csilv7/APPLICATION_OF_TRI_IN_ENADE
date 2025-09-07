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
score.CE <- rowSums(matrix.hits[, (n.itens.FG+1):(n.itens)], na.rm = T)

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
# [3.4] PROPORÇÃO DE ACERTOS POR ESCORE
# -------------------------------------

# Converter para Data Drame
df.hits <- as.data.frame(matrix.hits)

# Nome dos Itens
# index <- index

# Ajustar o nome das colunas com o `index`
colnames(df.hits) <- index

# Adiciona `score_total` à base
df.hits$SCORE <- score.tot

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

# ----------------------
# [3.4.1] ITENS DE 1 A 5
# ----------------------

# Itens
itens1 <- 1:5

# Itens de 1 (1º FG) a 5 (5º FG)
p1 <- get.curves_by_item(df.hits, index[itens1]) %>%
  ggplot(aes(x = SCORE, PROP_HIT, color = ITEM)) +
  geom_line(size = 1) + geom_point() +
  xlim(0, n.itens) + ylim(0, 1) +
  labs(
    x = "Escore",
    y = "Proporção de Acertos (%)",
    color = paste0("ITENS ", index[itens1[1]], " A ", index[itens1[5]]),
    
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Imprimir p1
print(p1)

# -----------------------
# [3.4.2] ITENS DE 6 A 10
# -----------------------

# Itens
itens2 <- 6:10

# Itens de 1 (1º FG) a 5 (5º FG)
p2 <- get.curves_by_item(df.hits, index[itens2]) %>%
  ggplot(aes(x = SCORE, PROP_HIT, color = ITEM)) +
  geom_line(size = 1) + geom_point() +
  xlim(0, n.itens) + ylim(0, 1) +
  labs(
    x = "Escore",
    y = "Proporção de Acertos (%)",
    color = paste0("ITENS ", index[itens2[1]], " A ", index[itens2[5]]),
    
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Imprimir p2
print(p2)

# ------------------------
# [3.4.3] ITENS DE 11 A 15
# ------------------------

# Itens
itens3 <- 11:15

# Itens de 1 (1º FG) a 5 (5º FG)
p3 <- get.curves_by_item(df.hits, index[itens3]) %>%
  ggplot(aes(x = SCORE, PROP_HIT, color = ITEM)) +
  geom_line(size = 1) + geom_point() +
  xlim(0, n.itens) + ylim(0, 1) +
  labs(
    x = "Escore",
    y = "Proporção de Acertos (%)",
    color = paste0("ITENS ", index[itens3[1]], " A ", index[itens3[5]]),
    
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Imprimir p3
print(p3)

# ------------------------
# [3.4.4] ITENS DE 16 A 20
# ------------------------

# Itens
itens4 <- 16:20

# Itens de 1 (1º FG) a 5 (5º FG)
p4 <- get.curves_by_item(df.hits, index[itens4]) %>%
  ggplot(aes(x = SCORE, PROP_HIT, color = ITEM)) +
  geom_line(size = 1) + geom_point() +
  xlim(0, n.itens) + ylim(0, 1) +
  labs(
    x = "Escore",
    y = "Proporção de Acertos (%)",
    color = paste0("ITENS ", index[itens4[1]], " A ", index[itens4[5]]),
    
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Imprimir p4
print(p4)

# ------------------------
# [3.4.5] ITENS DE 21 A 25
# ------------------------

# Itens
itens5 <- 21:25

# Itens de 1 (1º FG) a 5 (5º FG)
p5 <- get.curves_by_item(df.hits, index[itens5]) %>%
  ggplot(aes(x = SCORE, PROP_HIT, color = ITEM)) +
  geom_line(size = 1) + geom_point() +
  xlim(0, n.itens) + ylim(0, 1) +
  labs(
    x = "Escore",
    y = "Proporção de Acertos (%)",
    color = paste0("ITENS ", index[itens5[1]], " A ", index[itens5[5]]),
    
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Imprimir p5
print(p5)

# ------------------------
# [3.4.6] ITENS DE 26 A 30
# ------------------------

# Itens
itens6 <- 26:30

# Itens de 1 (1º FG) a 5 (5º FG)
p6 <- get.curves_by_item(df.hits, index[itens6]) %>%
  ggplot(aes(x = SCORE, PROP_HIT, color = ITEM)) +
  geom_line(size = 1) + geom_point() +
  xlim(0, n.itens) + ylim(0, 1) +
  labs(
    x = "Escore",
    y = "Proporção de Acertos (%)",
    color = paste0("ITENS ", index[itens6[1]], " A ", index[itens6[5]]),
    
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Imprimir p6
print(p6)

# ------------------------
# [3.4.7] ITENS DE 31 A 35
# ------------------------

# Itens
itens7 <- 31:35

# Itens de 1 (1º FG) a 5 (5º FG)
p7 <- get.curves_by_item(df.hits, index[itens7]) %>%
  ggplot(aes(x = SCORE, PROP_HIT, color = ITEM)) +
  geom_line(size = 1) + geom_point() +
  xlim(0, n.itens) + ylim(0, 1) +
  labs(
    x = "Escore",
    y = "Proporção de Acertos (%)",
    color = paste0("ITENS ", index[itens7[1]], " A ", index[itens7[5]]),
    
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Imprimir p7
print(p7)

# ------------------------
# [3.4.8] VERSÃO OTIMIZADA
# ------------------------

# Variável auxiliar
id.itens_group <- seq(1, n.itens, by = 5)

# Plotar os Curvas em Grupos de 5 Itens
for (j in id.itens_group) {
  # Grupo de Itens (Sua lógica robusta já está aqui)
  itens_group <- index[j:min(j+4, n.itens)]
  
  # Data Frame que será usado no Gráfico
  df.plot <- get.curves_by_item(df.hits, itens_group)
  
  # Gráfico com ggplot2
  p <- ggplot(data = df.plot, aes(x = SCORE, y = PROP_HIT, color = ITEM)) +
    geom_line(linewidth = 1) + 
    geom_point() +
    xlim(0, n.itens) + 
    ylim(0, 1) +
    labs(
      x = "Escore",
      y = "Proporção de Acertos (%)",
      color = paste0("ITENS ", itens_group[1], " A ", itens_group[length(itens_group)])
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

# --------------------------
# [3.5] GRUPOS DE DESEMPENHO
# --------------------------

# Calculando P1 (percentil 33%) e P3 (percentil 67%)
q1 <- quantile(df.hits$SCORE, probs = 1/3, na.rm = TRUE)
q3 <- quantile(df.hits$SCORE, probs = 2/3, na.rm = TRUE)

# Exibir os valores
q1; q3

# Criar variável de Grupo com base nas pontuações
df.hits$GRUPO <- cut(
  df.hits$SCORE,
  breaks = c(-Inf, q1, q3, Inf),
  labels = c("GRUPO 1","GRUPO 2", "GRUPO 3"),
  include.lowest = TRUE,
  right = FALSE
)

# Armazenar os Resultados no Formato de Data Frame
tbl.div_group <- data.frame(
  FREQ = as.numeric(table(df.hits$GRUPO)),
  PERCENT = as.numeric(prop.table(table(df.hits$GRUPO))) * 100
)

# Ajuste do Nome das Linhas
row.names(tbl.div_group) <- str_to_title(c("GRUPO 1","GRUPO 2", "GRUPO 3"))

# Visualizar 
gt(tbl.div_group, rownames_to_stub = T) %>%
  cols_label(
    FREQ = md("**Frquência**"), PERCENT = md("**Percentual**")
  ) %>%
  fmt_number(columns = FREQ, decimals = 0, sep_mark = ".") %>%
  fmt_number(columns = PERCENT, decimals = 2, dec_mark = ",")
