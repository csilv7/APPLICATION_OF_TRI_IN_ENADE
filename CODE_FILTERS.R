# ======================================
# DESCRIÇÃO DA FUNCIONALIDADE DO ARQUIVO
# ======================================
# --------------------------------------
# Este arquivo está destinada a aplicação
# de filtros nos Dados do ENADE
# --------------------------------------
# ======================================

# --------------------------
# [1] CONFIGURAÇÕES INICIAIS
# --------------------------

# Definir o Diretório do Dados
setwd("~/DETRAN/TRI_PROJECT_DIEGO/")

# Pacotes Carregados
library(dplyr)

# ----------------------------------------------
# [2] CARREGAMENTO E POSSÍVEIS AJUSTES DOS DADOS
# ----------------------------------------------

# Caminho do arquivo.RDS
path <- "~/DETRAN/TRI_PROJECT_DIEGO/MICRODADOS_ENADE_2017_LGPD/DADOS_COMPLETOS.rds"

# Leitura do arquivo.rds
ENADE <- readRDS(file = path)

# -------------------------------------------
# [2.1] SELEÇÃO DE VARIÁVEIS SÓCIO-ECONÔMICAS
# -------------------------------------------

# Filtro
enade.filter <- ENADE %>%
  select(
    CO_IES,
    CO_CATEGAD,
    CO_GRUPO,
    CO_CURSO,
    CO_MODALIDADE,
    CO_MUNIC_CURSO,
    CO_UF_CURSO,
    CO_REGIAO_CURSO,
    CO_TURNO_GRADUACAO,
    TP_SEXO,
    NU_IDADE,
    QE_I01,
    QE_I02,
    QE_I06,
    QE_I07,
    QE_I08,
    QE_I10,
    QE_I23,
    NT_GER,
    NT_FG,
    NT_CE,
    NT_OBJ_FG,
    NT_OBJ_CE,
    NT_DIS_FG,
    NT_DIS_CE,
    DS_VT_ESC_OFG,
    DS_VT_ESC_OCE,
    DS_VT_ACE_OFG,
    DS_VT_ACE_OCE,
    TP_PR_GER
  )

# ----------------------
# [2.2] FILTTRO DE CURSO
# ----------------------

# Filtrando Curso de Engenharia da Computação
ENG.COMP <- ENADE %>% filter(CO_GRUPO == 4003)

# Filtrando Curso de Ciencia da Computação Bacharelado
C.COMP.B <- ENADE %>% filter(CO_GRUPO == 4004)

# Filtrando Curso de Ciencia da Computação Licenciatura
C.COMP.L <- ENADE %>% filter(CO_GRUPO == 4005)

# Filtrando Curso de Sistemas de Computação
SIS.INFO <- ENADE %>% filter(CO_GRUPO == 4006)

# --------------------------
# [2.3] FILTTROS GEOGRÁFICOS
# --------------------------

# # Filtro de Unidade da Federação
# enade.PA <- enade.filter %>% filter(CO_UF_CURSO == 15)
# 
# # Filtro de Unidade da Federação e Munícipio
# enade.PA.BEL <- enade.PA %>% filter(CO_MUNIC_CURSO == 1501402)

# -----------------------------
# [2.4] FILTTROS INSTITUCIONAIS
# -----------------------------

# -----------------------------
# [3] SALVAR ARQUIVOS FILTRADOS
# -----------------------------

# Salvar em RDS
saveRDS(SIS.INFO, file = "MICRODADOS_ENADE_2017_LGPD/DADOS_GERAIS_SISTEMAS.rds")