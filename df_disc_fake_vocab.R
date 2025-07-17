# Vocabulário
library(tidyverse)
library(tidytext)

# Palavras únicas nos 3 momentos
# Lista com os nomes dos objetos
#objetos <- c("resultados_roda01", "resultados_roda02", "resultados_roda03",
             "resultados_roda04", "resultados_roda05", "resultados_roda06")

# Loop sobre os objetos
for (objeto in objetos) {
  # Obter o objeto atual
  dados_tidy <- get(objeto)
  
  # Contagem de palavras únicas
  palavras_unicas <- tidy_dfl %>%
    distinct(word) %>%
    count()
  
  # Imprimir o resultado
  print(palavras_unicas)
}

# Adjetivos e verbos
library(udpipe)

# Carregando um modelo para português. Substitua "portuguese" pelo modelo específico se necessário
modelo <- udpipe_download_model(language = "portuguese")
ud_model <- udpipe_load_model(modelo$file_model)

# Juntando as bases
#tidycomp <- bind_rows (resultados_roda01, resultados_roda02, resultados_roda03,resultados_roda04, resultados_roda05, resultados_roda06)
# Realizando POS tagging
dados_annotated <- udpipe_annotate(ud_model, x = tidy_dfl$word)
dados_pos <- as.data.frame(dados_annotated)

# Filtrando por adjetivos e verbos
adjetivos <- dados_pos %>%
  filter(upos == "ADJ") %>%
  count(lemma, sort = TRUE) # lemma para forma base das palavras

verbos <- dados_pos %>%
  filter(upos == "VERB") %>%
  count(lemma, sort = TRUE)

substantivos <- dados_pos %>%
  filter(upos == "NOUN") %>%
  count(lemma, sort = TRUE)

# Mostrando os principais adjetivos, verbos e substantivos
head(adjetivos, n = 10) # Ajuste 'n' para mostrar mais ou menos resultados
head(verbos, n = 10)
head(substantivos, n = 20)

# Aulas
# Palavras únicas
# Lista com os nomes dos objetos
objetos <- c("resaula01", "resaula02", "resaula03",
             "resaula04", "resaula05", "resaula06", "resaula07")

# Loop sobre os objetos
for (objeto in objetos) {
  # Obter o objeto atual
  dados_tidy <- get(objeto)
  
  # Contagem de palavras únicas
  palavras_unicas <- dados_tidy %>%
    distinct(word) %>%
    count()
  
  # Imprimir o resultado
  print(palavras_unicas)
}

# Adjetivos, verbos e substantivos
library(udpipe)

# Carregando um modelo para português. Substitua "portuguese" pelo modelo específico se necessário
modelo <- udpipe_download_model(language = "portuguese")
ud_model <- udpipe_load_model(modelo$file_model)

tidycomp <- bind_rows (resaula01, resaula02, resaula03,
                       resaula04, resaula05, resaula06, resaula07)

# Realizando POS tagging
dados_annotated <- udpipe_annotate(ud_model, x = tidycomp$word) #substitui pelo nome do arquivo
dados_pos <- as.data.frame(dados_annotated)

# Filtrando por adjetivos e verbos
adjetivos <- dados_pos %>%
  filter(upos == "ADJ") %>%
  count(lemma, sort = TRUE) # lemma para forma base das palavras

verbos <- dados_pos %>%
  filter(upos == "VERB") %>%
  count(lemma, sort = TRUE)

substantivos <- dados_pos %>%
  filter(upos == "NOUN") %>%
  count(lemma, sort = TRUE)

# Mostrando os principais adjetivos, verbos e substantivos
head(adjetivos, n = 10) # Ajuste 'n' para mostrar mais ou menos resultados
head(verbos, n = 10)
head(substantivos, n = 20)

# Individual
# Realizando POS tagging substitui tidycomp pelo nome da base
#dados_annotated <- udpipe_annotate(ud_model, x = tidycomp$word)
#dados_pos <- as.data.frame(dados_annotated)

