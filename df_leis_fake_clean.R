## Preparando o dataframe para análise textual com tidytext

library(dplyr)
library(tidytext)

### Tokenização, stopwords e palavras escolhidas como stopwords

tidy_df <- dados_artigos %>%
  unnest_tokens(word, texto) %>%
  anti_join(get_stopwords())

tidy_df %>%
  count(word, sort = T)

# Limpeza geral
tidy_df <- tidy_df %>%
  mutate(word = gsub("[[:punct:]]", "", word)) %>%
  mutate(word = tolower(word)) %>%
  mutate(word = gsub("[[:digit:]]", "", word)) %>%
  mutate(word = gsub("[^[:alnum:]\\s]", "", word)) %>%
  filter(nchar(word) >= 3)

# Remoção de pontuações
# Remoção de maiusculas para evitar duplicações
# Remover números
# Remover caracteres especiais
# Remover palavras pequenas (quando for possível)

#limpeza com dicionário em pt-br
#limpeza, aproveitando stopwords em pt do pacote tm
removeWords=c("como","sobre","vezes","com", "uma", "para",
              "por","meu", "devido", "pois", "ainda", "nao", 
              "assim", "sim", "é", "sei")
# Verificar se tidy_df já está tokenizado
# Evitar redundância na tokenização
# A estrutura correta de stop_words é essencial

# Certifique-se de que stop_words é um tibble com coluna "word"
stop_words <- tibble(word = c(tm::stopwords("pt"), removeWords))

# Aplique anti_join diretamente sem nova tokenização
tidy_df <- tidy_df %>%
  anti_join(stop_words, by = "word")


# Lematização - redução de palavras a sua forma básica com textstem

tidy_dfl <- tidy_df  %>%
  mutate(word = textstem::lemmatize_words(word, language = "pt"))

tidy_dfl %>%
  count(word, sort = T)

# Outras formas de limpeza podem ser feitas para gerar determinadas saídas
# Ex: Remover palavras que aparecem raramente com count; remoção de palavras comuns
# adicionadas nominalmente as stopwords; remover erros de digitação com hunspell
# agrupar sinônimos, entre outros. 




