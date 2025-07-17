## Análise descritiva em formato tidytext
library(dplyr)
library(tidytext)

df <- tidy_dfl

# Frequência de palavras

contagem_palavras <- df %>%
  count(word, sort=T)

library(ggplot2)

#O resultado Pode indicar strings de buscas não utilizados em revisões

grafico_palavras <- contagem_palavras %>%
  top_n(25) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +  # Mude a cor das barras para azul
  labs(x = "Palavras", y = "Contagem") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 24))
grafico_palavras

# Remover expressões a depender da base na etapa anterior como stopword
# Verificar a coluna de identificação dos textos
df %>% distinct(autores)

