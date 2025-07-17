# Modelagem de tópicos LDA
library(stm)
library(tidymodels)
library(tidytext)
library(topicmodels)
library(textmineR)

# Criando a matriz
spars_tidy_dfl <- tidy_dfl %>%
  count(autores, word) %>%
  cast_sparse(autores, word, n)

spars_tidy_dfl

num_topics_range <- seq(2, 10, by = 1)

# Lista para armazenar a perplexidade
perplexity_values <- list()

# Calculando a perplexidade para cada número de tópicos
for (k in num_topics_range) {
  lda_model <- LDA(spars_tidy_dfl, k = k, control = list(seed = 1234))
  perplexity_values[[k]] <- perplexity(lda_model)
}

# Criando um data frame com os resultados
perplexity_df <- data.frame(
  num_topics = num_topics_range,
  perplexity = unlist(perplexity_values)
)
perplexity_df
#Dependendo do número de documentos, demora.

# Plotando a perplexidade
library(ggplot2)
ggplot(perplexity_df, aes(x = num_topics, y = perplexity)) +
  geom_line() +
  geom_point() +
  labs(title = "Perplexidade para Diferentes Números de Tópicos",
       x = "Número de Tópicos", y = "Perplexidade") +
  theme_minimal()
#Procure o cotovelo, quando a perplexidade começa a diminuir menos.
# Train a model
library(stm)
# Alterar k a partir da perplexidade
topic_model <- stm(spars_tidy_dfl, K = 4)

#Verificando o sumário
summary(topic_model)

# Highest sao as palavras mais citadas
# FREX é a medida de exclusividade
# Lift é a associação das palavras com seu tópico
# Score é uma medida ponderada de FREX e Lift

# Adaptar intervalo de topics
score_words <- labelTopics(topic_model, n=20, topics = 1:4)

score_words

# Probabilidade por documento
doc_topic <- tidy(topic_model, matrix = "gamma",document_names = rownames(spars_tidy_dfl))

# Visualizar em tabela ao invés do gráfico
doc_topic_t <- doc_topic %>%
  pivot_wider(names_from = topic, values_from = gamma, values_fill = 0)
doc_topic_t

# Gerando variável de classificação de tópico
# Adicionar uma coluna indicando o número do tópico com a maior probabilidade
doc_topic_t2 <- doc_topic_t %>%
  rowwise() %>%
  mutate(topic_number = which.max(c_across(2:5))) #Alterar de acordo com k
doc_topic_t2

table(doc_topic_t2$topic_number)
# Identificando um tópico para cada documento
doc_gg <- doc_topic %>%
  mutate(document = forcats::fct_reorder(document, gamma), topic = factor(topic)) %>%
  ggplot(aes(gamma, topic, fill = topic))+
  geom_col(show.legend = F)+
  facet_wrap(vars(document), ncol = 5)+
  theme(strip.text = element_text(size = 9))+
  labs(x = expression(gamma), y = "Topic")

#ggsave("/Users/fernandolhamas/Documentos/FakeNews/gamma_fakenews_leis.png", doc_gg, width = 11, height = 8.5, dpi = 300)
doc_gg

