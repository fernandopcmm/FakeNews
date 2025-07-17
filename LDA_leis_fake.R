# Modelagem de tópicos LDA
library(stm)
library(tidymodels)
library(tidytext)
library(topicmodels)
library(textmineR)

# Adicionando um identificador de documento
resultados_roda01 <- resultados_roda01 %>% mutate(document = "doc1")
resultados_roda02 <- resultados_roda02 %>% mutate(document = "doc2")
resultados_roda03 <- resultados_roda03 %>% mutate(document = "doc3")
resultados_roda04 <- resultados_roda04 %>% mutate(document = "doc4")
resultados_roda05 <- resultados_roda05 %>% mutate(document = "doc5")
resultados_roda06 <- resultados_roda06 %>% mutate(document = "doc6")

# Combinando os dados
tidycomp <- bind_rows(resultados_roda01, resultados_roda02, resultados_roda03,
                      resultados_roda04, resultados_roda05, resultados_roda06)

# Criando a matriz
spars_tidy_dfl <- tidy_dfl %>%
  count(autores, word) %>%
  cast_sparse(autores, word, n)

spars_tidy_dfl

#spars_tidy_dfl <- tidy_dfl %>%
 # count(document, word) %>%
  #cast_sparse(document, word, n)

#spars_tidy_dfl
# Perplexidade para número ideal de tópicos
# Definindo um intervalo de tópicos para testar
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
topic_model <- stm(spars_tidy_dfl, K = 5)

#Verificando o sumário
summary(topic_model)

# Highest sao as palavras mais citadas
# FREX é a medida de exclusividade
# Lift é a associação das palavras com seu tópico
# Score é uma medida ponderada de FREX e Lift

# Adaptar intervalo de topics
score_words <- labelTopics(topic_model, n=20, topics = 1:5)

score_words

# Probabilidade por documento
doc_topic <- tidy(topic_model, matrix = "gamma",document_names = rownames(spars_tidy_dfl))

# Visualizar em tabela ao invés do gráfico
doc_topic_t <- doc_topic %>%
  pivot_wider(names_from = topic, values_from = gamma, values_fill = 0)
print(n = 107, doc_topic_t)

# Gerando variável de classificação de tópico
# Adicionar uma coluna indicando o número do tópico com a maior probabilidade
doc_topic_t2 <- doc_topic_t %>%
  rowwise() %>%
  mutate(topic_number = which.max(c_across(2:6))) #Alterar de acordo com k
print(n = 107, doc_topic_t2)

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

#Gráfico grande é melhor escolher o tamanho para exportar

#Juntando com a base de sentimentos para discursos
dados2 <- cbind (dados, topico = doc_topic_t2$topic_number) 
table(dados2$sentimento, dados2$topico)

#Coerência
# Extraindo as distribuições de palavras por tópico
beta <- tidy(topic_model, matrix = "beta")

# Convertendo beta em uma matriz adequada para textmineR
# Cada linha é um tópico, e cada coluna é uma palavra
beta_matrix <- beta %>%
  pivot_wider(names_from = term, values_from = beta) %>%
  column_to_rownames(var = "topic") %>%
  as.matrix()

# Calculando a coerência dos tópicos
coherence_scores <- CalcProbCoherence(beta_matrix, dtm = spars_tidy_dfl)
coherence_scores

#Aulas

# Adicionando um identificador de documento
resaula01 <- resaula01 %>% mutate(document = "doc1")
resaula02 <- resaula02 %>% mutate(document = "doc2")
resaula03 <- resaula03 %>% mutate(document = "doc3")
resaula04 <- resaula04 %>% mutate(document = "doc4")
resaula05 <- resaula05 %>% mutate(document = "doc5")
resaula06 <- resaula06 %>% mutate(document = "doc6")
resaula07 <- resaula07 %>% mutate(document = "doc7")

# Combinando os dados
tidycomp <- bind_rows(resaula01, resaula02, resaula03,
                      resaula04, resaula05, resaula06, resaula07)
# Voltar para  criação da matriz