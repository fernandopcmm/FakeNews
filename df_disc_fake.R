## Tinst

#pdftools
library(pdftools)
diretorio <- "/Users/fernandolhamas/Documents/FakeNews/notas_discursos_sessoes"
arquivos_pdf <- list.files(diretorio, pattern = ".pdf$", full.names = T)

#Extrair o texto de cada um deles
texto_pdf <- lapply(arquivos_pdf, pdf_text)

#Montando a base de dados

#Variável 1: id
num_artigos <- length(arquivos_pdf)
id <- paste0 ("art", 1:num_artigos)

#Variável 2: autor e ano extraídos do título dos arquivos, sem extensão do arquivo
autores <- tools::file_path_sans_ext(basename(arquivos_pdf))

#Juntando em um dataframe
#Cada item da lista é um var do tipo chr com tamanhos distintos. cbind ou dataframe n funciona

#Criando dataframe vazio
dados_artigos <- data.frame(id = character(length(texto_pdf)), autores = character(length(texto_pdf)), texto = character(length(texto_pdf)), stringsAsFactors = FALSE)

#Criando um laço para associar cada artigo com sua id e autores
for (i in 1:length(texto_pdf)) {
  dados_artigos[i, "id"] <- id[i]
  dados_artigos[i, "autores"] <- autores[i]
  dados_artigos[i, "texto"] <- paste(texto_pdf[[i]], collapse = " ")
}
str(dados_artigos)


