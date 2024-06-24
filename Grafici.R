install.packages("igraph")
install.packages("ggplot2")
install.packages("quanteda")
install.packages("rjson")
install.packages("quanteda.textplots")
install.packages("quanteda.textstats")


# url for BuzzFeed (num_news = 91):
#   - Data\\BuzzFeed\\BuzzFeedNewsUser.csv
#   - Data\\BuzzFeed\\BuzzFeedUserUser.csv
#   - Data\\BuzzFeed\\FakeNewsContent\\
#   - Data\\BuzzFeed\\RealNewsContent\\

# url for PolitiFact (num_news = 120):
#   - Data\\PolitiFact\\PolitiFactNewsUser.csv
#   - Data\\PolitiFact\\PolitiFactUserUser.csv
#   - Data\\PolitiFact\\FakeNewsContent\\
#   - Data\\PolitiFact\\RealNewsContent\\


library(igraph)
library(ggplot2)

num_news = 91
df  <- read.csv("Data\\BuzzFeed\\BuzzFeedNewsUser.csv", sep=",", header = TRUE)

#Grafico a torta del numero delle iterazioni di notizie vere e false

numero_iterazioni_real = 0
numero_iterazioni_fake = 0

for (i in 1:nrow(df)) {
  
  if(df$news[i]<=num_news){
    numero_iterazioni_real = numero_iterazioni_real + df$time[i]
  }
  else{
    numero_iterazioni_fake = numero_iterazioni_fake + df$time[i]
  }
  
}
numero_iterazioni_totali = numero_iterazioni_fake + numero_iterazioni_real
numero_iterazioni_totali
valoriG <- c(numero_iterazioni_real, numero_iterazioni_fake)
etichette <- c("Real news", "Fake news")
colori <- c("green", "red")
percentuali <- round(valoriG / sum(valoriG) * 100, 1)
pie(valoriG, labels = paste(etichette, percentuali), col = colori)


#Grafico a torta del numero di iterazioni di utenti univoci di notizie vere e false

numero_iterazioni_real = 0
numero_iterazioni_fake = 0

for (i in 1:nrow(df)) {
  
  if(df$news[i]<=num_news){
    numero_iterazioni_real = numero_iterazioni_real + 1
  }
  else{
    numero_iterazioni_fake = numero_iterazioni_fake + 1
  }
  
}

valoriG <- c(numero_iterazioni_real, numero_iterazioni_fake)
etichette <- c("Real news", "Fake news")
percentuali <- round(valoriG / sum(valoriG) * 100, 1)
pie(valoriG, labels = paste(etichette, percentuali), col = colori)


# grafo di engagement tra real e fake con archi utenti

edges <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(edges) = c("From", "To")

for (i in 2:nrow(df)) {
  j = i
  codice_utente_i = df$user[i-1]
  codice_utente_j = df$user[j]
  
  while(codice_utente_i == codice_utente_j){
    newedge = data.frame(From = df$news[i-1] , To = df$news[j])
    edges = rbind(edges, newedge)
    j = j + 1
    codice_utente_j = df$user[j]
  }
}

G <- graph.edgelist(as.matrix(edges), directed=FALSE)

set.seed(1)
lout <- layout.fruchterman.reingold(G)
V(G)$label <- NA
all_colors <- rep("green", vcount(G))
all_colors[(num_news+1):(num_news*2)] <- "red"
V(G)$color <- all_colors

#elimono i nodi isolati
Isolated <- which(degree(G) == 0)
G <- delete.vertices(G, Isolated)

plot.igraph(G, layout=lout, vertex.size=8)
legend("bottomleft", legend = c("Real News", "Fake News"), col = c("green", "red"), pch = 16)


# grafico affidabilità ad U asse y-> percentuale utenti asse x->da real a fake

affidabilità = data.frame(matrix(ncol = 2, nrow = 0))
colnames(affidabilità) = c("User", "Affidability")

for (i in 1:max(df$user)) {
  num_tot <- sum(df$user == i)
  num_real <- sum(df$user == i & df$news <= num_news)
  val = num_real/num_tot
  newrow = data.frame(User = i , Affidability = val)
  affidabilità = rbind(affidabilità, newrow)
}

ggplot(affidabilità, aes(x = Affidability)) + 
  geom_density(fill="gray") + 
  scale_x_continuous(limits = c(0,1), 
                     breaks=c(0,1),
                     labels=c("Fake","Real")) +
  labs(x = "affidabilità") +
  theme_bw()


#grafico word frequency

library(quanteda)
library(rjson)
library(quanteda.textstats)
library(ggplot2)
library(quanteda.textplots)

file_paths <- list.files(path = "Data\\BuzzFeed\\FakeNewsContent\\", pattern = "\\.json$", full.names = TRUE)

dfwords <- data.frame()

for (file in file_paths) {
  json_data <- jsonlite::fromJSON(file)
  dfwords <- rbind(dfwords, data.frame(text = json_data$text))
}


corpus = corpus(dfwords)
doc.tokens = tokens(corpus)
doc.tokens = tokens(doc.tokens, remove_punct = TRUE, remove_numbers = TRUE)
doc.tokens = tokens_select(doc.tokens, stopwords(language = "en", source = "snowball", simplify = TRUE), selection ='remove')
doc.tokens = tokens_tolower(doc.tokens)
dfmat1 = dfm(doc.tokens) %>% dfm_trim(min_termfreq = 30)
features_dfm = textstat_frequency(dfmat1, n = 30)
features_dfm$feature = with(features_dfm, reorder(feature, -frequency))

ggplot(features_dfm, aes(x = feature, y = frequency)) +
  geom_point() + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

textplot_wordcloud(dfmat1)


file_paths <- list.files(path = "Data\\BuzzFeed\\RealNewsContent\\", pattern = "\\.json$", full.names = TRUE)

dfwords <- data.frame()

for (file in file_paths) {
  json_data <- jsonlite::fromJSON(file)
  dfwords <- rbind(dfwords, data.frame(text = json_data$text))
}


corpus = corpus(dfwords)
doc.tokens = tokens(corpus)
doc.tokens = tokens(doc.tokens, remove_punct = TRUE, remove_numbers = TRUE)
doc.tokens = tokens_select(doc.tokens, stopwords(language = "en", source = "snowball", simplify = TRUE), selection ='remove')
doc.tokens = tokens_tolower(doc.tokens)
dfmat1 = dfm(doc.tokens) %>% dfm_trim(min_termfreq = 30)
features_dfm = textstat_frequency(dfmat1, n = 30)
features_dfm$feature = with(features_dfm, reorder(feature, -frequency))

ggplot(features_dfm, aes(x = feature, y = frequency)) +
  geom_point() + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

textplot_wordcloud(dfmat1)





