library(plyr)
library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(networkD3)
library(widyr) #may need to install from github
library(DT)
library(readr)
#Read in csv files:

df <- read_csv("")

#Example of some basic string cleaning steps
df$Discharge.Diagnosis <- gsub(";", " ",df$Discharge.Diagnosis)
df$CCDD <- gsub(";", " ",df$CCDD)
df$CCDD <- toupper(df$CCDD)
df$CCDD <- gsub("-", " ",df$CCDD)
df$CCDD <- gsub("/", " ",df$CCDD)
df$CCDD <- gsub("<BR>", " ",df$CCDD)

data("stop_words") 
tokenize_df <- df %>% mutate(linenumber = row_number())
tokenize_df <- tokenize_df %>% unnest_tokens(word, ChiefComplaintParsed)
tokenize_df <- tokenize_df %>% anti_join(stop_words)

#Example of changing spelling of common terms - can adapt to use the stringr package too
tokenize_df$word <- gsub("coughing", "cough", tokenize_df$word, ignore.case = TRUE, perl = TRUE)
tokenize_df$word <- gsub("symptoms", "symptom", tokenize_df$word, ignore.case = TRUE)
tokenize_df$word <- gsub("feverm", "fever", tokenize_df$word, ignore.case = TRUE)
tokenize_df$word <- gsub("fevers", "fever", tokenize_df$word, ignore.case = TRUE)
tokenize_df$word <- gsub("sorethroat", "sore throat", tokenize_df$word, ignore.case = TRUE)
tokenize_df$word <- gsub("headaches", "headache", tokenize_df$word, ignore.case = TRUE)
tokenize_df$word <- gsub("vaccination", "vaccine", tokenize_df$word, ignore.case = TRUE)
tokenize_df$word <- gsub("rabbies", "rabies", tokenize_df$word, ignore.case = TRUE)

#Count the term frequencies
tokenize_df %>% count(word,sort=TRUE) %>% print(n=50)

#Create ngrams, or frequencies of multiple terms together
ngrams <- df %>% unnest_tokens(ngram, ChiefComplaintParsed, token = "ngrams", n = 4) %>% count(ngram, sort=TRUE)
table <- ngrams %>% filter(n > 5) %>% datatable(ngrams)
table

#Create Word relationships and produce plot
word_cooccurences <- tokenize_df %>% pairwise_count(word,linenumber, sort=TRUE, upper = FALSE)
set.seed(1254)
word_cooccurences %>%
  filter(n >= 50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 2.5) +
  theme_void()


#Search for particular strings from a query
filter(tokenize_df, grepl("her", word, ignore.case=TRUE)) %>% count(word,sort=TRUE) %>% print(n=50)


#^ = matches start of string
#$ = matches end of string

