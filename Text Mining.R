" Title:
  A2_Group Project 
  Script created by:
  Group 8 

MsBA I - HULT IBS 
  for the NLP class by Prof. Thomas Kurnicki

Date: 
14th of Feb. 2021

Details:
  script for the analyisis of the book: 
  Analyzization of self created and sourced data 
Note the usage of the script is for the NLP class at HULT only.

"
# calling of all necessary libraries & checking if they run for this R version
library (ggplot2) # for plotting  findings 
library(scales) # for correlogram 
library(stringr)
library (gutenbergr) # to download the text 
library (dplyr) # the gramma for data manipulation 
library(tidytext) # to analyze our tidied text 
library(tidyr) # to manipulate tidied data 
library(tidytext)
library(tidyverse)
library(textshape)
library (RColorBrewer) # for the word cloud 
library(wordcloud) # to create nice word clouds 
library(igraph) # for the ngram analysis 
library (tm) # to remove numeric values 
# library(textreadr)
data(stop_words) # calling the library to remove stop words later 

################################################################################
################################################################################
#INTRO 
################################################################################
# we have pre-cleaned the data and here is 

# READING TEXT 


library(textreadr)
#Importing all .txt files from one directory 
# a txt works like a csv file with multiple rows
setwd("/Users/Alejandra 1/Downloads/all cleaned otter") # point of dictionary to your computer 
all_otter_files <- list.files(path="/Users/Alejandra 1/Downloads/all cleaned otter")
#using read document to import the data:
my_data <- read_document(file="/Users/Alejandra 1/Downloads/all cleaned otter") 
#This comes out as a vector
my_data_together <- paste(my_data, collapse = " ") # This will give us a concatenated vector

my_otter_text <- do.call(rbind, lapply(all_otter_files, function(x)
  paste(read_document(file=x), collapse = " ")))
print(all_otter_files) # all document 
print (my_otter_text) # raw text 

### converting into dataframe 

#install.packages("dplyr")
library(dplyr)
my_otter <- data.frame(line= 1:43, text=my_otter_text) # location information = my_txt 
print(my_otter)



############


# TIDYING DATA 
#tiding the books 
my_tidy_otter <- my_otter %>% # with piping I tidy the data set  
  unnest_tokens(word, text) %>% # tokenization of the book s
  anti_join(stop_words)# get rid of stop words
print(my_tidy_otter)



#counting frequencies for tokens (in this case words that are NO stop words)
print ("Otter_Movies: Frequency of words")
my_tidy_otter %>%
  count(word, sort=TRUE)


# Otter_movies: frequency histogram 
freq_hist<- my_otter %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  count (word, sort=TRUE) %>%
  mutate(word=reorder(word, n)) %>%
  filter(n >= 5) %>% # n >= 2
  ggplot(aes(word, n))+
  geom_col(fill = "deepskyblue")+
  ylab("Count of Words")+
  xlab ("Words")+
  theme_classic()+
  ggtitle("Otter_Movies: Most Frequent Words") +
  coord_flip()
print(freq_hist)




################################################################################
# SENTIMENT ANALYSIS 
# install.packages("textdata")
library (textdata)
sent_bing<- get_sentiments ("bing") # online positive or negative 



# getting the sentiment 
my_bing_otter <- 
  my_tidy_otter %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()

# plotting the sentiment 
my_bing_otter %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) + 
  scale_fill_manual(values = c("tomato", "olivedrab3")) +
  geom_col(show.legend = FALSE) + 
  theme_minimal()+
  facet_wrap(~sentiment, scales = "free_y") + 
  ggtitle("Otter Movies: Sentiment Analysis\nWith the bing dictionary") +
  labs(y = "Contribution to sentiment",
       x="Negative (red) -- Postive (green)")+ coord_flip()



################################################################################
# WORD CLOUD 


# word cloud  smith 
my_tidy_otter %>%
  count(word) %>% 
  with(wordcloud(word, n, max.words= 50, colors = brewer.pal(6, "Blues")))










################################################################################
#############################################
###### N-grams and tokenizing ###############
#############################################

library(dplyr)
library(tidytext)
library(tidyr)

otter_bigrams <- my_otter %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) # bigrams -> n=2

otter_bigrams #We want to see the bigrams (words that appear together, "pairs")

# Bigrams are overlapping together (overlap in e.g. "sense and", "and sensibility",
# duplicated amount of words)

otter_bigrams %>%
  count(line, bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- otter_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>% #filter OUT(!) words for which first token is a stop_word
  filter(!word2 %in% stop_words$word) #filter OUT(!) words for which second token is a stop_word

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE) # specify tw tokens bc we are studying combos of tokens
#want to see the new bigrams
bigram_counts

###########################################################
###### What if we are interested in the most common #######
################ 4 consecutive words - quadro-gram ########
###########################################################

# filtering out NAs as well:
quadrogram <- my_otter %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n= 4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
  filter(!word1 == "NA") %>%
  filter(!word2 == "NA") %>%
  filter(!word3 == "NA") %>%
  filter(!word4 == "NA") 

quadrogram

###########################################################
###### We can also apply the tf_idf framework  ############
########### on our bigram and quadro-gram #################
###########################################################

bigram_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

bigram_tf_idf <- bigram_united %>%
  count(line, bigram) %>%
  bind_tf_idf(bigram, line, n) %>%
  ungroup() %>%
  arrange(desc(tf_idf))
  
bigram_tf_idf


######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

# install.packages("igraph")
library(igraph)
bigram_graph <- bigram_counts %>%
  filter(n>1) %>% #filtering for those that are more frequent
  graph_from_data_frame()

bigram_graph


# install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph, layout = "fr") + #layout = frequency
  geom_edge_link()+ #linking bigrams together
  theme_void() +
  geom_node_point(color = "deepskyblue2", size = 2) +
  geom_node_text(aes(label=name), vjust =1, hjust=1, color= "deepskyblue4") #label = name of our bigram

################################################################################
################################################################################
# NAIVE BAYES CLASSIFICATION ANALYSIS
################################################################################
# loading libarries
library(quanteda.textmodels)
library(RColorBrewer)
library(ggplot2)
library(quanteda)

msg.dfm <- dfm(corpus(my_otter), tolower = TRUE) #generating document 
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 2, min_docfreq = 1)
msg.dfm <- dfm_weight(msg.dfm)

head(msg.dfm)
#let's split the docs into training and testing data
msg.dfm.train<-msg.dfm[1:30,]
msg.dfm.test<-msg.dfm[31:43,]

#building the Naive Bayes model:
NB_classifier <- textmodel_nb(msg.dfm.train, c(1,1,1,0,0,1,0,1,1,1,
                                                 0,0,1,0,0,1,1,1,0,1,
                                                 1,1,1,1,1,1,0,1,1,1))
NB_classifier
summary(NB_classifier)

# predicting the testing data
pred <- predict(NB_classifier, msg.dfm.test)
pred

################################################################################
################################################################################
# SHINY APP DEVELOPMENT
################################################################################
# importing libraries
library(shiny)


################################################################################
### USER INTERFACE CODE (IU)

ui <- fluidPage(
  titlePanel("Likelihood of Watching Movie Tonight"),
  # sidebarLayout(
  #     sidebarPanel(
  #         selectInput('sex', 'Select Sex', c("M", "F")), # EXAMPLE of reactive source:
  #                                                                  # expression
  #         sliderInput('year', 'Select Year', min = 1880, max = 2013, value = 1900)
  #     ),
  mainPanel(
    tabsetPanel(
      tabPanel('Frequent Words', plotOutput('word_freq')),
      tabPanel('Sentiment Analysis', plotOutput('sentiment')),
      tabPanel('Bigrams', plotOutput('bigrams'))
    )
  )
)


################################################################################
### SERVER CODE

# importing libraries
library(shiny)

server <- function(input, output, session) {
  
  # loading libraries
  library (ggplot2) # for plotting  findings 
  library(scales) # for correlogram 
  library(stringr)
  library (gutenbergr) # to download the text 
  library (dplyr) # the gramma for data manipulation 
  library(tidytext) # to analyze our tidied text 
  library(tidyr) # to manipulate tidied data 
  library(tidytext)
  library(tidyverse)
  library(textshape)
  library (RColorBrewer) # for the word cloud 
  library(wordcloud) # to create nice word clouds 
  library(igraph) # for the ngram analysis 
  library (tm) # to remove numeric values 
  # library(textreadr)
  data(stop_words) # calling the library to remove stop words later 
  
  # importing dataset
  library(textreadr)
  setwd("/Users/Alejandra 1/Downloads/all cleaned otter") # point of dictionary to your computer 
  all_otter_files <- list.files(path="/Users/Alejandra 1/Downloads/all cleaned otter")
  my_data <- read_document(file="/Users/Alejandra 1/Downloads/all cleaned otter") 
  my_data_together <- paste(my_data, collapse = " ")
  my_otter_text <- do.call(rbind, lapply(all_otter_files, function(x)
    paste(read_document(file=x), collapse = " ")))
  library(dplyr)
  my_otter <- data.frame(line= 1:43, text=my_otter_text)
  
  ### adding new column to dataset for binary classification output?
  
  ### GRAPH OUTPUTS
  ## word frequency histogram
  output$word_freq <- renderPlot({
    # plotting AND creating bar chart
    freq_hist<- my_otter %>%
      unnest_tokens(word,text) %>%
      anti_join(stop_words) %>%
      count (word, sort=TRUE) %>%
      mutate(word=reorder(word, n)) %>%
      filter(n >= 5) %>% # n >= 2
      ggplot(aes(word, n))+
      geom_col(fill = "deepskyblue")+
      ylab("Count of Words")+
      xlab ("Words")+
      theme_classic()+
      ggtitle("Otter_Movies: Most Frequent Words") +
      coord_flip()
    print(freq_hist)
  })
  
  ## sentiment analysis bar charts
  output$sentiment <- renderPlot({
    # plotting AND creating bar charts
    # getting the sentiment 
    my_bing_otter <- 
      my_tidy_otter %>% 
      inner_join(get_sentiments("bing")) %>% 
      count(word, sentiment, sort = TRUE) %>% 
      ungroup()
    # plotting the sentiment 
    my_bing_otter %>%
      group_by(sentiment) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>% 
      ggplot(aes(word, n, fill = sentiment)) + 
      scale_fill_manual(values = c("tomato", "olivedrab3")) +
      geom_col(show.legend = FALSE) + 
      theme_minimal()+
      facet_wrap(~sentiment, scales = "free_y") + 
      ggtitle("Otter Movies: Sentiment Analysis\nWith the bing dictionary") +
      labs(y = "Contribution to sentiment",
           x="Negative (red) -- Postive (green)")+ coord_flip()
  })
  
  ## bigrams
  output$bigrams <- renderPlot({
    # creting bigrams
    library(dplyr)
    library(tidytext)
    library(tidyr)
    otter_bigrams <- my_otter %>%
      unnest_tokens(bigram, text, token = "ngrams", n=2)
    library(tidyr)
    bigrams_separated <- otter_bigrams %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    bigrams_filtered <- bigrams_separated %>%
      filter(!word1 %in% stop_words$word) %>% #filter OUT(!) words for which first token is a stop_word
      filter(!word2 %in% stop_words$word)
    bigram_counts <- bigrams_filtered %>%
      count(word1, word2, sort = TRUE)
    # plotting bigram network
    # install.packages("igraph")
    library(igraph)
    bigram_graph <- bigram_counts %>%
      filter(n>1) %>% #filtering for those that are more frequent
      graph_from_data_frame()
    # install.packages("ggraph")
    library(ggraph)
    ggraph(bigram_graph, layout = "fr") + #layout = frequency
      geom_edge_link()+ #linking bigrams together
      theme_void() +
      geom_node_point(color = "deepskyblue2", size = 2) +
      geom_node_text(aes(label=name), vjust =1, hjust=1, color= "deepskyblue4") #label = name of our bigram
    
  })
}

shinyApp(ui = ui, server = server)

  