install.packages("plotly")
library(readxl)
library(sentiment)
library(devtools)
install.packages("Rstem", repos = "http://www.omegahat.org/R")
install.packages("Rstem_0.4-1.tar.gz" , repos=NULL, type="source")
install.packages("devtools")
library("devtools")
if(!require(Rstem)) install_url("http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
if(!require(sentiment)) install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")





library(readxl)
library(sentiment)
library(plotly)

df <- read_excel(file.choose(),sheet = 1)

f_clean_tweets <- function (tweets) {
  
  clean_tweets <- tweets
  # remove retweet entities
  clean_tweets <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', clean_tweets)
  # remove @people
  clean_tweets <- gsub('@\\w+', '', clean_tweets)
  # Removal of Hashtags : 
  clean_tweets <- gsub("#\\w+","",clean_tweets)
  # remove punctuation
  clean_tweets <- gsub('[[:punct:]]', '', clean_tweets)
  # remove numbers
  clean_tweets <-  gsub('[[:digit:]]', '', clean_tweets)
  # remove html links
  clean_tweets <- gsub('(f|ht)(tp)(s?)(://)(.*)[.|/](.*)', '', clean_tweets)
  # Removal of all except letters
  clean_tweets <- gsub("[^a-zA-Z]"," ",clean_tweets)
  # remove unnecessary spaces
  clean_tweets <- gsub('[ \t]{2,}', ' ', clean_tweets)
  clean_tweets <- gsub('^\\s+|\\s+$', '', clean_tweets)
  # ToLower
  clean_tweets <- tolower(clean_tweets)
  
  #Return Clean Tweets
  clean_tweets
}

clean_tweets <- f_clean_tweets(df$Message)

# removing duplicates due to retweets
clean_tweets <- clean_tweets[!duplicated(clean_tweets)]

emotions <- classify_emotion(clean_tweets, algorithm='bayes')

# using sentiment package to classify polarities
polarities = classify_polarity(clean_tweets, algorithm='bayes')

Data <-  data.frame(text=clean_tweets, emotion=emotions[,'BEST_FIT'],stringsAsFactors=FALSE)
df[is.na(df)] <- "N.A."

length(clean_tweets)

plot_ly(Data, x=~emotion,type="histogram",
        marker = list(color = c('grey', 'red',
                                'orange', 'navy',
                                'yellow'))) %>%
  layout(yaxis = list(title='Count'), title="Sentiment Analysis: Emotions")
plot_ly(Data, x=~polarity, type="histogram",
        marker = list(color = c('magenta', 'gold',
                                'lightblue'))) %>%
  layout(yaxis = list(title='Count'), title="Sentiment Analysis: Polarity")


Data$text[76]
