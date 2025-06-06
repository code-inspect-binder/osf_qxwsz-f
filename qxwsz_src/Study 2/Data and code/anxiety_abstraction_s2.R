## Load packages
library(tidyverse)
library(readxl)
library(fastmatch)
library(academictwitteR)
library(openxlsx)
library(quanteda)
library(tidytext)
library(tm)
library(qdapRegex)
library(jtools)
library(reshape2)
library(PupillometryR)
library(cowplot)

# Import Brysbaert et al.'s concreteness dictionary
load("brysbaert_dic.RData")

# Get the concreteness dictionary into shape
## Convert all library words into lower case
brysbaert_dic$Word<-tolower(brysbaert_dic$Word)
## Create a vector of the concreteness scores
ConcScores <- brysbaert_dic$Conc.M  
## Provide the word associated with each concreteness score
names(ConcScores) <- brysbaert_dic$Word 

#### DOWNLOAD TWEETS ####

# Fear tweets 
tweets1_new <-
  academictwitteR::get_all_tweets(
    query = c("\"I was scared\"",
              "\"I felt scared\"",
              "\"I feel scared\"",
              "\"I am scared\"",
              "\"I was afraid\"",
              "\"I felt afraid\"",
              "\"I feel afraid\"",
              "\"I am afraid\"",
              "\"I feel fear\"",
              "\"I feel fearful\"",
              "\"I felt fearful\"",
              "\"I felt fear\""),
    exclude = c("\"I was worried\"",
                "\"I felt worried\"",
                "\"I feel worried\"",
                "\"I am worried\"",
                "\"I was anxious\"",
                "\"I felt anxious\"",
                "\"I feel anxious\"",
                "\"I feel anxiety\"",
                "\"I am anxious\"",
                "\"I am apprehensive\"",
                "\"I feel apprehensive\"",
                "\"I felt apprehensive\"",
                "\"I felt apprehension\"",
                "\"I feel apprehension\""),
    exact_phrase = F,
    start_tweets = "2018-01-01T00:00:00Z",
    end_tweets = "2022-10-01T00:00:00Z",
    is_retweet=FALSE,
    #country = "US",
    lang = "en",
    #has_mentions = FALSE,
    #has_images = FALSE,
    n= 10000,
    data_path = "tweetdata_new",
    bind_tweets = FALSE
  )
# Anxiety tweets 
tweets2_new <-
  academictwitteR::get_all_tweets(
    query = c("\"I was worried\"",
              "\"I felt worried\"",
              "\"I feel worried\"",
              "\"I am worried\"",
              "\"I was anxious\"",
              "\"I felt anxious\"",
              "\"I feel anxious\"",
              "\"I feel anxiety\"",
              "\"I am anxious\"",
              "\"I am apprehensive\"",
              "\"I feel apprehensive\"",
              "\"I felt apprehensive\"",
              "\"I felt apprehension\"",
              "\"I feel apprehension\""),
    exclude = c("\"I was scared\"",
                  "\"I felt scared\"",
                  "\"I feel scared\"",
                  "\"I am scared\"",
                  "\"I was afraid\"",
                  "\"I felt afraid\"",
                  "\"I feel afraid\"",
                  "\"I am afraid\"",
                  "\"I feel fear\"",
                  "\"I feel fearful\"",
                  "\"I felt fearful\"",
                  "\"I felt fear\""),
    exact_phrase = F,
    start_tweets = "2018-01-01T00:00:00Z",
    end_tweets = "2022-10-01T00:00:00Z",
    is_retweet=FALSE,
    #country = "US",
    lang = "en",
    # has_mentions = FALSE,
    # has_images = FALSE,
    n= 10000,
    data_path = "tweetdata_new",
    bind_tweets = FALSE,
  )

# Import tweets that have been locally stored as JSON files
tweets1_new <- bind_tweets(data_path = "tweetdata/") # fear tweets
tweets2_new <- bind_tweets(data_path = "anxiety_tweets/") # anxiety tweets

# Extract likes and retweets
## DATASET 1 (fear)
tweets_fear <- tweets1_new$public_metrics %>%
  dplyr::select(likes1=like_count,retweets1=retweet_count)
tweets_fear$text1 <- tweets1_new$text

## DATASET 2 (anxiety)
tweets_anxiety <- tweets2_new$public_metrics %>%
  dplyr::select(likes2=like_count,retweets2=retweet_count)
tweets_anxiety$text2 <- tweets2_new$text

# Merge the two datasets
everything <- gdata::cbindX(tweets_fear, tweets_anxiety)
everything$ID <- c(1:nrow(everything))
everything <- everything %>%
  dplyr::select(ID,likes1,retweets1,text1,likes2,retweets2,text2)

# We saved the dataframe as an Excel file and manually combine columns
openxlsx::write.xlsx(everything,"twitter_fearVSanxiety.xlsx")
# Import manually reshaped datafile
combined_twitter_dataset <- read_xlsx("twitter_fearVSanxiety.xlsx")

#### Brysbaert concreteness index (code by Johnson-Grey et al., 2020) ####

# NOTE! Code takes a long time to run

# First, create the "keep.words" function
keep.words <- function(text, keep) {
  require(tm)
  text <- as.character(text)
  text <- iconv(text, 'UTF-8', 'ASCII')
  text[sapply(text, is.character)] <- lapply(text[sapply(text, is.character)], tolower) 
  text <- as.character(text)
  text<-removePunctuation(text)
  keepwords.output <- as.data.frame(matrix(NA,
                                           ncol = 1,
                                           nrow = length(text)))
  colnames(keepwords.output) <- c("newtext")
  for (i in 1:length(text)){
    if (i %% 100 == 0){
      print(paste0(i," done"))
    }
    words <- strsplit(text[i], " ")[[1]]
    keepwords.output$newtext[i] <- paste(words[words %in% keep],
                                         collapse = " ")
  }
  keepwords.output <<- keepwords.output
}

# Create the "Brysbaert-calculator" function
brysbaert.calculator <- function(text, keep){
  require(quanteda)
  text <- as.character(text)
  brys.output <- as.data.frame(matrix(NA,
                                      ncol = 3,
                                      nrow = length(text)))
  colnames(brys.output) <- c("newtext", "wc", "bryscore")
  brys.output$bryscore <- 0
  for (i in 1:length(text)){
    if (i %% 100 == 0){
      print(paste0(i," done"))
    }
    words <- strsplit(text[i], " ")[[1]]
    words <- sapply(words, tolower)
    brys.output$newtext[i] <- paste(words[words %in% keep],
                                    collapse = " ")
    words <- strsplit(brys.output$newtext[i], " ")[[1]]
    for (w in words){
      index <- which(!is.na(match(brysbaert_dic$Word, w)))
      weight <- brysbaert_dic$Conc.M[index]
      brys.output$bryscore[i] <- brys.output$bryscore[i] + weight
    }
  }
  bryscorpus <- corpus(brys.output$newtext)
  brysdfm <- dfm(bryscorpus)
  brys.output$wc <- rowSums(brysdfm)
  brys.output$bryscore <- brys.output$bryscore / brys.output$wc
  brys.output <<- brys.output
}

# Analyze the text
keep.words(combined_twitter_dataset$text, brysbaert_dic$Word) # Change text to the name of the column that contains your text data to be analyzed

brysbaert.calculator(keepwords.output$newtext, brysbaert_dic$Word)

df = cbind(combined_twitter_dataset, brys.output)

remove(brys.output)
remove(keepwords.output)

# Save file and calculate scope variables using the LIWC software
combined_twitter_dataset$subject <- c(1:nrow(df))
combined_twitter_dataset %>% 
  dplyr::select(subject,fearVSanx,text,concreteness,likes,retweets) %>% 
  write.xlsx(., file = "twitter_fearVSanx_BCI.xlsx", row.names = FALSE)

#### ANALYSIS ####

# Import data that also includes scope variables from LIWC
final_twitter_data <- read.csv("anxiety_abstraction_s2.csv",head = TRUE, sep=";")

## Save all variables as numeric
final_twitter_data <- final_twitter_data %>% mutate_each(funs(as.numeric))

# Only include tweets with at least 3 words
final_twitter_data_ex <- final_twitter_data %>% filter(wc>=3)
final_twitter_data_ex <- final_twitter_data_ex %>% filter(bryscore>=0.0001)

# Correlations
descriptive_variables <- select(final_twitter_data_ex,
                                fearVSanx,
                                bryscore,
                                i,
                                we,
                                they,
                                focusfuture
                                )

# Plot correlations
psych::corPlot(descriptive_variables,
               alpha=.05,
               stars=TRUE,
               diag=FALSE,
               upper=FALSE)

# Run code chunk below to create correlation table
  apaTables::apa.cor.table(
    descriptive_variables,
  #filename = "study2_corrtable1.doc",
  show.conf.interval = TRUE,
  show.sig.stars = TRUE,
  landscape = TRUE
)

# Factorize IV (0 = fear-related phrases in tweets, 1 = anxiety-related phrases)
final_twitter_data_ex$fearVSanx <- final_twitter_data_ex$fearVSanx %>%
  factor(levels = c(0,1), labels = c("Fear tweets","Anxiety tweets"))

# Means and SDs in fear-tweets and anxiety-tweets
final_twitter_data_ex %>%
  group_by(fearVSanx) %>%
  summarise_at(vars(bryscore,i,we,they,focusfuture),
               list(mean=mean, sd=sd))

# Independent samples t-test: Brysbaert concreteness score as DV
t.test_bci <- final_twitter_data_ex %>%
  rstatix::t_test(bryscore ~ fearVSanx,
                  var.equal = TRUE,
                  detailed=TRUE) %>% 
  rstatix::add_significance()

t.test_bci

## effect size and CIs
psych::cohen.d(final_twitter_data_ex$bryscore,
               final_twitter_data_ex$fearVSanx,
               alpha=.05,
               std=TRUE,
               sort=NULL,
               dictionary=NULL,
               MD=TRUE,
               data=NULL)

# Independent samples t-test: percentage of singular first-person pronoun "I"  as DV
t.test_i <- final_twitter_data_ex %>%
  rstatix::t_test(i ~ fearVSanx,
                  var.equal = TRUE,
                  detailed=TRUE) %>% 
  rstatix::add_significance()

t.test_i

## effect size and CIs
psych::cohen.d(final_twitter_data_ex$i,
               final_twitter_data_ex$fearVSanx,
               alpha=.05,
               std=TRUE,
               sort=NULL,
               dictionary=NULL,
               MD=TRUE,
               data=NULL)

# Independent samples t-test: percentage of plural first-person pronoun "We" as DV
t.test_we <- final_twitter_data_ex %>%
  rstatix::t_test(we ~ fearVSanx,
                  var.equal = TRUE,
                  detailed=TRUE) %>% 
  rstatix::add_significance()

t.test_we

### effect size and CIs 
psych::cohen.d(final_twitter_data_ex$we,
               final_twitter_data_ex$fearVSanx,
               alpha=.05,
               std=TRUE,
               sort=NULL,
               dictionary=NULL,
               MD=TRUE,
               data=NULL)

# Independent samples t-test: percentage of third-person pronoun "They" as DV
t.test_they <- final_twitter_data_ex %>%
  rstatix::t_test(they ~ fearVSanx,
                  var.equal = TRUE,
                  detailed=TRUE) %>% 
  rstatix::add_significance()

t.test_they

## effect size and CIs
psych::cohen.d(final_twitter_data_ex$they,
               final_twitter_data_ex$fearVSanx,
               alpha=.05,
               std=TRUE,
               sort=NULL,
               dictionary=NULL,
               MD=TRUE,
               data=NULL)

# Independent samples t-test with percentage of future-related words as DV
t.test_future <- final_twitter_data_ex %>%
  rstatix::t_test(focusfuture ~ fearVSanx,
                  var.equal = TRUE,
                  detailed=TRUE) %>% 
  rstatix::add_significance()

t.test_future

## effect size and CIs
psych::cohen.d(final_twitter_data_ex$focusfuture,
               final_twitter_data_ex$fearVSanx,
               alpha=.05,
               std=TRUE,
               sort=NULL,
               dictionary=NULL,
               MD=TRUE,
               data=NULL)
