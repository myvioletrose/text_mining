packages <- c("RCurl", "XML", "dplyr", "ggplot2", "purrr", "rvest", "tm", "tidytext", "quanteda", "tidyverse", "sqldf", "XML", "readxl", "wordcloud", "lubridate")
sapply(packages, library, character.only = T)


#######################################################
################# tutorial on twitteR #################
# https://www.youtube.com/watch?v=QETCjkQ3CBw
# https://davetang.org/muse/2013/04/06/using-the-r_twitter-package/
# http://varianceexplained.org/r/bb-tweets/

#############################################################################################
## register API using Twitter account
# https://apps.twitter.com

## find this info under "Keys and Access Tokens"
# source the api_key, api_secret, access_token & access_token_secret from a different file 
source("api.R")

## load library
if(!require(twitteR)){install.packages("twitteR"); require(twitteR)}
if(!require(RColorBrewer)){install.packages("RColorBrewer"); require(RColorBrewer)}
if(!require(wordcloud)){install.packages("wordcloud"); require(wordcloud)}
if(!require(tm)){install.packages("tm"); require(tm)}

# # necessary file for Windows!?
# download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
# the cainfo parameter is necessary only on Windows
# r_stats <- searchTwitter("#Rstats", n=1500, cainfo="cacert.pem")

## set up oauth
## get tweets from today
tweets <- searchTwitter("bloomberg + fake news", n = 10000, lang = "en", 
                        since = as.character(Sys.Date()-2))  # past 48 hours

## save text
tweets_text <- sapply(tweets, function(x) x$getText())
class(tweets_text)  # [1] "character"
str(tweets_text)  # chr [1:1000] "The 2018 #muniland hangover remains a rough one. 1.5% loss so far, via @amanda_albright https://t.co/sNEag6jtgA" ...

## create corpus
tweets_text_corpus <- Corpus(VectorSource(tweets_text))
inspect(tweets_text_corpus[[1]])

## convert it into dataframe
tweetsDF <- twListToDF(tweets)
class(tweetsDF)  # [1] "data.frame"
str(tweetsDF)
write.table(tweetsDF, file = "tweetsDF.txt", row.names = F, sep = "\t", append = T)

# #if you get the below error
# #In mclapply(content(x), FUN, ...) :
# #  all scheduled cores encountered errors in user code
# #add mc.cores=1 into each function
# 
# #run this step if you get the error:
# #(please break it!)' in 'utf8towcs'
# r_stats_text_corpus <- tm_map(r_stats_text_corpus,
#                               content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
#                               mc.cores=1
# )
# r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower), mc.cores=1)
# r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation, mc.cores=1)
# r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()), mc.cores=1)
# wordcloud(r_stats_text_corpus)

## clean up
tweets_text_corpus <- tm_map(tweets_text_corpus, removePunctuation)
tweets_text_corpus <- tm_map(tweets_text_corpus, content_transformer(tolower)) 
tweets_text_corpus <- tm_map(tweets_text_corpus, removeNumbers)
tweets_text_corpus <- tm_map(tweets_text_corpus, stripWhitespace)
tweets_text_corpus <- tm_map(tweets_text_corpus, function(x) iconv(x, "latin1", "ASCII", ""))
# tweets_text_corpus <- tm_map(tweets_text_corpus, removeWords, stopwords("english"))
tweets_text_corpus <- tm_map(tweets_text_corpus, function(x) removeWords(x, stopwords()))

## an alternative of cleaning up
# data(crude)
# crude[[1]]
# skipWords <- function(x) removeWords(x, c("it", "the"))
# funs <- list(stripWhitespace,
#              skipWords,
#              removePunctuation,
#              content_transformer(tolower))
# tm_map(crude, FUN = tm_reduce, tmFuns = funs)[[1]]

## wordcloud visualization
windows()
set.seed(1234)
wordcloud(tweets_text_corpus, min.freq = 2, colors = brewer.pal(8, "RdBu"))

################################################################################
## trend locations
trend <- availableTrendLocations()  # find trends by location
head(trend)  # woeid stands for where on earth id
#        name country woeid
# 1 Worldwide             1
# 2  Winnipeg  Canada  2972
# 3    Ottawa  Canada  3369
# 4    Quebec  Canada  3444
# 5  Montreal  Canada  3534
# 6   Toronto  Canada  4118

## get trends
world <- getTrends(woeid = 1)




#######################################################################
######################################################################
#####################################################################
# Text analysis of Trump's tweets confirms he writes only the (angrier) Android half
# http://varianceexplained.org/r/trump-tweets/

if(!require(purrr)){install.packages("purrr"); require(purrr)}
if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}
if(!require(tidyr)){install.packages("tidyr"); require(tidyr)}

###############################################################################
####################### about user timeline #######################
# The Twitter API will only return tweets from the past week or so. See the documentation.
# "The Search API is not complete index of all Tweets, but instead an index of recent Tweets. At the moment that index includes between 6-9 days of Tweets."
###############################################################################

################ get user timeline ################
bb_tweets <- userTimeline("Bloomberg", n = 3200)  # max n is 3200
bb_tweets_df <- dplyr::tbl_df(
        purrr::map_df(bb_tweets, as.data.frame)
)
dim(bb_tweets_df)

# functions to manage Twitter users
# getUser("realDonaldTrump")
# lookupUsers("realDonaldTrump")


############# extract the source application, i.e. iPhone vs. Android #############
tweets <- bb_tweets_df %>%
        dplyr::select(id, statusSource, text, created) %>%
        tidyr::extract(statusSource, "source", "Twitter for (.*?)<") %>%
        dplyr::filter(source %in% c("iPhone", "Android"))


#################### tweeting by hours of day ####################
if(!require(lubridate)){install.packages("lubridate"); require(lubridate)}
if(!require(scales)){install.packages("scales"); require(scales)}
if(!require(ggplot2)){install.packages("ggplot2"); require(ggplot2)}

windows()
tweets %>%
        count(source, hour = hour(with_tz(created, "EST"))) %>%
        mutate(percent = n / sum(n)) %>%
        ggplot(aes(hour, percent, color = source)) +
        geom_line() +
        scale_y_continuous(labels = percent_format()) +
        labs(x = "Hour of day (EST)",
             y = "% of tweets",
             color = "")


#################### with/without Picture/Link ####################
if(!require(stringr)){install.packages("stringr"); require(stringr)}

tweet_picture_counts <- tweets %>%
        filter(!str_detect(text, '^"')) %>%
        count(source,
              picture = ifelse(str_detect(text, "t.co"),
                               "Picture/link", "No picture/link"))

ggplot(tweet_picture_counts, aes(source, n, fill = picture)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "", y = "Number of tweets", fill = "")



##################################################################################
############################## comparison of words ##############################
###############################################################################

# tidytext::unnest_tokens, you can do "ngrams" using this function
# also try tm::TermDocumentMatrix, i.e. giving a corpus you can create a term document matrix
# t <- as.data.frame(
#         as.matrix(
#                 TermDocumentMatrix(tweets_text_corpus)
#         )
# )

if(!require(tidytext)){install.packages("tidytext"); require(tidytext)}

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- tweets %>%
        filter(!str_detect(text, '^"')) %>%
        mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
        tidytext::unnest_tokens(word, text, token = "regex", pattern = reg) %>%
        filter(!word %in% stop_words$word,
               str_detect(word, "[a-z]"))

tweet_words

require(plyr)
View(arrange(plyr::count(df = tweet_words, vars = "word"), -freq))
head(arrange(plyr::count(df = tweet_words, vars = "word"), -freq), 60)

############# word cloud on tweet words in recent 6 to 9 days #############
windows()
set.seed(1234)
wordcloud(word = arrange(plyr::count(df = tweet_words, vars = "word"), -freq)$word,
          freq = arrange(plyr::count(df = tweet_words, vars = "word"), -freq)$freq,
          min.freq = 2, colors = brewer.pal(8, "RdBu"), scale = c(4, .1), rot.per = .2)


############### comparison of device os ###############
# android_iphone_ratios <- tweet_words %>%
#         count(word, source) %>%
#         filter(sum(n) >= 5) %>%
#         spread(source, n, fill = 0) %>%
#         ungroup() %>%
#         mutate_each(funs((. + 1) / sum(. + 1)), -word) %>%
#         mutate(logratio = log2(Android / iPhone)) %>%
#         arrange(desc(logratio))


#############################################################################
################## sentiment analysis ##################
###################################################################################
# We'll work with the NRC Word-Emotion Association lexicon, available from the tidytext package, which associates words with 10 sentiments: positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.

nrc <- tidytext::sentiments %>%
        dplyr::filter(lexicon == "nrc") %>%
        dplyr::select(word, sentiment)

nrc

windows()
bb_sentiment <- tweet_words %>%
        select(id, created, word) %>%
        inner_join(y = nrc, by = "word") %>%
        group_by(sentiment) %>%
        dplyr::summarise(count = n()) %>%
        arrange(desc(count)) 

ggplot(bb_sentiment, aes(reorder(sentiment, count), count)) +
        geom_bar(aes(fill = sentiment), stat = "identity") +
        coord_flip()






########################## Poisson test ##########################
## measure the difference between Android and iPhone
# We then want to measure how much more likely the Android account is to use an emotionally-charged term relative to the iPhone account. Since this is count data, we can use a Poisson test to measure the difference:

# if(!require(broom)){install.packages("broom"); require(broom)}
# 
# sentiment_differences <- by_source_sentiment %>%
#         dplyr::group_by(sentiment) %>%
#         dplyr::do(
#                 broom::tidy(
#                         poisson.test(.$words, .$total_words)
#                 )  # first group by sentiment, then convert the output to a tidy data frame using broom::tidy()
#         )
# 
# sentiment_differences




