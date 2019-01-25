# save(list = ls(), file = "textMining_exercise.RData")

#############################################################
### Part I: import, clean, word cloud

# load packages
lapply(c("tm", "RColorBrewer", "wordcloud", "magrittr", "dplyr", "sqldf", "tidytext"), 
       library, character.only = T)

# load text file
q29 <- read.csv(file.choose(), header = T) 
class(q29)  # [1] "data.frame"
dim(q29)  # [1] 1211    3

# create corpus
q29_corpus <- tm::Corpus(VectorSource(q29$body))
class(q29_corpus)  # [1] "SimpleCorpus" "Corpus" 
tm::inspect(q29_corpus[[1]])

#######################
#### text clean-up ####
#######################

### method 1 ###
q29_corpus <- tm_map(q29_corpus, removePunctuation)
q29_corpus <- tm_map(q29_corpus, content_transformer(tolower)) 
q29_corpus <- tm_map(q29_corpus, removeNumbers)
q29_corpus <- tm_map(q29_corpus, stripWhitespace)
q29_corpus <- tm_map(q29_corpus, function(x) iconv(x, "latin1", "ASCII", ""))
q29_corpus <- tm_map(q29_corpus, removeWords, stopwords("english"))
q29_corpus <- tm_map(q29_corpus, function(x) removeWords(x, stopwords()))

### method 2 ###
# data(crude)
# crude[[1]]
# skipWords <- function(x) removeWords(x, c("it", "the"))
# funs <- list(stripWhitespace,
#              skipWords,
#              removePunctuation,
#              content_transformer(tolower))
#
# tm_map(crude, FUN = tm_reduce, tmFuns = funs)[[1]]

### method 3 ###
clean.text = function(x)
{
        # tolower
        x = tolower(x)
        # remove rt
        x = gsub("rt", "", x)
        # remove at
        x = gsub("@\\w+", "", x)
        # remove punctuation
        x = gsub("[[:punct:]]", "", x)
        # remove numbers
        x = gsub("[[:digit:]]", "", x)
        # remove links http
        x = gsub("http\\w+", "", x)
        # remove tabs
        x = gsub("[ |\t]{2,}", "", x)
        # remove blank spaces at the beginning
        x = gsub("^ ", "", x)
        # remove blank spaces at the end
        x = gsub(" $", "", x)
        return(x)
}

q29$body <- clean.text(q29$body)


##############################################################
######################### word cloud ######################### 


####################### overall word cloud #######################
windows()
set.seed(8321)
wordcloud(q29_corpus,
          min.freq = 3,
          colors = brewer.pal(8, "RdBu"))

savePlot(filename = "q29_word cloud (overall).png", type = "png")


####################### comparison word cloud #######################
# clean text #
# q29$body <- clean.text(q29$body)

q29_seg1 <- sqldf("select body from q29 where segment = 1")
q29_seg2 <- sqldf("select body from q29 where segment = 2")
q29_seg3 <- sqldf("select body from q29 where segment = 3")
q29_seg4 <- sqldf("select body from q29 where segment = 4")

q29_seg1 <- paste(q29_seg1, collapse = " ")
q29_seg2 <- paste(q29_seg2, collapse = " ")
q29_seg3 <- paste(q29_seg3, collapse = " ")
q29_seg4 <- paste(q29_seg4, collapse = " ")

all <- c(q29_seg1, q29_seg2, q29_seg3, q29_seg4)
all <- removeWords(all,
                   c(stopwords("english"), "q29_seg1", "q29_seg2", "q29_seg3", "q29_seg4"))

# convert into corpus #
q29_seg_corpus <- Corpus(VectorSource(all))

# convert into term document matrix #
tdm <- TermDocumentMatrix(q29_seg_corpus)
tdm <- as.matrix(tdm)

# add column names
colnames(tdm) <- c("segment 1", "segment 2", "segment 3", "segment 4")

# comparison cloud
windows()
set.seed(1234)
comparison.cloud(tdm,
                 random.order = FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC"),
                 title.size = 2,
                 max.words = 1000)

savePlot(filename = "q29_comparison cloud_4Segments.png", type = "png")  # v1 with max.words = 500

####################### commonality word cloud #######################
windows()
set.seed(8321)
commonality.cloud(tdm, random.order = FALSE,
                  colors = brewer.pal(8, "Dark2"),
                  title.size = 3)

savePlot(filename = "q29_commonality cloud.png", type = "png")


###############################################################################################

#############################################################
### Part II: tdm, unnest tokens, sentiment analysis

lapply(c("purrr", "tidyr", "scales", "ggplot2", "tidytext"), 
       library, character.only = T) %>%
        invisible

#############################################################
###################### word extraction ######################
#############################################################

### Term Document Matrix ###
# tm::TermDocumentMatrix, i.e. giving a corpus you can create a term document matrix
q29_tdm <- as.data.frame(
        as.matrix(
                tm::TermDocumentMatrix(q29_corpus)
        )
)

# cleaning up, adding segment ID as column names
names(q29_tdm) <- q29$segment
length(names(q29_tdm))
q29_tdm$word <- tolower(row.names(q29_tdm))
row.names(q29_tdm) <- NULL

# listGather <- tidyr::gather(q29_tdm, key = "seg", value = "count", -word)
# does not work  # Error: cannot allocate vector of size 15.2 Mb

# write.table(q29_tdm, "q29_tdm.txt", sep = "\t", row.names = F, append = F)

# do lapply instead

fields <- c("1", "2", "3", "4")

tdmAgg <- lapply(fields, function(x){
        apply( subset(q29_tdm, 
                      select = names(q29_tdm) %in% x), 
               MARGIN = 1,  
               FUN = sum)
})

str(tdmAgg)

tdmAgg2 <- data.frame(q29_tdm$word,
                      tdmAgg[1],
                      tdmAgg[2],
                      tdmAgg[3],
                      tdmAgg[4])
names(tdmAgg2) <- c("word", "segment 1", "segment 2", "segment 3", "segment 4")

tdmAgg3 <- tidyr::gather(tdmAgg2, 
                         key = "segment", 
                         value = "count", 
                         -word)

# ### unnest tokens ###
# # tidytext::unnest_tokens, you can do "ngrams" using this function
# reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
# words <- tidytext::unnest_tokens(tbl = q29,  # a data.frame
#                                  word,  # output
#                                  body,  # input 
#                                  token = "regex", 
#                                  pattern = reg) %>%
#         filter(!word %in% tidytext::stop_words$word,
#                stringr::str_detect(word, "[a-z]"))


#############################################################################
################## sentiment analysis ##################
###################################################################################

# We'll work with the NRC Word-Emotion Association lexicon, available from the tidytext package, which associates words with 10 sentiments: positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.

nrc <- tidytext::sentiments %>%
        dplyr::filter(lexicon == "nrc") %>%
        dplyr::select(word, sentiment)

nrc

tdmAgg4 <- dplyr::left_join(tdmAgg3, 
                            nrc,
                            by = "word")

tdmAgg4$sentiment <- ifelse(is.na(tdmAgg4$sentiment), 
                            "neutral", tdmAgg4$sentiment)

tdmAgg5 <- aggregate(count ~ segment + sentiment,
                     data = tdmAgg4,
                     FUN = sum) %>%
        dplyr::arrange(segment, -count) %>%
        print

tdmAgg6 <- subset(tdmAgg5, sentiment != "neutral") 
tdmAgg6 <- merge(tdmAgg6, 
                 aggregate(count ~ segment, tdmAgg6, sum), 
                 "segment") %>% 
        print
tdmAgg6$percent <- round(tdmAgg6$count.x / tdmAgg6$count.y, 3)
# tdmAgg6$percent <- paste(round(tdmAgg6$count.x / tdmAgg6$count.y, 3) * 100, "%", sep = " ")
names(tdmAgg6) <- c("segment", "sentiment", "word_count", "total_word_count", "percent")

################
### ggplot2 ###
windows()
ggplot(data = tdmAgg6,
       aes(x = reorder(sentiment, word_count), 
           y = percent)) +
        geom_bar(aes(fill = segment), stat = "identity") +
        coord_flip() +
        xlab("") +
        theme(plot.title = element_text(hjust = 0.5)) +
        ggtitle("Q29. Overall, what information/services do you need today to better service your clients that you are not getting from asset managers you work with?") + 
        theme_bw() +
        scale_y_continuous(labels = scales::percent) +
        facet_wrap(~ segment, ncol = 2)

savePlot("sentiment_by segments.png", type = "png")




