library(sbo)
library(tm)
library(RWeka)


News <- readLines(file("final/en_US/en_US.news.txt", open="r"))

Blogs <- readLines(file("final/en_US/en_US.blogs.txt", open="r"))

Twitter <- readLines(file("final/en_US/en_US.twitter.txt", open="r"))


### SAMPLING AND CLEANING THE DATA

# Files are a large size, need a small sample or else takes a very long time to generate results/run out of memory

# First, set the seed for reproducibility
set.seed(50)

# Sample the files. Keep somewhat small for shiny app
blogsSample <- sample(Blogs, round(length(Blogs)*0.1,digits = 0))
newsSample <- sample(News, round(length(News)*0.1,digits = 0))
twitterSample <- sample(Twitter, round(length(Twitter)*0.1,digits = 0))

# Combine the files together into one single text file
textFileSample <- paste(blogsSample,newsSample,twitterSample)


# Clean the data 

#Combine texts into corpus
textFileVectorSource <- VectorSource(textFileSample)
corpus <- VCorpus(textFileVectorSource)
dataframe <- data.frame(text=unlist(sapply(corpus, `[`, "content")), 
                        stringsAsFactors=F)

t <- sbo_predtable(object = textFileSample, # load corpus
                   N = 3, # Train a 3-gram model
                   dict = target ~ 0.75, # cover 75% of training corpus
                   .preprocess = sbo::preprocess, # Preprocessing transformation 
                   EOS = ".?!:;", # End-Of-Sentence tokens
                   lambda = 0.4, # Back-off penalization in SBO algorithm
                   L = 3L, # Number of predictions for input
                   filtered = "<EOS>"  #Exclude end of sentence
)

x <- sbo_predictor(t)

save(t,file="sbo_predictor.RData")

#Returns best prediction
NextWord <- function(phrase) {
  
    #ans <- (predict(x,phrase))
  #Exclude unknown words
  if ((predict(x,phrase))[1]=="<UNK>") { 
    print (predict(x,phrase)[2])
  }
  else{print(predict(x,phrase)[1])}

 }
  