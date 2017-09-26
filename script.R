library(tidyverse) #read_lines
library(koRpus) #flesch
library(magrittr) # %<% operator
library(tm) #removing Punctuation etc

#saving all file names
filenames <- list.files(pattern = "\\.txt")

#reading file contents
read.content <- function(my.file) {
  tagged.text <- treetag(my.file, treetagger="manual",
                         lang="en", 
                         TT.options=list(path="~/Documents/treetagger/", 
                         preset="en"))
  #path to treetagger should be set to your local installation location
  flesch.score <- flesch(tagged.text)
  flesch.score <- slot(flesch.score, 'Flesch')
  return(round(flesch.score$RE,2)) #flesch score in the x.xx format
}

#and saving them to a list
texts <- lapply(filenames, read.content)

#list to data frame
df <- data.frame(matrix(unlist(texts), byrow=T))
colnames(df) <- "flesch"

calculate.lengths <- function(my.file) {
  my.text <- read_lines(as.character(my.file))
  my.text <- strsplit(paste(my.text, collapse = ""),' ')[[1]]
  my.text <- my.text %>%
    removePunctuation(preserve_intra_word_dashes = TRUE) %>% 
    removeNumbers() %>% 
    stripWhitespace() %>%
    tolower()
  return(length(my.text))
}

lengths <- lapply(filenames, calculate.lengths)
lengths.df <- data.frame(matrix(unlist(lengths),byrow = T))
colnames(lengths.df) <- "total.words"
df <- cbind(df, lengths.df)

#no. of sentences

calculate.sentences <- function(my.file) {
  my.text <- read_lines(as.character(my.file))
  my.text <- strsplit(paste(my.text, collapse = " "),' ')[[1]]
  my.text <- paste(my.text, collapse = " ")
  return(length(gregexpr('[[:alnum:] ][.!?]', my.text)[[1]]))
}

sentences <- lapply(filenames,calculate.sentences)
sentences.df <- data.frame(matrix(unlist(sentences),byrow=T))
colnames(sentences.df) <- "total.sentences"
df <- cbind(df,sentences.df)

#no of paragraphs
calculate.paragraphs <- function(my.file) {
  my.text <- read_lines(as.character(my.file))
  return(ceiling(length(my.text)+1)/2)
}

paragraphs <- lapply(filenames, calculate.paragraphs)
paragraphs.df <- data.frame(matrix(unlist(paragraphs),byrow=T))
colnames(paragraphs.df) <- "total.paragraphs"
df <- cbind(df,paragraphs.df)

#characters per word
calculate.characters <- function(my.file) {
  my.text <- read_lines(as.character(my.file))
  my.text <- strsplit(paste(my.text, collapse = ""),' ')[[1]]
  my.text <- my.text %>%
    removePunctuation(preserve_intra_word_dashes = TRUE) %>% 
    removeNumbers() %>% 
    stripWhitespace() %>%
    tolower()
  my.text <- paste(my.text, collapse = "")
  chars <- nchar(my.text)
  return(chars)
}

characters <- lapply(filenames, calculate.characters)
characters.df <- data.frame(matrix(unlist(characters),byrow=T))
colnames(characters.df) <- "total.characters"
df <- cbind(df,characters.df)

#characters per word
df[['characters.per.word']] <- round(df$total.characters/df$total.words,2)

#words per paragraph
df[['words.per.paragraph']] <- df$total.words/df$total.paragraphs
#to excel

write.table(df, file="results.csv", row.names=FALSE, sep="\t")