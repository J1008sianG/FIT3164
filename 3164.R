rm(list = ls())
library("slam")
library("tm")
library("SnowballC")
library("dplyr")
library("XML")
library("xml2")
library("RCurl")


file_list <- list.files()
articles_table <- data.frame(matrix(ncol = 4, nrow = 0))

for (article in file_list) {
  # read an article
  article <- read_xml(article)
  
  
  title <- xml_text(xml_find_all(article, xpath = "//title-group"))
  abstract <- xml_text(xml_find_all(article, xpath = "//abstract"))
  body <- xml_text(xml_find_all(article, xpath = "//body"))
  category <- xml_text(xml_find_all(article, xpath = "//article-categories"))
  
  clean <- function(x){
    
    x <-tolower(x)
    
    x <-removeWords(x,stopwords('en'))
    
    x <-removePunctuation(x)
    
    x <-removeNumbers(x)
    
    x <-stripWhitespace(x)
    
    x <-stemDocument(x)
    
    return(x) }
  
  
  title <- clean(title)
  abstract <- clean(abstract)
  body <- clean(body)
  category <- clean(category)
  
  temp_abs <- ""
  if (length(abstract) > 1){
    for (text in abstract){
      temp_abs <- paste(temp_abs,text)
    }
  }
  abstract <- temp_abs

  articles_table <- rbind(articles_table, c(title,abstract,body,category))


}

colnames(articles_table) = c("Title","Abstract","Paragraphs","Category")

#################################Create term document matrix########################################################
#Abstract TDM
myCorpus <- Corpus(VectorSource(articles_table$Abstract))

tdm_abstract <- TermDocumentMatrix(myCorpus)

tdm_abstract = as.data.frame(as.matrix(tdm_abstract))

tdm_abstract = t(tdm_abstract)

tdm_abstract = as.data.frame(tdm_abstract, stringsAsFactors = FALSE)

tdm_abstract <- tdm_abstract[1,]


#Paragraph TDM
paraCorpus <- Corpus(VectorSource(articles_table$Paragraphs))

tdm_paragraph <- TermDocumentMatrix(paraCorpus)

tdm_paragraph <- removeSparseTerms(tdm_paragraph, 0.7)

tdm_paragraph = as.data.frame(as.matrix(tdm_paragraph))

tdm_paragraph = t(tdm_paragraph)

tdm_paragraph = as.data.frame(tdm_paragraph, stringsAsFactors = FALSE)

