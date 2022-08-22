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
  article <- read_xml(article,'UTF_8')
  
  
  title <- xml_text(xml_find_all(article, xpath = "//title-group"))
  abstract <- xml_text(xml_find_all(article, xpath = "//abstract"))
  body <- xml_text(xml_find_all(article, xpath = "//body"))
  category <- xml_text(xml_find_all(article, xpath = "//article-categories"))
  
  clean <- function(x){
    
    x <-removeNumbers(x)
    
    x <-removePunctuation(x)
    
    x <-tolower(x)
    
    x <-removeWords(x,stopwords('en'))
    
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
library(tm)
#Abstract TDM
myCorpus <- Corpus(VectorSource(articles_table$Abstract))

tdm_abstract <- TermDocumentMatrix(myCorpus)

tdm_abstract = as.data.frame(as.matrix(tdm_abstract))

tdm_abstract = t(tdm_abstract)

tdm_abstract = as.data.frame(tdm_abstract, stringsAsFactors = FALSE)


#Paragraph TDM
paraCorpus <- Corpus(VectorSource(articles_table$Paragraphs))

tdm_paragraph <- TermDocumentMatrix(paraCorpus)

tdm_paragraph <- removeSparseTerms(tdm_paragraph, 0.7)

tdm_paragraph = as.data.frame(as.matrix(tdm_paragraph))

tdm_paragraph = t(tdm_paragraph)

tdm_paragraph = as.data.frame(tdm_paragraph, stringsAsFactors = FALSE)

count1 = 0 
count2 = 0

articles_table$indability <- NA

for (i in 1:50){
  word_list = as.list(scan(text=articles_table$Title[i], what="[[:space:]]"))
  for ( words in word_list) {
    if ( ! is.null(tdm_abstract[i,words]) ){
      if (tdm_abstract[i,words] > 0 ){
        count1 = count1 + tdm_abstract[i,words]
        #density1 = 
        
      }
    }
    if ( ! is.null(tdm_paragraph[i,words]) ){
      if (tdm_paragraph[i,words] > 0 ){
        count2 = count2 + tdm_paragraph[i,words]
        #density2 =
      }
    }
  }
  print(i)
  print(count1)
  print(count2)
  print(count1+count2)
  #new column density save the density1+2
  if(count1+count2 > 400){
    articles_table$findability[i] = 1
  }
  else{
    articles_table$findability[i] = 0
  }
  count1 = 0 
  count2 = 0
}
#median for density column
#lop thru df agn then if smaller than median, 0 else 1

