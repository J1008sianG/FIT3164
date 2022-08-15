rm(list = ls())
library("slam")
library("tm")
library("SnowballC")
library("dplyr")
library("XML")
library("xml2")
library("RCurl")

# read an article
article <- read_xml("journal.pbio.0050304.xml")


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

articles_table <- data.frame(matrix(ncol = 4, nrow = 0))
articles_table <- rbind(articles_table, c(title,abstract,body,category))

temp_abs <- ""
if (length(abstract) > 1){
   for (text in abstract){
     temp_abs <- paste(temp_abs,text)
   }
 }
abstract <- temp_abs

colnames(articles_table) = c("Title","Abstract","Paragraphs","Category")




