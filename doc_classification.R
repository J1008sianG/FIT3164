#load libraries
library("XML")
library("methods")
library("xml2")
library("dplyr")
library("tidyr")
library("readr")


to_list <- function(xmlfile){
  #create empty list
  sub_title_list <- list() 
  article_title_list <- list()  
  abstract_list <- list() 
  paragraph_list <- list() 
  
  #parse XML file
  data <- xmlParse(xmlfile)
  
  #extract sub title
  sub_title <- xpathSApply(data, "//title",xmlValue )
  sub_title_list <- c(sub_title)
  
  #extract article title
  article_title <- xpathSApply(data, "//article-title",xmlValue )
  article_title_list <- c(article_title)
  
  #extract abstract
  abstract <- xpathSApply(data, '/article/front/article-meta/abstract/p', xmlValue)
  abstract_list <- c(abstract)
  
  #extract paragraph
  paragraph <- xpathSApply(data, "//p",xmlValue )
  paragraph_list <- c(paragraph)
  
  #combine lists 
  newList <- list("subtitle" = sub_title_list, "article_title" = article_title_list, "abstract" = abstract_list, "paragraph" = paragraph_list)
  
  return(newList)
  
  
}

myText = to_list("article_2.xml")
myText$paragraph
