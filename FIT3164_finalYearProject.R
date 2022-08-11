###Set the working directory###
setwd("D:/Downloaded articles final year project")


#install.packages("XML")   #To read XML files
library("XML")   #Load the packages
library("methods")

library(xml2)

###################  extract the xml
pg <- read_xml("journal.pbio.0050304.xml")

#title-group, abstract, body, article-categories


#Extract the relevant data
title_group <- xml_find_all(pg, "//title-group")
abstract <- xml_find_all(pg, "//abstract")
body <- xml_find_all(pg, "//body")
article_categories <- xml_find_all(pg, "//article-categories")

#Trim all the data
title_group <- trimws(xml_text(title_group))
abstract <- trimws(xml_text(abstract))
body <- trimws(xml_text(body))
article_categories <- trimws(xml_text(article_categories))









#
#recs <- xml_find_all(pg, "//article-categories")
#print(recs)
#vals <- trimws(xml_text(recs))
#print(vals)

#Read XML data 
xmlData = xmlParse(file = 'journal.pbio.0050304.xml')
print(xmlData)

print(xmlSize(xmlData))





#Convert XML into dataframe
xml_df = xmlToDataFrame(xmlData)
xml_df('journal-meta')

print(xml_df)











