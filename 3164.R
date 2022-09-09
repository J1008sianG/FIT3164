rm(list = ls())
library("slam")
library("tm")
library("SnowballC")
library("dplyr")
library("XML")
library("xml2")
library("RCurl")
library("e1071")
library("randomForest")
library("tree")
library("adabag")
library("caret")
library("plyr")
library(caTools)
library(neuralnet)



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


articles_table$abstractTotal <- rowSums(as.matrix(tdm_abstract))
articles_table$paragraphTotal <- rowSums(as.matrix(tdm_paragraph))
articles_table$abstractDensity <- NA
articles_table$paragraphDensity <- NA

abstractCount = 0 
paragraphCount = 0

for (i in 1:length(articles_table$Title)){
  word_list = as.list(scan(text=articles_table$Title[i], what="[[:space:]]"))
  for ( words in word_list) {
    if ( ! is.null(tdm_abstract[i,words]) ){
      if (tdm_abstract[i,words] > 0 ){
        abstractCount = abstractCount + tdm_abstract[i,words]
        absDensity = abstractCount / articles_table$abstractTotal[i]
        articles_table$abstractDensity[i] = absDensity
      }
    }
    if ( ! is.null(tdm_paragraph[i,words]) ){
      if (tdm_paragraph[i,words] > 0 ){
        paragraphCount = paragraphCount + tdm_paragraph[i,words]
        pDensity = paragraphCount / articles_table$paragraphTotal[i]
        articles_table$paragraphDensity[i] = pDensity
      }
    }
  }
  abstractCount = 0 
  paragraphCount = 0
}
#median for density column
#lop thru df agn then if smaller than median, 0 else 1
medianWithoutNA<-function(x) {
  median(x[which(!is.na(x))])
}

medians = apply(articles_table[,7:8], 2, medianWithoutNA)

abs_medianDensity = medians[1]
p_medianDensity = medians[2]

articles_table$Findability <- NA

for (i in 1:length(articles_table$Title)){
  if(is.na(articles_table$paragraphDensity[i])){
    articles_table$Findability[i] = 0
  }
  else {
    if(is.na(articles_table$abstractDensity[i])){
      if(articles_table$paragraphDensity[i] >= p_medianDensity){
        articles_table$Findability[i] = 1
      }
      else{
        articles_table$Findability[i] = 0
      }
    }
    else{
      
      if(articles_table$abstractDensity[i] >= abs_medianDensity && articles_table$paragraphDensity[i] >= p_medianDensity){
        articles_table$Findability[i] = 1
      }
      else{
        articles_table$Findability[i] = 0
      }
    }
  }
}

#Replace NaN with 0 in articles_table
articles_table[is.na(articles_table)] <- 0


#split data
train.row <- sample(1:nrow(articles_table), 0.7*nrow(articles_table))
train_data <- articles_table[train.row,]
test_data <- articles_table[-train.row,]

#########################Modelling#####################

train_data$Findability <- as.factor(train_data$Findability)
test_data$Findability <- as.factor(test_data$Findability)

#Naive Bayes
naive.fit <- naiveBayes(Findability ~ Abstract+Paragraphs+Category, train_data)
naive.pred <- predict(naive.fit, test_data)
naive.cfm <- table("actual" = test_data$Findability, "predicted" = naive.pred)
naive.acc <- round(mean(naive.pred == test_data$Findability)*100, digits=2)
cat("NaÃ¯ve Bayes model accuracy is: ", naive.acc, "%") 

#Random Forest 
rf.fit <- randomForest(Findability ~ Abstract+Paragraphs+Category, train_data)
rf.pred <- predict(rf.fit, test_data)
rf.cfm <- table("actual" = test_data$Findability, "predicted" = rf.pred)
rf.acc <- round(mean(rf.pred == test_data$Findability)*100, digits = 2)
cat("Random Forest ensemble model accuracy is: ", rf.acc , "%") 

#Decision Tree 
tree.fit <- suppressWarnings(tree(Findability ~ Abstract+Paragraphs+Category, data = train_data, method = "class"))
tree.pred <- suppressWarnings(predict(tree.fit, test_data, type="class"))
tree.cfm <- table("actual" = test_data$Findability, "predicted" = tree.pred)
tree.acc <- round(mean(tree.pred == test_data$Findability)*100, digits = 2)
cat("Decision tree accuracy is: ", tree.acc, "%") 

#SVM 
svm.fit <- svm(Findability ~ Abstract+Paragraphs+Category, train_data,  type = 'C-classification', kernel="linear")
svm.predict <- predict(svm.fit, test_data)
svm.cfm <- table("actual" = test_data$Findability, "predicted" = svm.predict)
svm.acc <- round(mean(svm.predict == test_data$Findability)*100, digits = 2)
cat("Support Vector Machine accuracy is: ", svm.acc, "%") 


#Neural Network 
nn_train <- train_data
nn_test <- test_data
nn.fit <- neuralnet(Findability ~  Abstract+Paragraphs+Category, nn_train,hidden=3,act.fct = "logistic", linear.output = FALSE )
plot(nn.fit)
nn.pred <- compute(nn.fit, nn_test)
nn.pred <- as.data.frame(round(nn.pred$net.result,0))
nn.cfm.1 <- table("actual" = nn_test$Findability, "predicted" = nn.pred$V1)
nn.cfm.2 <- table("actual" = nn_test$Findability, "predicted" = nn.pred$V2)
nn.acc.1 <- sum(nn.cfm.1[1], nn.cfm.1[4]) / sum(nn.cfm.1[1:4])
nn.acc.2 <- sum(nn.cfm.2[1], nn.cfm.2[4]) / sum(nn.cfm.2[1:4])
nn.acc <- (sum(nn.acc.1,nn.acc.2)/2)*100
cat("Neural Network accuracy is: ", nn.acc, "%") 

###########################################################################################################################

#AUC value of original random forest 
rf.conf = predict(rf.fit, test_data, type = "prob")
rf.conf.pred = prediction(rf.conf[,2], test_data$Findability)
rf.auc = performance(rf.conf.pred, "auc")
original_auc_val = as.numeric(rf.auc@y.values)


#Find the original number of trees used 
rf.fit$ntree #500 

#save training and testing data in new variable
imp.train = train_data
imp.test = test_data



#Algorithm to fit the number of trees from 501 to 700 by tuning different mtry value

for(ntrees in 500:700){
  set.seed(9999)
  new_rf.fit <- randomForest(Findability ~ Abstract+Paragraphs+Category, data=imp.train,importance=TRUE, ntree=ntrees, mtry=2)
  new_rf.pred = predict(new_rf.fit, imp.test)
  new_rf.cfm = table(actual = imp.test$Findability, predicted = new_rf.pred)
  
  new_rf.acc = round(mean(new_rf.pred == imp.test$Findability)*100, digits = 2)
  
  #calculate confidence and AUC of the new Random Forest model 
  new_rf.conf = predict(new_rf.fit, imp.test, type = "prob")
  new_rf.conf.pred = prediction(new_rf.conf[,2], df_target$Findability)
  new_rf.auc = performance(new_rf.conf.pred, "auc")
  new_rf.auc = as.numeric(new_rf.auc@y.values)
  
  cat("Best Tree is: ", new_rf.fit$ntree, "accuracy is: ", new_rf.acc , 
      "AUC value: ", new_rf.auc ,"\n")
  
  
  
}









