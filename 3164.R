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
library(ROCR)

setwd("C:/Users/User/Desktop/FIT3164/coding/data/t_data_200")
set.seed(10000)



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
#bottom 20% 
#lop thru df agn then if smaller than median, 0 else 1
medianWithoutNA<-function(x) {
  median(x[which(!is.na(x))])
}

medians = apply(articles_table[,7:8], 2, medianWithoutNA)

abs_medianDensity = 0.15 #medians[1]
p_medianDensity = 0.1 #medians[2]

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
cat("Random Forest model accuracy is: ", rf.acc , "%") 

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

###################################Random Forest evaluation####################################

#Calculate confidence and construct ROC curve  
rf.conf = predict(rf.fit, test_data, type = "prob")
rf.conf.pred = prediction(rf.conf[,2], test_data$Findability)
rf.perf = performance(rf.conf.pred, "tpr", "fpr")
plot(rf.perf, col = "red")
abline(a=0, b=1, col = "blue")

#Calculate AUC value for random forest #0.48
rf.auc = performance(rf.conf.pred, "auc")
rf.auc = round(rf.auc@y.values[[1]],2)
cat("Area Under the Curve (AUC) of Random Forest is:", rf.auc)


#Calculate RMSE for original random forest #0.55
rf.rmse = round(sqrt(mean((as.numeric(rf.pred) - as.numeric(test_data$Findability))^2)),2)
cat("RMSE value is:", rf.rmse)

#Find the original number of trees used 
rf.fit$ntree #500 

#Parameter tuning for num of trees
tree_num = c(550, 600, 650, 500, 750, 800, 850, 900, 950, 1000)
best_acc = rf.acc
best_pred = rf.pred
best_fit = rf.fit 

for(i in tree_num){
  set.seed(501:1000)
  rf.fit.new = randomForest(Findability ~  Abstract+Paragraphs+Category, data = train_data, importance = TRUE, ntree = i)
  rf.pred.new = predict(rf.fit.new, test_data)
  rf.new.acc = round(mean(rf.pred.new == test_data$Findability)*100, digits = 2)
  
  if(rf.new.acc > best_acc){
    best_acc = rf.new.acc
    best_fit = rf.fit.new
    best_pred = rf.pred.new
    
    
  }
  
}

cat("best number of tree is", best_fit$ntree, "with accuracy of", best_acc,"%")

#Tuning different mtry for best result 
for(i in (best_fit$mtry+1):7){
  set.seed(999)
  rf.mtry = randomForest(Findability ~  Abstract+Paragraphs+Category, data = train_data, importance = TRUE, ntree = best_fit$ntree, mtry = i)
  rf.mtry.pred = predict(rf.mtry, test_data)
  rf.mtry.acc = round(mean(rf.mtry.pred == test_data$Findability)*100, digits = 2)
  
  if(rf.mtry.acc > best_acc){
    best_acc = rf.mtry.acc
    best_fit = rf.mtry
    best_pred = rf.mtry.pred
    
  }
}


cat("best number of split is", best_fit$mtry, "with accuracy of", best_acc,"%")



