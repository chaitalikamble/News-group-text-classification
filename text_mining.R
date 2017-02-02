# Title   : Implementation for determining the title for newsgrops 
# Details : This program determines the titles for newsgroups using classification techniques. 
#           Here, the input will be path to the newsgroups and it will output the accuracies from two
#           classifiers Knn and Naive Bayes.  
# Authors : Chaitali Kamble
#         : Siddharth Bidwalkar
#         : Rudresh Pandit
#Date     : 11/17/2016
# Version : 1.0

# install all the required packages for implementation as provided in ReadMe file


#args = commandArgs(trailingOnly = TRUE)
#if(length(args) == 0){
#    stop("Please supply path for newsgroups")
#  }else if (length(args) == 1){
#    Common_Path = args[1]
#}

# Use libraries from installed packages
library(caret)
library(MASS)
library(tm)
library(plyr)
library(class)
library(tmap)
library(slam)
library(e1071)

options(stringsAsFactors = FALSE)

# Provide your path for directories here.
Common_Path <- "Please provide your path here for newsgroups"

# Calculate total number of files present in the directory    
Total_files = length(list.files(Common_Path))

# Add all files to a list named "news_titles"
news_titles <- list.files(Common_Path)

# Make a list of corpus and create a corpus for each and every directory from news_titles lists
Corpus_list <- list()
i <- 1
while(i < (Total_files+1)){
  # Make a corpus by providing a Directory source which is common path appended by title
  # by iterating over the list news_titles
  Corpus_list[[i]] <- Corpus(DirSource(sprintf("%s/%s", Common_Path, news_titles[[i]])))
  i <- i + 1
}

# Clean the data matrix function
cleaning_Corpus <- function(Total_Corpus){
  # Remove Punctuation from data   
  Total_Corpus <- tm_map(Total_Corpus, removePunctuation)  
  # Strip out white spaces	
  Total_Corpus <- tm_map(Total_Corpus, stripWhitespace)    
  # Remove stop words from data	
  Total_Corpus <- tm_map(Total_Corpus, removeWords, stopwords("english")) 
  # convert all the words to lower case
  Total_Corpus <- tm_map(Total_Corpus, content_transformer(tolower), lazy = TRUE) 
  # return function to store the result
  return (Total_Corpus)
}


#Loop over all the corpus and create an array of term document matrices
i <- 1
Array_Term_Matrix <- list()
while (i < (Total_files+1)){
  # Call clean corpus function by passing corpus from list
  cleaned_Corpus <- cleaning_Corpus(Corpus_list[[i]])
  # create term document matrix
  TermDocumentMatrix <- DocumentTermMatrix(cleaned_Corpus)
  # remove sparse terms by setting a level of sparsity to 0.6
  TermDocumentMatrix <- removeSparseTerms(TermDocumentMatrix, 0.6)
  #Store title and corresponding term document matrix into list
  Array_Term_Matrix[[i]] <- list(title = news_titles[[i]], tdm = TermDocumentMatrix)
  i <- i + 1
}

#Create a group of term document matrix and convert term document matrices into 
#corresponding data frames
Grouped_Term_Document_Matrix <- list()
i = 1
while(i < (Total_files+1)){
  matrix <- t(t(data.matrix(Array_Term_Matrix[[i]]$tdm)))
  # create data frame from matrix
  frame_data <- as.data.frame(matrix,stringASFactors=FALSE)
  frame_data <- cbind(frame_data, rep(Array_Term_Matrix[[i]]$title, nrow(frame_data)))
  #Assign column titles to column names
  colnames(frame_data)[ncol(frame_data)] <- "title"
  Grouped_Term_Document_Matrix[[i]] <- frame_data
  i = i + 1
}

# Stack two matrices
Total_data_matrix <- do.call(rbind.fill, Grouped_Term_Document_Matrix)
Total_data_matrix[is.na(Total_data_matrix)] <- 0

# Creat Sample size
sample_size <- floor(0.7 * nrow(Total_data_matrix))

# set a seed of 1000
set.seed(1000)

#Create training index
Training_index <- sample(nrow(Total_data_matrix), sample_size, replace = FALSE, prob = NULL)

#Create testing index
Testing_index <- (1:nrow(Total_data_matrix))[-Training_index]

# Create term matrix group with titles
Term_matrix_group <- Total_data_matrix[,"title"]

# Column names which exists in title
column_names_in_title <- (colnames(Total_data_matrix) %in% "title")

# Create a Total data matrix including columns that are not present in title
Total_data_matrix <- Total_data_matrix[,!column_names_in_title]

# Create training data
training_data <- Total_data_matrix[Training_index,]

# Create testing data
testing_data <- Total_data_matrix[Testing_index,]

# titles for training data
titles_train <- Term_matrix_group[Training_index]

# titles for testing data
titles_test <- Term_matrix_group[Testing_index]

#Create Classifier 1 using knn classification
Classifier1 <- knn(training_data,testing_data, titles_train)

# Create confusion matrix for Classifier 1
confusion_matrix1 <- confusionMatrix(Classifier1,titles_test)
overall1 <- confusion_matrix1$overall

# Get accuracy
accuracy1 <- overall1['Accuracy']
print(accuracy1)
# Create Classifier 2 using Naive Bayes classification
Classifier2 <- naiveBayes(training_data,titles_train)

# Predict titles
#predictions <- predict(Classifier2,titles_test)

# Create confusion matrix for Classifier 2
#confusion_matrix2 <- confusionMatrix(predictions,titles_test)
#overall12 <- confusion_matrix2$overall

# Get accuracy
#accuracy2 <- overall12['Accuracy']





