

# Install required packages if not already installed
if(!require(tm)) install.packages("tm", dependencies = TRUE)
if(!require(SnowballC)) install.packages("SnowballC", dependencies = TRUE)
if(!require(wordcloud)) install.packages("wordcloud", dependencies = TRUE)
if(!require(RColorBrewer)) install.packages("RColorBrewer", dependencies = TRUE)
# Load the required libraries
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
# Step 1: Create a small text corpus
texts <- c(
"The quick brown fox jumps over the lazy dog.",
"Data Science is an exciting field with endless possibilities!",
"R programming makes data analysis easy and powerful."
)
corpus <- Corpus(VectorSource(texts))
cat("==== Original Corpus ====\n")
inspect(corpus)
# Step 2: Convert to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))
cat("\n==== After Converting to Lowercase ====\n")
inspect(corpus)
# Step 3: Remove punctuation
corpus <- tm_map(corpus, removePunctuation)
cat("\n==== After Removing Punctuation ====\n")
inspect(corpus)
# Step 4: Remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))
cat("\n==== After Removing Stopwords ====\n")
inspect(corpus)
# Step 5: Perform stemming
corpus <- tm_map(corpus, stemDocument)
cat("\n==== After Stemming ====\n")
inspect(corpus)
# Step 6: Remove extra whitespace
corpus <- tm_map(corpus, stripWhitespace)
cat("\n==== After Removing Extra Whitespace ====\n")
inspect(corpus)
# Step 7: Display final cleaned text
cat("\n==== Final Cleaned Text ====\n")
for (i in 1:length(corpus)) {
cat(paste0("Document ", i, ": ", as.character(corpus[[i]]), "\n"))
}
# Step 8: Create a Term-Document Matrix (TDM)
tdm <- TermDocumentMatrix(corpus)
cat("\n==== Term-Document Matrix (TDM) ====\n")
inspect(tdm)
# Step 9: Convert TDM to matrix form for word frequency analysis
tdm_matrix <- as.matrix(tdm)
word_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)
# Step 10: Display word frequencies
cat("\n==== Word Frequencies ====\n")
print(word_freq)
# Step 11: Plot a bar chart of the most frequent words
top_words <- head(word_freq, 10)
barplot(top_words,
las = 2,
col = "steelblue",
main = "Top 10 Most Frequent Words",
ylab = "Frequency")
# Step 12: Word Cloud Visualization
set.seed(1234)
wordcloud(words = names(word_freq), freq = word_freq,
min.freq = 1,
max.words = 100 , random.order = FALSE,colors = brewer.pal(8, "Dark2"))

# Install required packages (only if not already installed)
if(!require(tm)) install.packages("tm", dependencies = TRUE)
if(!require(SnowballC)) install.packages("SnowballC", dependencies = TRUE)
if(!require(syuzhet)) install.packages("syuzhet", dependencies = TRUE)
if(!require(wordcloud)) install.packages("wordcloud", dependencies = TRUE)
if(!require(RColorBrewer)) install.packages("RColorBrewer", dependencies = TRUE)
# Load libraries
library(tm)
library(SnowballC)
library(syuzhet)
library(wordcloud)
library(RColorBrewer)
# ------------------------------------------------------------
# Step 1: Create Corpus
# ------------------------------------------------------------
texts <- c(
"The movie was fantastic! I really enjoyed every moment of it.",
"The service at the restaurant was terrible and the food was cold.",
"The product quality is okay, not too great but not too bad either.",
"I love using this app. It’s simple, fast, and makes my life easier!",
"The weather ruined our trip. We couldn’t go anywhere and felt bored.",
"The teacher inspired me to work harder and follow my dreams.",
"I hate when my computer crashes during important work.",
"The support team responded quickly and solved my issue. Great job!"
)
corpus <- Corpus(VectorSource(texts))
cat("==== Original Corpus ====\n")
inspect(corpus)
# ------------------------------------------------------------
# Step 2: Text Preprocessing
# ------------------------------------------------------------
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)
cat("\n==== Cleaned Corpus ====\n")

inspect(corpus)
# Convert cleaned corpus to plain text vector
clean_texts <- sapply(corpus, as.character)
# ------------------------------------------------------------
# Step 3: Sentiment Analysis using syuzhet
# ------------------------------------------------------------
sentiments <- get_nrc_sentiment(clean_texts)
cat("\n==== Sentiment Scores (per document) ====\n")
print(sentiments)
# ------------------------------------------------------------
# Step 4: Sentiment Polarity Classification (Positive / Negative / Neutral)
# ------------------------------------------------------------
# Compute overall sentiment polarity
sentiments$polarity <- ifelse(sentiments$positive > sentiments$negative, "Positive",
ifelse(sentiments$negative > sentiments$positive, "Negative", "Neutral"))
# Combine results
results <- data.frame(Document = 1:length(texts),
Text = texts,
Positive = sentiments$positive,
Negative = sentiments$negative,
Polarity = sentiments$polarity)
cat("\n==== Sentiment Polarity Classification ====\n")
print(results)
# ------------------------------------------------------------
# Step 5: Word Clouds for Positive and Negative Words
# ------------------------------------------------------------
# Convert all text into a single vector
all_words <- paste(clean_texts, collapse = " ")
# Split into words
word_vector <- unlist(strsplit(all_words, " "))
# Get sentiment for each unique word
word_sentiments <- get_nrc_sentiment(word_vector)
# Separate positive and negative words
positive_words <- word_vector[word_sentiments$positive > 0]
negative_words <- word_vector[word_sentiments$negative > 0]
# Remove empty strings (if any)
positive_words <- positive_words[positive_words != ""]
negative_words <- negative_words[negative_words != ""]

# Step 6: Generate Word Clouds
# ------------------------------------------------------------
par(mfrow = c(1,2)) # Display two plots side-by-side
# Positive word cloud
wordcloud(words = positive_words,
min.freq = 1,
max.words = 100,
colors = brewer.pal(8, "Greens"),
scale = c(3, 0.8),
random.order = FALSE,
main = "Positive Words")
# Negative word cloud
wordcloud(words = negative_words,
min.freq = 1,
max.words = 100,
colors = brewer.pal(8, "Reds"),
scale = c(3, 0.8),
random.order = FALSE,
main = "Negative Words")
par(mfrow = c(1,1)) # Reset layout
# ------------------------------------------------------------
# Step 7: Overall Sentiment Distribution Visualization
# ------------------------------------------------------------
total_sentiments <- colSums(sentiments[, 1:8]),barplot(total_sentiments,
col = rainbow(8),las = 2,
main = "Overall Emotion Distribution", ylab = "Count",
xlab = "Sentiment Type")




# Load required libraries
# - readr: For fast CSV reading.
# - tm: For text corpus creation, cleaning, and DTM.
# - e1071: For Naive Bayes classification.
# - caret: For train/test splitting and confusion matrix.
# - SnowballC: For stemming (reduces words like "running" -> "run").
library(readr)
library(tm)
library(e1071)
library(caret)
library(SnowballC)
# Step 1: Load the training and test datasets from local files
# Explanation: Use relative paths to the unzipped dataset folder in your working
directory.
# If files are directly in working dir, change to "train.csv" and "test.csv".
# Uncomment below for interactive selection (pops up file browser).
dataset_folder <- "ag-news-classification-dataset" # Update if your folder name differs
train_file <- paste0("./", dataset_folder, "/train.csv")
test_file <- paste0("./", dataset_folder, "/test.csv")
# Interactive file selection (uncomment if preferred)
# train_file <- file.choose(new = TRUE) # Browse and select train.csv
# test_file <- file.choose(new = TRUE) # Browse and select test.csv
# Check if files exist (optional, for debugging)
if (!file.exists(train_file)) {
print(paste("Train file not found:", train_file))
print("Adjust 'dataset_folder' or use file.choose() above.")
stop("Fix path and rerun.")
}
if (!file.exists(test_file)) {
print(paste("Test file not found:", test_file))
stop("Fix path and rerun.")
}
train_df <- read_csv(train_file, col_types = cols(`Class Index` = col_integer(),
Title = col_character(),
Description = col_character()))
test_df <- read_csv(test_file, col_types = cols(`Class Index` = col_integer(),



Title = col_character(),
Description = col_character()))
# Add factor labels for topics (for easier interpretation)
train_df$Topic <- factor(train_df$`Class Index`, levels = 1:4,
labels = c("World", "Sports", "Business", "Sci/Tech"))
test_df$Topic <- factor(test_df$`Class Index`, levels = 1:4,
labels = c("World", "Sports", "Business", "Sci/Tech"))
print(paste("Training samples:", nrow(train_df)))
print(paste("Test samples:", nrow(test_df)))
print("Topic distribution in train:")
table(train_df$Topic)
# Step 2: Create and preprocess the text corpus for training data
# Explanation:
# - Corpus: Container for documents (here, Descriptions).
# - Cleaning: Lowercase, remove punctuation/numbers, stopwords (common words like
"the"), extra whitespace.
# - Stemming: Reduce words to root form for normalization.
corpus_train <- Corpus(VectorSource(train_df$Description))
# Basic transformations
corpus_train <- tm_map(corpus_train, content_transformer(tolower)) # Lowercase
corpus_train <- tm_map(corpus_train, removePunctuation) # Remove punctuation
corpus_train <- tm_map(corpus_train, removeNumbers) # Remove numbers
corpus_train <- tm_map(corpus_train, removeWords, stopwords("english")) # Remove
stopwords
corpus_train <- tm_map(corpus_train, stripWhitespace) # Trim spaces
# Stemming
corpus_train <- tm_map(corpus_train, stemDocument, language = "english")
# Create Document-Term Matrix (DTM): Rows=docs, Columns=words, Values=term
frequencies (bag-of-words).
# Control: Limit to top 1,000 frequent terms to reduce sparsity/dimensionality (AG News
has ~50K unique terms).
dtm_train <- DocumentTermMatrix(corpus_train,
control = list(minWordLength = 2, # Words at least 2 chars
bounds = list(global = c(5, Inf)))) # Terms in at least 5 docs
dtm_train <- removeSparseTerms(dtm_train, 0.99) # Remove terms in >99% docs (keep
~1K-2K terms)
print(paste("DTM dimensions (train):", dim(dtm_train)))
# Step 3: Preprocess test data similarly (must match train's vocabulary)
corpus_test <- Corpus(VectorSource(test_df$Description))
corpus_test <- tm_map(corpus_test, content_transformer(tolower))
corpus_test <- tm_map(corpus_test, removePunctuation)
corpus_test <- tm_map(corpus_test, removeNumbers)


corpus_test <- tm_map(corpus_test, removeWords, stopwords("english"))
corpus_test <- tm_map(corpus_test, stripWhitespace)
corpus_test <- tm_map(corpus_test, stemDocument, language = "english")
dtm_test <- DocumentTermMatrix(corpus_test,
control = list(minWordLength = 2,
dictionary = Terms(dtm_train))) # Use train's terms only
dtm_test <- removeSparseTerms(dtm_test, 0.99)
print(paste("DTM dimensions (test):", dim(dtm_test)))
# Step 4: Train Naive Bayes model
# Explanation: Naive Bayes assumes word independence; great for text. Uses Laplace
smoothing for zero counts.
# Train on DTM (features) and topics (labels). Convert DTM to matrix for model input.
model <- naiveBayes(as.matrix(dtm_train), train_df$Topic, laplace = 1)
print("Model trained!")
# Step 5: Predict on test set
# Explanation: Apply model to test DTM. Get predicted topics.
predictions <- predict(model, as.matrix(dtm_test))
# Step 6: Evaluate performance on built-in test set
# Explanation: Compare predictions to true labels. Use confusionMatrix for accuracy,
precision, recall.
conf_matrix <- confusionMatrix(predictions, test_df$Topic)
print("Confusion Matrix (Built-in Test Set):")
print(conf_matrix$table)
print("Overall Accuracy (Built-in Test Set):")
print(conf_matrix$overall['Accuracy'])
# Step 7: Test on a fresh set of articles
# Explanation: Create a small data frame of 5 new, unseen articles (one per topic +mixed).
# These are hardcoded examples to simulate new inputs. Preprocess them identically to
the train/test.
# Predict topics and print results to verify the model generalizes.
new_articles <- data.frame(
Article = c(
"International tensions rise as leaders from Europe and Asia meet to discuss trade
agreements amid global economic uncertainty.", # World/Politics
"The championship game saw an epic overtime finish with the underdog team
clinching victory by a single point.", # Sports
"Wall Street surges after the Federal Reserve announces interest rate cuts, boosting
investor confidence in major tech stocks.", # Business
"Scientists develop a new quantum computing algorithm that could revolutionize data
encryption methods.", # Sci/Tech
"A startup in Silicon Valley secures funding to build AI tools for political campaign
analysis." # Mixed: Business + Politics + Tech),
Expected_Topic = c("World", "Sports", "Business", "Sci/Tech", "Business")
dictionary = Terms(dtm_train)))
dtm_new <- removeSparseTerms(dtm_new, 0.99)



# Predict
new_predictions <- predict(model, as.matrix(dtm_new))
# Output results
results_df <- data.frame(
Article = new_articles$Article,
Expected = new_articles$Expected_Topic,
)
print("Testing on New Articles:")
print(new_articles)
# Preprocess new articles
corpus_new <- Corpus(VectorSource(new_articles$Article))
corpus_new <- tm_map(corpus_new, content_transformer(tolower))
corpus_new <- tm_map(corpus_new, removePunctuation)
corpus_new <- tm_map(corpus_new, removeNumbers)
corpus_new <- tm_map(corpus_new, removeWords, stopwords("english"))
corpus_new <- tm_map(corpus_new, stripWhitespace)
corpus_new <- tm_map(corpus_new, stemDocument, language = "english")
# Create DTM for new articles (using train's dictionary)
dtm_new <- DocumentTermMatrix(corpus_new,
control = list(minWordLength = 2, Predicted = as.character(new_predictions)
)
print("Prediction Results for New Articles:")
print(results_df)
# Manual accuracy on new set (for demo)
manual_accuracy <- mean(new_articles$Expected_Topic == new_predictions)

print(paste("Manual Accuracy on New Articles:", round(manual_accuracy * 100, 2), "%"))
