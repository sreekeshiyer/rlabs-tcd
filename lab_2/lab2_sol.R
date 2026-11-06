# run on first use
# -----------------------------------
# install.packages("naivebayes")
# -----------------------------------


library(naivebayes)

# Refresher  --------------------------------------------------------------
# Multinomial Naive-Bayes toy example

# Small training dataframe
df <- data.frame(
  free    = c(2,1,0,0,0,1,0,1),
  win     = c(1,0,2,0,0,0,0,1),
  meeting = c(0,0,0,2,1,1,0,0),
  class   = factor(c("spam","spam","spam","ham","ham","ham","ham","spam"))
)


# Calculate the priors for each class (p(spam), p(ham))
table(df$class)

p_ham <- sum(df$class == "ham") / nrow(df)
p_spam <- sum(df$class == "spam") / nrow(df)

table(df$class)/nrow(df)


#Calculate the conditional probabilities for each word in each class

# WE NEED SMOOTHING HERE:
# If a word never appeared in a class (e.g. "meeting" in spam here),
# then P(meeting | spam) = 0. In the Multinomial NB model, the likelihood
# is a product of all conditional probabilities raised to their word counts.
# If any term is zero and the count for that word in a new query count > 0,
# the entire product becomes zero, wiping out that class probability completely.
#
# Laplace smoothing fixes this by adding a small "pseudo count" alpha to every word:
#   numerator = count(w, c) + alpha
#   denominator = total_words_in_c + alpha * V
# where:
#   alpha = 1  (Laplace smoothing)
#   V = Size of vocabulary (3 here: free, win, meeting)
#
# This prevents zero probabilities and keeps all classes comparable.


# alpha for Laplace smoothing
alpha <- 1

# Using Laplace smoothing, calculate conditional probabilities:
#   P(free | spam), P(win | spam), P(meeting | spam)
#   P(free | ham),  P(win | ham),  P(meeting | ham)
#
# 

# Individual word counts in ham and spam
words <- c("free", "win", "meeting")
counts_ham <- colSums(df[df$class == "ham", words])
counts_spam <- colSums(df[df$class == "spam", words])

# Total amount of words in ham and spam
total_words_ham <- sum(counts_ham)
total_words_spam <- sum(counts_spam)

# Conditional probabilties using Laplace smoothing
# V is the size of the vocabulary
V <- length(words)

p_w_ham <- (counts_ham + alpha) / (total_words_ham + alpha * V)
p_w_spam <- (counts_spam + alpha) / (total_words_spam + alpha * V)


# Compute the probabilities and prediction for a new email (the query)
query <- c(free = 2, win = 1, meeting = 0)


# You now have all the components you need to make predictions using Bayes rule!
# So use your conditional probabilities and priors to calculate:
#
# p(query | spam)
# p(query | ham)
#
# and then the posterior probabilities:
#
# p(spam | query)
# p(ham | query)


# Likelihood
p_x_given_ham <- prod(p_w_ham^query)
p_x_given_spam <- prod(p_w_spam^query)

# unnormalised posterior: Likelihood * prior
u_ham <- p_x_given_ham * p_ham
u_spam <- p_x_given_spam * p_spam

# normalise to get posterior p(ham | query) and p(spam | query)
post_ham <- u_ham / (u_ham + u_spam)
(post_spam <- u_spam / (u_ham + u_spam))


# PREDICTION -----> SPAM!!!!


#Question 1.  -------------------------------------
#Train a Naive Bayes classifier on a messy email dataset 'full_text_emails.csv' on blackboard
# and then test its accuracy on unseen emails.

#Download data set. This is a csv file consisting of [emails as string, label as binary]

emails <- read.csv("full_text_emails.csv")

# Adding more informative names, we create a new variable 'Class' based on the binary variable 'spam'
# This is to make it easier to interpret the results, 
# and it also changes the binary into a factor which is what naivebayes function expects

emails$Class <- factor(
  emails$spam,
  levels = c(0, 1),
  labels = c("ham", "spam")
)

#Data Cleaning ------------------------
# Convert to lower case

text <- tolower(emails$text)

# Remove non alphabetical characters
text <- gsub("[^a-z ]", " ", text)

# Remove excess whitespace
text <- gsub("\\s+", " ", text)

# Split the text string into a list of vectors of strings
words <- strsplit(text, " ")



# Count total occurrences of each word
word_docs <- table(unlist(words))

# Obtain vocabulary
vocab <- names(word_docs)

#TODO 1: Write data into matrix. In the previous exercise we loaded the data in this format.

X <- matrix(
  0,
  nrow = nrow(emails),
  ncol = length(vocab),
  dimnames = list(NULL, vocab)
)

for (i in 1:nrow(emails)) {
  counts <- table(words[[i]])
  X[i, names(counts)] <- counts
}

# TODO 2: Split into train/test data
# We need a dataset that we can learn the parameters of the model on, as well as a dataset we can test its predictions on.
# The standard split is 80/20.
# (Sidenote: Notice that we found the vocabulary before splitting the data. This is easier, but why might it cause a problem?)

# Set a seed for reproducibility 
set.seed(123)

train_idx <- sample(
  seq_len(nrow(emails)),
  size = floor(0.8 * nrow(emails)),
  replace = FALSE
)

X_train <- X[train_idx, ]
X_test <- X[-train_idx, ]

y_train <- emails$Class[train_idx]
y_test <- emails$Class[-train_idx]


# TODO 3: Run naive bayes
multi_NB <- multinomial_naive_bayes(
  x = X_train,
  y = y_train,
  laplace = 1
)

# TODO 4: Get accuracy statistics
pred <- predict(
  multi_NB,
  newdata = X_test,
  
)

table(
  predictions = pred,
  actual = y_test
)

(accuracy <- mean(pred == y_test))

# Question 2 ----------------------
# Write function to predict a new email text -> prediction
# The basic function should take a text string and a model and return a prediction
# Also! You can try to get the function to output a probability

# Example function syntax, this function will add two numbers
add_numbers <- function(x, y){
  sum = x + y
  return(sum)
}
# The name is arbitrary 

predict_new_email <- function(email_text, model){
  text <- tolower(email_text)
  
  # Remove non alphabetical characters
  text <- gsub("[^a-z ]", " ", text)
  
  # Remove excess whitespace
  text <- gsub("\\s+", " ", text)
  
  # Split the text string into a list of vectors of strings
  words <- strsplit(text, " ")
  
  
  
  word_docs <- table(unlist(words))
  
  vocab <- names(model$data$x[1,])
  
  X <- matrix(
    0,
    nrow = 1,
    ncol = length(vocab),
    dimnames = list(NULL, vocab)
  )

  
    counts <- table(words)
    common_words <- intersect(names(counts), vocab)
    X[1, common_words] <- counts[common_words]
    
  
  predict(model, newdata = X)
}
# Question 3
# Write a function to build and test the model
# Experiment with giving your function more parameters to make it easier to run experiments with this dataset

fit_model <- function(data, alpha = 1, split = .8, seed = 42, alphabet_only = T, remove_white = T){
  emails <- read.csv(data)

  
  emails$Class <- factor(
    emails$spam,
    labels = c("ham", "spam")
  )
  text <- emails$text
  
  text <- tolower(emails$text)
  
  # Remove non alphabetical characters
  if(alphabet_only){
  text <- gsub("[^a-z ]", " ", text)
  }
  
  # Remove excess whitespace
  if(remove_white){
  text <- gsub("\\s+", " ", text)
  }
  
  # Split the text string into a list of vectors of strings
  words <- strsplit(text, " ")
  
  
  
  word_docs <- table(unlist(words))
  
  vocab <- names(word_docs)
  
  
  X <- matrix(
    0,
    nrow = nrow(emails),
    ncol = length(vocab),
    dimnames = list(NULL, vocab)
  )
  
  for (i in 1:nrow(emails)) {
    counts <- table(words[[i]])
    X[i, names(counts)] <- counts
  }
  
  # Set a seed for reproducibility 
  set.seed(seed)
  
  train_idx <- sample(
    seq_len(nrow(emails)),
    size = split * nrow(emails),
    replace = FALSE
  )
  
  X_train <- X[train_idx, ]
  X_test <- X[-train_idx, ]
  
  y_train <- emails$Class[train_idx]
  y_test <- emails$Class[-train_idx]
  
  
  # TODO 3: Run naive bayes
  multi_NB <- multinomial_naive_bayes(
    x = X_train,
    y = y_train,
    laplace = alpha
  )
  
  # TODO 4: Get accuracy statistics
  pred <- predict(
    multi_NB,
    newdata = X_test,
    
    )
  
  tab = table(
    predictions = pred,
    actual = y_test
  )
  
  accuracy <- mean(pred == y_test)
  
  return(list("model" = multi_NB, "accuracy" = accuracy, "confusion_matrix" = tab))
}
  
  
  
  