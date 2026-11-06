# Exercise 1 --------------------------------------------------------------
# Multinomial Naive-Bayes toy example

# Small training dataframe
df <- data.frame(
  free    = c(2,1,0,0,0,1,0,1),
  win     = c(1,0,2,0,0,0,0,1),
  meeting = c(0,0,0,2,1,1,0,0),
  class   = factor(c("spam","spam","spam","ham","ham","ham","ham","spam"))
)


# TO DO 1: Calculate the priors for each class (p(spam), p(ham))
table(df$class)

# SOLUTION
p_ham <- sum(df$class == "ham") / nrow(df)
p_spam <- sum(df$class == "spam") / nrow(df)


# TO DO 2: Calculate the conditional probabilities for each word in each class

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

# TO DO: Using Laplace smoothing, calculate conditional probabilities:
#   P(free | spam), P(win | spam), P(meeting | spam)
#   P(free | ham),  P(win | ham),  P(meeting | ham)
#
# (Hint: sum word for each class using colSums, then apply the formula above.)

# SOLUTION
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


# TO DO 3: Compute the probabilities and prediction for a new email (the query)
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

# SOLUTION
# Likelihood
p_x_given_ham <- prod(p_w_ham^query)
p_x_given_spam <- prod(p_w_spam^query)

# unnormalised posterior: Likelihood * prior
u_ham <- p_x_given_ham * p_ham
u_spam <- p_x_given_spam * p_spam

# normalise to get posterior p(ham | query) and p(spam | query)
post_ham <- u_ham / (u_ham + u_spam)
post_spam <- u_spam / (u_ham + u_spam)

# PREDICTION -----> SPAM!!!!



# Exercise 2 --------------------------------------------------------------
# Apply a Multinomial Naive Bayes classifier using the "naivebayes" package

# TO DO 1: Load required package and the email_clean.csv dataset
# TO DO 2: Split the dataset into training (90%) and test (10%) sets
# TO DO 3: Fit a multinomial Naive Bayes model with laplace = 1
# TO DO 4: Use predict() to classify the test set
# TO DO 5: Create and print a confusion matrix using table()
# TO DO 6: Compute the model accuracy and interpret it
# -------------------------------------------------------------------------



# TO DO 1: Load required package and the email_clean.csv dataset
install.packages("naivebayes")
library(naivebayes)

# Load dataset (could take a few seconds to load)
emails <- read.csv("email_clean.csv")

# Last column is the target feature "Class" !!! CAPITAL "C" !!!
colnames(emails)[ncol(emails)]
head(emails[, ncol(emails)])

emails$Class <- as.factor(emails$Class)


# TO DO 2: Split the dataset into training (90%) and test (10%) sets

# Set a seed for reproducibility 
set.seed(1234)

# Create indices to split into test/train
train_idx <- sample(seq_len(nrow(emails)), size = 0.9 * nrow(emails), replace = FALSE)

emails_train <- emails[train_idx, ] # Remember that "emails" is 2 dimensional
emails_test <- emails[-train_idx, ]

# Split into feature and target variables
X_train <- emails_train[, -ncol(emails_train)] # Capital "X"
y_train <- emails_train[, "Class"]

X_test <- emails_test[, -ncol(emails_test)] # Capital "X"
y_test <- emails_test[, "Class"]



# TO DO 3: Fit a multinomial Naive Bayes model with laplace = 1 using the
# "naivebayes" package

# Find the right function using ?"package_name-package", check the help page
# (?function_name) for required parameters
?"naivebayes-package"

multi_NB <- multinomial_naive_bayes(x = as.matrix(X_train), y = y_train, laplace = 1)



# TO DO 4: Use predict() to classify the test set (newdata = emails_test)

pred <- predict(multi_NB, newdata = as.matrix(X_test))



# TO DO 5: Print a confusion matrix using table() to evaluate the performance

table(predictions = pred, actual = y_test)



# TO DO 6: Compute the model accuracy and interpret it

mean(pred == emails_test$Class)



