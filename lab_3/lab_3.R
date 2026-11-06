
# Include on first run
# install.packages("naivebayes")

library(naivebayes)

# Load dataset
houses <- read.csv("austinHousingData.csv")

# Check for missingness
sum(is.na(houses))

# See variables in the dataset
colnames(houses)
nrow(houses)
mean(houses$latestPrice)
median(houses$latestPrice)

#Question 1 
#TODO 1
# Perform EDA (Exploratory Data Analysis) by plotting variables against each other

# use plot functions like hist(), boxplot() and plot()
boxplot(houses$latestPrice, houses$latitude)

# Remove unstructured data, in this case text addresses, sale dates, descriptions and RGB images.
houses <- houses[, !names(houses) %in% c("streetAddress", "description","homeImage", "latest_saledate" , "latestPriceSource")]

# Convert data to levels so that we can use Naive Bayes. Naive Bayes assumes a distribution conditional on a discrete label. 
# P(parameter | label)
# To predict house price using it we need labels

houses$priceLevel <- cut(
  houses$latestPrice,
  breaks = quantile(houses$latestPrice, probs = c(0, 1/3, 2/3, 1)),
  labels = c("low", "medium", "high"),
  include.lowest = TRUE
)

# Split the data

set.seed(1234)
idx = sample(seq(nrow(houses)), size = floor(.8 * nrow(houses)))
X_train = houses[idx, !(names(houses) %in% c("priceLevel"))]
y_train = houses[idx, "priceLevel"]

X_test = houses[-idx, !(names(houses) %in% c("priceLevel"))]
y_test = houses[-idx, "priceLevel"]

# Find categorical variables
categorical <- names(X_train)[sapply(X_train, function(x)
  is.factor(x) || is.character(x))]

for (var in categorical) {
  # Ensure test set has the same factors
  X_test[[var]] <- factor(
    X_test[[var]],
    levels = unique(X_train[[var]])
  )
}

# Train the model and evaluate

model = naive_bayes(X_train, y_train, laplace = 1)


preds = predict(model, newdata = X_test)
mean(preds == y_test)
table(prediction = preds, true = y_test)

# Question 2 Model critique
# We now take a look at our finished model and ask if there are any problems?
# TODO 1
# There is data leakage in the model. This means our evaluation is unrealistic and overly optimistic.
# Fix the data leakage and reevaluate the model.

#Inspect model
model$tables

# Split the data with the fix

# your code
set.seed(1234)
idx = sample(seq(nrow(houses)), size = floor(.8 * nrow(houses)))
X_train = houses[idx, !(names(houses) %in% c("priceLevel", "latestPrice"))]
y_train = houses[idx, "priceLevel"]

X_test = houses[-idx, !(names(houses) %in% c("priceLevel", "latestPrice"))]
y_test = houses[-idx, "priceLevel"]

# Find categorical variables
categorical <- names(X_train)[sapply(X_train, function(x)
  is.factor(x) || is.character(x))]

for (var in categorical) {
  # Ensure test set has the same factors
  X_test[[var]] <- factor(
    X_test[[var]],
    levels = unique(X_train[[var]])
  )
}


model = naive_bayes(X_train, y_train, laplace = 1)


preds = predict(model, newdata = X_test)
mean(preds == y_test)
table(prediction = preds, true = y_test)
# Train the model and evaluate


#For numerical variables, naivebayes() assumes a Gaussian distribution. This is a different assumption than many models you may have seen
# like linear regression (which usually assumes errors are Gaussian). So for instance p(property tax rate | high) is Gaussian, in particular
# It is symmetric. Is this a realistic assumption? Plot the conditional continuous variables to see if the Gaussian curve fits.


# Select numerical variables
continuous <- names(X_train)[sapply(X_train, is.numeric)]

#


# Compare (mixed) Gaussian Naive Bayes with categorical Naive Bayes

# Gaussian Naive Bayes assumes numerical variables follow
# a normal distribution conditional on the price level.

# categorical Naive Bayes uses probabilities for categories.
# To use it, we first convert numerical variables into categories.

# Start with the training and test data from Question 1
train_discrete = X_train
test_discrete = X_test


# Divide each numerical variable into three groups
# Use the training data to choose the cutoffs
for (var in continuous) {
  
  breaks = unique(quantile(
    X_train[[var]],
    probs = c(0, 1/3, 2/3, 1),
    na.rm = TRUE
  ))
  
  # Only discretise if there are enough distinct values
  #TODO 3 following the example in question 1
  if (length(breaks) > 2) {
    
    X_train[[var]] <- cut(
      X_train[[var]],
      breaks = quantile(houses$latestPrice, probs = c(0, 1/3, 2/3, 1)),
      include.lowest = TRUE
    )
    
    X_test[[var]] <- cut(
      X_test[[var]],
      breaks = quantile(houses$latestPrice, probs = c(0, 1/3, 2/3, 1)),
      include.lowest = TRUE
    )
  } 
  
  else {
    # Otherwise treat the variable as categorical
    train_discrete[[var]] = factor(X_train[[var]])
    test_discrete[[var]] = factor(
      X_test[[var]],
      levels = levels(train_discrete[[var]])
    )
  }
}

# Make sure all variables are categorical or characters (which are converted to categorical by naive_bayes())
sapply(train_discrete, class)

# Train a Naive Bayes model on categorical data
model_discrete = naive_bayes( train_discrete, y_train, laplace = 1)

# Evaluate the model

preds_discrete = predict(model_discrete, test_discrete)
mean(preds_discrete == y_test)
table(Predicted = preds_discrete, Real = y_test)


# TODO 4
# Compare the two approaches using tables and a plot

barplot(preds, preds_discrete)
