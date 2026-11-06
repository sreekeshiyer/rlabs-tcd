# lintr: disable=line_length_linter
# ==============================================================================
# R CHEATSHEET: DATA INGESTION, PREPROCESSING, EDA & NAIVE BAYES CLASSIFICATION
# ==============================================================================
# File: ~/projects/r_labs/references/reference.R
# Purpose: Comprehensive syntax reference for data analytics workflows in R:
#   1. Package Management & Setup
#   2. Reading CSV Files (All major R functions & packages)
#   3. Reading Excel Files (.xlsx, .xls) (All major R functions & packages)
#   4. Renaming Columns & Variable Names (Base R, dplyr, janitor)
#   5. Changing Data Formats & Types (Type casting, dates, factors)
#   6. Exploratory Data Analysis (EDA) (Summary stats, frequencies, visualizations)
#   7. Data Preprocessing & Cleaning (Missing values, filtering, feature scaling)
#   8. Train/Test Data Splitting (Base R, caret, caTools)
#   9. Naive Bayes Classification (e1071, naivebayes, klaR, caret)
#  10. Model Evaluation & Performance Metrics (Confusion matrix, ROC, AUC)
#  11. Quick Syntax Summary & Cheat Sheet Card
# ==============================================================================


# ==============================================================================
# SECTION 1: PACKAGE MANAGEMENT & SETUP
# ==============================================================================

# List of all required packages for this complete pipeline
required_packages <- c(
  "readr",       # Fast, tidy CSV reading
  "data.table",  # fread: ultra-fast data ingestion
  "readxl",      # Modern Excel reading (.xls and .xlsx)
  "openxlsx",    # Reading and writing Excel files without Java dependency
  "rio",         # Universal import/export wrapper
  "dplyr",       # Data wrangling, manipulation, and transformations
  "tidyr",       # Tidying data, handling missing values
  "janitor",     # Data cleaning and column name standardization
  "ggplot2",     # Data visualization and EDA plots
  "e1071",       # Classic implementation of Naive Bayes and SVM
  "naivebayes",  # Dedicated, high-speed Naive Bayes with kernel density support
  "klaR",        # Alternative Naive Bayes implementation
  "caret",       # Unified machine learning workflow, data splitting, tuning
  "pROC",        # ROC curves and Area Under the Curve (AUC) calculation
  "caTools"      # Utility for stratified train/test splitting
)

# Automated package installer (installs only if not already present)
install_if_missing <- function(packages) {
  missing_pkgs <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(missing_pkgs) > 0) {
    message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs, dependencies = TRUE)
  } else {
    message("All required packages are already installed.")
  }
}

# Uncomment the following line to install missing libraries:
# install_if_missing(required_packages)

# Load core packages quietly
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})


# ==============================================================================
# SECTION 2: READING CSV FILES (ALL MAJOR METHODS)
# ==============================================================================
# In R, there are 4 primary ways to read CSV files:
#   1. Base R: read.csv() / read.csv2() / read.table()
#   2. readr: read_csv()
#   3. data.table: fread()
#   4. rio: import()

# We use the famous Titanic dataset hosted publicly on GitHub as our internet dataset: # nolint: line_length_linter.
dataset_url <- "https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv" # nolint: line_length_linter.

# ------------------------------------------------------------------------------
# 2.1 Base R: read.csv() & read.table()
# ------------------------------------------------------------------------------
# Syntax: read.csv(file, header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("NA", ""))
#
# Key Parameters:
# - file: file path (local) or direct web URL (character string)
# - header: TRUE if the first row contains column headers
# - sep: field separator character (default is ",")
# - stringsAsFactors: should string columns be converted to factors? (Default: FALSE in R >= 4.0)
# - na.strings: character vector of strings to interpret as NA missing values
# - nrows: maximum number of rows to read (useful for testing big files)
# - skip: number of lines to skip before beginning to read data

df_base <- read.csv(
  file = dataset_url,
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE,
  na.strings = c("", " ", "NA", "NULL", "?")
)

# read.csv2() is designed for European CSV files (sep = ";" and dec = ",")
# df_euro <- read.csv2("european_data.csv")

# read.table() is the general-purpose parent function:
# df_table <- read.table("data.txt", header = TRUE, sep = "\t")

# ------------------------------------------------------------------------------
# 2.2 Tidyverse / readr: read_csv()
# ------------------------------------------------------------------------------
# Advantages: Fast, returns a tibble, auto-detects column types, never converts
# strings to factors automatically, displays a concise parsing summary.
#
# Syntax: readr::read_csv(file, col_names = TRUE, na = c("", "NA"), col_types = NULL)

if (requireNamespace("readr", quietly = TRUE)) {
  df_readr <- readr::read_csv(
    file = dataset_url,
    col_names = TRUE,
    na = c("", " ", "NA", "NULL", "?"),
    show_col_types = FALSE  # Set to TRUE to inspect parsed column types
  )
}

# ------------------------------------------------------------------------------
# 2.3 data.table: fread()
# ------------------------------------------------------------------------------
# Advantages: Blazing fast, memory efficient, best choice for files > 100MB.
# Returns a data.table (which also inherits from data.frame).
#
# Syntax: data.table::fread(input, sep = "auto", header = "auto", na.strings = "NA")

if (requireNamespace("data.table", quietly = TRUE)) {
  df_fread <- data.table::fread(
    input = dataset_url,
    header = TRUE,
    sep = ",",
    na.strings = c("", " ", "NA", "NULL", "?"),
    data.table = FALSE  # Set to FALSE if you want a standard data.frame returned
  )
}

# ------------------------------------------------------------------------------
# 2.4 rio: import()
# ------------------------------------------------------------------------------
# Advantages: Single universal command that auto-detects format from file extension.

if (requireNamespace("rio", quietly = TRUE)) {
  # df_rio <- rio::import("my_file.csv")
}


# ==============================================================================
# SECTION 3: READING EXCEL FILES (.XLSX & .XLS)
# ==============================================================================
# Excel workbooks contain sheets and formatting. There are 3 main packages in R:
#   1. readxl: read_excel(), read_xlsx(), read_xls() (Fast C++ backend, no Java)
#   2. openxlsx: read.xlsx() (No Java, provides rich read/write/styling)
#   3. rio: import() with sheet specification

# To make this runnable offline, let's create a temporary local Excel file from our data:
temp_excel_path <- file.path(tempdir(), "titanic_sample.xlsx")
if (requireNamespace("openxlsx", quietly = TRUE)) {
  openxlsx::write.xlsx(
    list(Passengers = df_base[1:100, ], Metadata = data.frame(Source = "Titanic", N = 100)),
    file = temp_excel_path
  )
}

# ------------------------------------------------------------------------------
# 3.1 readxl: read_excel(), read_xlsx(), read_xls()
# ------------------------------------------------------------------------------
# Syntax:
#   readxl::read_excel(path, sheet = NULL, range = NULL, col_names = TRUE, na = "")
#
# Key Parameters:
# - path: local path to the Excel file
# - sheet: sheet name (character, e.g., "Sheet1") or sheet index (numeric, e.g., 1)
# - range: specific cell range to read (e.g., "B2:G50" or "Passengers!A1:E20")
# - col_names: TRUE to use first row as headers, or a character vector of custom names
# - na: character vector of strings to interpret as missing values
# - skip: number of rows to skip before reading data
# - n_max: maximum number of data rows to read

if (requireNamespace("readxl", quietly = TRUE) && file.exists(temp_excel_path)) {
  # 1. List all available sheet names in the workbook:
  available_sheets <- readxl::excel_sheets(temp_excel_path)
  cat("Available sheets in Excel file:", available_sheets, "\n")
  
  # 2. Read by sheet index (e.g., sheet 1):
  df_excel_idx <- readxl::read_excel(temp_excel_path, sheet = 1)
  
  # 3. Read by sheet name:
  df_excel_name <- readxl::read_excel(temp_excel_path, sheet = "Passengers")
  
  # 4. Read specific cell range:
  # df_excel_range <- readxl::read_excel(temp_excel_path, range = "A1:E20")
  
  # 5. Format-specific functions:
  # df_xlsx <- readxl::read_xlsx("data.xlsx", sheet = 1)
  # df_xls  <- readxl::read_xls("data.xls", sheet = 1)
}

# ------------------------------------------------------------------------------
# 3.2 openxlsx: read.xlsx()
# ------------------------------------------------------------------------------
# Syntax: openxlsx::read.xlsx(xlsxFile, sheet = 1, startRow = 1, colNames = TRUE)

if (requireNamespace("openxlsx", quietly = TRUE) && file.exists(temp_excel_path)) {
  df_openxlsx <- openxlsx::read.xlsx(
    xlsxFile = temp_excel_path,
    sheet = 1,
    startRow = 1,
    colNames = TRUE,
    detectDates = TRUE  # Automatically convert Excel serial date numbers to Date objects
  )
}

# ------------------------------------------------------------------------------
# 3.3 rio: import() for Excel
# ------------------------------------------------------------------------------
if (requireNamespace("rio", quietly = TRUE) && file.exists(temp_excel_path)) {
  # df_rio_excel <- rio::import(temp_excel_path, which = "Passengers")
}


# ==============================================================================
# SECTION 4: RENAMING COLUMNS & VARIABLE NAMES
# ==============================================================================
# We will work with a copy of df_base to demonstrate all renaming techniques.
df_clean <- df_base

# Let's inspect initial column names:
cat("Original column names:\n", names(df_clean), "\n\n")

# ------------------------------------------------------------------------------
# 4.1 Base R Renaming
# ------------------------------------------------------------------------------
# Method A: Rename by index position
names(df_clean)[1] <- "passenger_id"

# Method B: Rename by matching old name dynamically
names(df_clean)[names(df_clean) == "Pclass"] <- "passenger_class"

# Method C: Rename all columns simultaneously by assigning a full vector
# colnames(df_clean) <- c("id", "survived", "pclass", "name", "sex", "age",
#                         "sib_sp", "parch", "ticket", "fare", "cabin", "embarked")

# ------------------------------------------------------------------------------
# 4.2 dplyr::rename() & dplyr::rename_with()
# ------------------------------------------------------------------------------
# Syntax: rename(new_name = old_name)
df_clean <- df_clean %>%
  dplyr::rename(
    survived      = Survived,
    passenger_sex = Sex,
    ticket_fare   = Fare
  )

# Rename dynamically using functions with rename_with():
# Convert all remaining uppercase columns to lowercase:
df_clean <- df_clean %>%
  dplyr::rename_with(.fn = tolower)

# Replace dots or special characters in column names:
df_clean <- df_clean %>%
  dplyr::rename_with(.fn = ~ gsub("\\.", "_", .x))

# ------------------------------------------------------------------------------
# 4.3 janitor::clean_names() (Industry Standard Best Practice)
# ------------------------------------------------------------------------------
# Automatically converts all column names to clean, consistent snake_case,
# strips punctuation, replaces spaces with underscores, and handles camelCase.

if (requireNamespace("janitor", quietly = TRUE)) {
  df_clean <- janitor::clean_names(df_clean)
  cat("Cleaned column names after janitor::clean_names():\n", names(df_clean), "\n\n")
}


# ==============================================================================
# SECTION 5: CHANGING DATA FORMATS & TYPES (TYPE CASTING)
# ==============================================================================
# Proper data types are CRITICAL for Naive Bayes:
# - Categorical predictors & target: MUST be factors in R!
# - Continuous numeric predictors: MUST be numeric / integer.
# - Text/ID columns: character (usually dropped prior to modeling).

# Inspect current structure and data types:
str(df_clean)
sapply(df_clean, class)

# ------------------------------------------------------------------------------
# 5.1 Base R Type Conversions
# ------------------------------------------------------------------------------
# Convert target 'survived' to factor with descriptive labels:
df_clean$survived <- factor(
  df_clean$survived,
  levels = c(0, 1),
  labels = c("No", "Yes")  # 0 = Did not survive, 1 = Survived
)

# Convert passenger class to an ordered factor:
df_clean$passenger_class <- factor(
  df_clean$passenger_class,
  levels = c(1, 2, 3),
  labels = c("1st", "2nd", "3rd"),
  ordered = TRUE
)

# Convert sex to standard nominal factor:
df_clean$passenger_sex <- as.factor(df_clean$passenger_sex)

# Convert embarked port to factor:
df_clean$embarked <- as.factor(df_clean$embarked)

# Ensure continuous features are numeric:
df_clean$age         <- as.numeric(df_clean$age)
df_clean$ticket_fare <- as.numeric(df_clean$ticket_fare)
df_clean$sibsp       <- as.integer(df_clean$sibsp)
df_clean$parch       <- as.integer(df_clean$parch)

# Date formatting reference (if dataset contained dates):
# df_clean$date_col <- as.Date(df_clean$date_col, format = "%Y-%m-%d")
# lubridate::ymd("2026-11-06")

# ------------------------------------------------------------------------------
# 5.2 Tidyverse / dplyr Batch Type Conversions
# ------------------------------------------------------------------------------
# You can transform multiple columns at once using dplyr::mutate(across(...)):
#
# df_clean <- df_clean %>%
#   mutate(
#     across(c(passenger_sex, embarked), as.factor),
#     across(c(age, ticket_fare), as.numeric),
#     across(where(is.character), as.factor)
#   )


# ==============================================================================
# SECTION 6: EXPLORATORY DATA ANALYSIS (EDA)
# ==============================================================================
# EDA reveals distributions, missingness patterns, class balance, and anomalies.

cat("\n--- 6.1 Basic Dimensions & Head ---\n")
dim(df_clean)       # Rows and columns
head(df_clean, 3)   # First 3 rows
tail(df_clean, 3)   # Last 3 rows

cat("\n--- 6.2 Statistical Summary ---\n")
summary(df_clean)   # Comprehensive 5-number summary + factors

cat("\n--- 6.3 Missing Values Audit ---\n")
# Total NAs in entire dataframe:
total_nas <- sum(is.na(df_clean))
cat("Total NA values:", total_nas, "\n")

# Count of NAs per column:
na_count <- colSums(is.na(df_clean))
print(na_count[na_count > 0])

# Percentage of NAs per column:
na_pct <- round(colMeans(is.na(df_clean)) * 100, 2)
print(na_pct[na_pct > 0])

cat("\n--- 6.4 Categorical Distributions & Contingency Tables ---\n")
# 1. Target variable frequency & proportions:
table(df_clean$survived)
prop.table(table(df_clean$survived))  # Check for class imbalance

# 2. Two-way contingency table (Survival vs Sex):
tab_sex_surv <- table(Sex = df_clean$passenger_sex, Survived = df_clean$survived)
print(tab_sex_surv)

# Row proportions: Probability of survival conditioned on sex P(Survived | Sex)
prop.table(tab_sex_surv, margin = 1)

# Two-way contingency table (Survival vs Class):
tab_class_surv <- table(Class = df_clean$passenger_class, Survived = df_clean$survived)
prop.table(tab_class_surv, margin = 1)

# 3. Multi-way cross-tabulation with xtabs():
xtabs(~ passenger_sex + passenger_class + survived, data = df_clean)

cat("\n--- 6.5 Correlation Analysis (Continuous Variables) ---\n")
numeric_vars <- df_clean %>%
  select(where(is.numeric)) %>%
  select(-passenger_id)  # Drop identifier

# Correlation matrix (use = "complete.obs" handles missing values)
cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(round(cor_matrix, 2))

# ------------------------------------------------------------------------------
# 6.6 Visual EDA with ggplot2
# ------------------------------------------------------------------------------
# Visual 1: Target Class Balance (Bar Chart)
p1 <- ggplot(df_clean, aes(x = survived, fill = survived)) +
  geom_bar(alpha = 0.8, color = "black") +
  scale_fill_manual(values = c("No" = "#E64B35", "Yes" = "#4DBBD5")) +
  theme_minimal() +
  labs(title = "Target Class Distribution: Titanic Survival",
       x = "Survived", y = "Passenger Count")

# Visual 2: Age Distribution by Survival Status (Density Plot)
p2 <- ggplot(df_clean, aes(x = age, fill = survived)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  labs(title = "Age Density Distribution by Survival",
       x = "Age (Years)", y = "Density")

# Visual 3: Survival Rate by Passenger Class & Sex (Faceted Bar Plot)
p3 <- ggplot(df_clean, aes(x = passenger_class, fill = survived)) +
  geom_bar(position = "fill", alpha = 0.85, color = "black") +
  facet_wrap(~ passenger_sex) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(title = "Survival Proportions across Class & Sex",
       x = "Passenger Class", y = "Percentage", fill = "Survived")

# Note: In an interactive session or RStudio, run: print(p1); print(p2); print(p3)


# ==============================================================================
# SECTION 7: DATA PREPROCESSING & CLEANING
# ==============================================================================

# ------------------------------------------------------------------------------
# 7.1 Handling Missing Values
# ------------------------------------------------------------------------------
# Notice that:
# - 'age' has missing values (~177 NAs).
# - 'embarked' has 2 missing values.
# - 'cabin' has >75% missing values (we will drop cabin).

# Approach A: Impute missing numerical values using Median (robust to outliers)
median_age <- median(df_clean$age, na.rm = TRUE)
df_clean$age[is.na(df_clean$age)] <- median_age
cat("Imputed missing age values with median:", median_age, "\n")

# Approach B: Impute missing categorical values with Mode (most frequent)
mode_embarked <- names(which.max(table(df_clean$embarked)))
df_clean$embarked[is.na(df_clean$embarked)] <- mode_embarked
# Drop unused levels if any:
df_clean$embarked <- droplevels(df_clean$embarked)

# Approach C: Listwise deletion (if you prefer removing rows with any NA):
# df_complete <- na.omit(df_clean)
# df_complete <- tidyr::drop_na(df_clean)

# ------------------------------------------------------------------------------
# 7.2 Feature Selection (Dropping Irrelevant / Leaky Columns)
# ------------------------------------------------------------------------------
# Identifiers and raw text columns (e.g. passenger_id, name, ticket, cabin)
# are not suitable for Naive Bayes without NLP feature engineering.
# We retain only the clean predictors and target:

model_data <- df_clean %>%
  dplyr::select(
    survived,          # Target variable (factor: "No", "Yes")
    passenger_class,   # Categorical predictor (factor)
    passenger_sex,     # Categorical predictor (factor)
    age,               # Continuous numeric predictor
    sibsp,             # Integer predictor (siblings/spouses aboard)
    parch,             # Integer predictor (parents/children aboard)
    ticket_fare,       # Continuous numeric predictor
    embarked           # Categorical predictor (factor)
  )

# Verify no remaining NAs exist in model dataset:
stopifnot(sum(is.na(model_data)) == 0)
cat("\nFinal model dataset preview:\n")
glimpse(model_data)

# ------------------------------------------------------------------------------
# 7.3 Feature Scaling (Optional for Naive Bayes)
# ------------------------------------------------------------------------------
# NOTE: Naive Bayes estimates continuous features using Gaussian probability
# density function (normal distribution: mean and variance). Feature scaling
# (standardization or min-max) is NOT strictly necessary for Naive Bayes
# because each feature's mean and variance are calculated independently.
# However, if scaling is desired:
#
# Z-Score standardization: scale(x)
# Min-Max Normalization: (x - min(x)) / (max(x) - min(x))


# ==============================================================================
# SECTION 8: TRAIN / TEST DATA SPLITTING
# ==============================================================================
# Always set a random seed for reproducible results!
set.seed(42)

# ------------------------------------------------------------------------------
# 8.1 Method 1: caret::createDataPartition() (Recommended: Stratified Split)
# ------------------------------------------------------------------------------
# Maintains identical class proportions of the target variable in both sets.

if (requireNamespace("caret", quietly = TRUE)) {
  train_index <- caret::createDataPartition(
    y = model_data$survived,
    p = 0.80,          # 80% Training, 20% Testing
    list = FALSE
  )
  
  train_data <- model_data[train_index, ]
  test_data  <- model_data[-train_index, ]
  
  cat("\nCaret Stratified Split:\n")
  cat("Train set size:", nrow(train_data), "| Test set size:", nrow(test_data), "\n")
  cat("Train class balance:\n")
  print(prop.table(table(train_data$survived)))
  cat("Test class balance:\n")
  print(prop.table(table(test_data$survived)))
}

# ------------------------------------------------------------------------------
# 8.2 Method 2: Base R sample()
# ------------------------------------------------------------------------------
# sample_idx <- sample(seq_len(nrow(model_data)), size = 0.8 * nrow(model_data))
# train_data <- model_data[sample_idx, ]
# test_data  <- model_data[-sample_idx, ]

# ------------------------------------------------------------------------------
# 8.3 Method 3: caTools::sample.split()
# ------------------------------------------------------------------------------
# if (requireNamespace("caTools", quietly = TRUE)) {
#   split_vector <- caTools::sample.split(model_data$survived, SplitRatio = 0.8)
#   train_data   <- subset(model_data, split_vector == TRUE)
#   test_data    <- subset(model_data, split_vector == FALSE)
# }


# ==============================================================================
# SECTION 9: NAIVE BAYES CLASSIFICATION
# ==============================================================================
# Theoretical Primer:
# Naive Bayes relies on Bayes' Theorem:
#   P(Y | X1, X2, ..., Xn) = [ P(Y) * P(X1|Y) * P(X2|Y) * ... * P(Xn|Y) ] / P(X)
#
# Assumptions:
# 1. "Naive" Conditional Independence: All predictors X_i are conditionally
#    independent given the class label Y.
# 2. Continuous variables: Typically modeled with Gaussian Normal Distribution.
# 3. Categorical variables: Modeled via multinomial / frequency counts.
#
# Zero-Frequency Problem:
# If a category in the test set never appeared in the training set for class Y,
# P(X_i | Y) = 0, causing the entire posterior product to collapse to zero!
# SOLUTION: Laplace Smoothing (laplace = 1 or fL = 1) adds a pseudocount.

# ------------------------------------------------------------------------------
# 9.1 Package 1: e1071::naiveBayes() (Most Popular & Classic)
# ------------------------------------------------------------------------------
# Syntax:
#   naiveBayes(formula, data, laplace = 0)
#   naiveBayes(x = predictors, y = target, laplace = 0)

if (requireNamespace("e1071", quietly = TRUE)) {
  cat("\n==================================================================\n")
  cat("9.1 Fitting Naive Bayes with e1071\n")
  cat("==================================================================\n")
  
  # Fit model with Laplace smoothing = 1
  nb_e1071 <- e1071::naiveBayes(
    formula = survived ~ .,
    data    = train_data,
    laplace = 1
  )
  
  # Print the model to inspect:
  # - A-priori class probabilities: P(Y)
  # - Conditional probability tables for categorical variables: P(X|Y)
  # - Mean and Standard Deviation for continuous variables (Gaussian assumption)
  print(nb_e1071)
  
  # Inspect specific conditional probability table for passenger_sex:
  cat("\nConditional Probabilities for passenger_sex given Survival:\n")
  print(nb_e1071$tables$passenger_sex)
  
  # Inspect Gaussian parameters (mean, sd) for continuous feature age:
  cat("\nGaussian Parameters (Mean, SD) for Age given Survival:\n")
  print(nb_e1071$tables$age)
  
  # ----------------------------------------------------------------------------
  # Predictions with e1071
  # ----------------------------------------------------------------------------
  # 1. Predict discrete class labels (type = "class"):
  pred_class_e1071 <- predict(nb_e1071, newdata = test_data, type = "class")
  
  # 2. Predict posterior probabilities (type = "raw"):
  pred_probs_e1071 <- predict(nb_e1071, newdata = test_data, type = "raw")
  
  cat("\nPredicted Probabilities (First 5 test rows):\n")
  print(head(pred_probs_e1071, 5))
}

# ------------------------------------------------------------------------------
# 9.2 Package 2: naivebayes::naive_bayes() (Modern, Fast & Flexible)
# ------------------------------------------------------------------------------
# Advantages: Allows non-parametric Kernel Density Estimation (KDE) for continuous
# features (usekernel = TRUE) instead of assuming a Gaussian bell curve.

if (requireNamespace("naivebayes", quietly = TRUE)) {
  cat("\n==================================================================\n")
  cat("9.2 Fitting Naive Bayes with 'naivebayes' package\n")
  cat("==================================================================\n")
  
  # Fit with Gaussian continuous features + Laplace smoothing:
  nb_modern <- naivebayes::naive_bayes(
    formula   = survived ~ .,
    data      = train_data,
    laplace   = 1,
    usekernel = FALSE  # Set to TRUE to use Kernel Density Estimation
  )
  
  # Predictions:
  pred_class_modern <- predict(nb_modern, newdata = test_data, type = "class")
  pred_probs_modern <- predict(nb_modern, newdata = test_data, type = "prob")
  
  # Visualizing marginal distributions conditioned on class:
  # plot(nb_modern, which = "age")
}

# ------------------------------------------------------------------------------
# 9.3 Package 3: klaR::NaiveBayes()
# ------------------------------------------------------------------------------
# Syntax: klaR::NaiveBayes(formula, data, fL = 1, usekernel = FALSE)

if (requireNamespace("klaR", quietly = TRUE)) {
  # nb_klar <- klaR::NaiveBayes(
  #   formula   = survived ~ .,
  #   data      = train_data,
  #   fL        = 1,        # Factor for Laplace correction
  #   usekernel = TRUE      # Use KDE for continuous features
  # )
  # pred_klar <- predict(nb_klar, newdata = test_data)
  # pred_klar$class         # predicted classes
  # pred_klar$posterior     # posterior probabilities
}

# ------------------------------------------------------------------------------
# 9.4 Package 4: caret::train() (Tuning & K-Fold Cross-Validation)
# ------------------------------------------------------------------------------
# caret provides a unified interface to tune Laplace smoothing and kernel density.

if (requireNamespace("caret", quietly = TRUE)) {
  cat("\n==================================================================\n")
  cat("9.4 Fitting Naive Bayes with caret Cross-Validation\n")
  cat("==================================================================\n")
  
  # Define 5-Fold Cross-Validation training control
  cv_control <- caret::trainControl(
    method          = "cv",
    number          = 5,
    classProbs      = TRUE,
    summaryFunction = twoClassSummary  # Computes ROC, Sensitivity, Specificity
  )
  
  # Hyperparameter tuning grid:
  # laplace: smoothing parameter
  # usekernel: Gaussian vs Kernel density
  # adjust: bandwidth adjustment for kernel
  nb_grid <- expand.grid(
    laplace   = c(0, 1),
    usekernel = c(FALSE, TRUE),
    adjust    = 1
  )
  
  # Train model across folds (suppress warnings from zero-density bins if any)
  nb_caret_fit <- suppressWarnings(
    caret::train(
      form      = survived ~ .,
      data      = train_data,
      method    = "naive_bayes",
      trControl = cv_control,
      tuneGrid  = nb_grid,
      metric    = "ROC"
    )
  )
  
  print(nb_caret_fit)
  
  # Predictions using best tuned model:
  pred_class_caret <- predict(nb_caret_fit, newdata = test_data)
  pred_probs_caret <- predict(nb_caret_fit, newdata = test_data, type = "prob")
}


# ==============================================================================
# SECTION 10: MODEL EVALUATION & PERFORMANCE METRICS
# ==============================================================================
# Using predictions from e1071 (pred_class_e1071 and pred_probs_e1071):

cat("\n==================================================================\n")
cat("10. MODEL EVALUATION & PERFORMANCE METRICS\n")
cat("==================================================================\n")

# ------------------------------------------------------------------------------
# 10.1 Confusion Matrix (caret::confusionMatrix)
# ------------------------------------------------------------------------------
# Syntax: confusionMatrix(data = predictions, reference = actual, positive = "Yes")
conf_matrix <- caret::confusionMatrix(
  data      = pred_class_e1071,
  reference = test_data$survived,
  positive  = "Yes"
)

print(conf_matrix)

# ------------------------------------------------------------------------------
# 10.2 Metric Breakdown & Definitions
# ------------------------------------------------------------------------------
# Extract table counts:
# Table layout:
#               Reference (Actual)
# Prediction    Yes       No
#    Yes        TP        FP
#    No         FN        TN
cm_table <- conf_matrix$table

tp <- cm_table["Yes", "Yes"]  # True Positive
tn <- cm_table["No", "No"]    # True Negative
fp <- cm_table["Yes", "No"]   # False Positive
fn <- cm_table["No", "Yes"]   # False Negative

accuracy    <- (tp + tn) / sum(cm_table)
sensitivity <- tp / (tp + fn)               # Recall: True Positive Rate
specificity <- tn / (tn + fp)               # True Negative Rate
precision   <- tp / (tp + fp)               # Positive Predictive Value
f1_score    <- 2 * (precision * sensitivity) / (precision + sensitivity)

cat("\n--- Manual Metric Verification ---\n")
cat(sprintf("Accuracy:    %.4f\n", accuracy))
cat(sprintf("Sensitivity: %.4f (Recall)\n", sensitivity))
cat(sprintf("Specificity: %.4f\n", specificity))
cat(sprintf("Precision:   %.4f (Positive Predictive Value)\n", precision))
cat(sprintf("F1-Score:    %.4f\n", f1_score))

# ------------------------------------------------------------------------------
# 10.3 ROC Curve & AUC (Area Under the Curve)
# ------------------------------------------------------------------------------
if (requireNamespace("pROC", quietly = TRUE)) {
  # Probability of positive class ("Yes")
  prob_positive <- pred_probs_e1071[, "Yes"]
  
  # Calculate ROC object:
  roc_curve <- pROC::roc(
    response  = test_data$survived,
    predictor = prob_positive,
    levels    = c("No", "Yes")  # negative level first, positive level second
  )
  
  # Compute Area Under the Curve (AUC):
  auc_val <- pROC::auc(roc_curve)
  cat(sprintf("\nArea Under the ROC Curve (AUC): %.4f\n", auc_val))
  
  # Plot ROC curve (optional in interactive session):
  # plot(
  #   roc_curve,
  #   col = "#1B9E77",
  #   lwd = 3,
  #   main = paste0("ROC Curve - Naive Bayes (AUC = ", round(auc_val, 3), ")")
  # )
  # abline(a = 0, b = 1, lty = 2, col = "darkgray")
}


# ==============================================================================
# SECTION 11: QUICK SYNTAX SUMMARY & CHEAT SHEET CARD
# ==============================================================================
# Keep this snippet handy for daily copying and pasting!
#
# +------------------------+---------------------------------------------------+
# | Task                   | Recommended Syntax / Function                     |
# +------------------------+---------------------------------------------------+
# | Read CSV (Standard)    | read.csv("data.csv", na.strings = c("", "NA"))    |
# | Read CSV (Fast/Tidy)   | readr::read_csv("data.csv", na = c("", "NA"))     |
# | Read CSV (Big Data)    | data.table::fread("data.csv", data.table = FALSE) |
# | Read Excel (.xlsx)     | readxl::read_excel("data.xlsx", sheet = 1)        |
# | List Excel Sheets      | readxl::excel_sheets("data.xlsx")                 |
# | Rename Columns         | dplyr::rename(new_name = old_name)                |
# | Clean Column Names     | janitor::clean_names(df)                          |
# | Check Data Types       | sapply(df, class)  OR  str(df)                    |
# | Convert to Factor      | factor(x, levels = c(...), labels = c(...))       |
# | Convert to Numeric     | as.numeric(as.character(x))                       |
# | Count Missing Values   | colSums(is.na(df))                                |
# | Impute Missing Values  | ifelse(is.na(x), median(x, na.rm=TRUE), x)        |
# | Cross-Tabulation       | table(df$var1, df$var2)                           |
# | Proportions Table      | prop.table(table(df$var1, df$var2), margin = 1)   |
# | Train/Test Split       | caret::createDataPartition(y, p = 0.8, list = F)  |
# | Naive Bayes Model      | e1071::naiveBayes(y ~ ., data = train, laplace=1) |
# | Predict Class Labels   | predict(model, newdata = test, type = "class")    |
# | Predict Probabilities  | predict(model, newdata = test, type = "raw")      |
# | Confusion Matrix       | caret::confusionMatrix(pred, actual, pos="Yes")   |
# | ROC & AUC Calculation  | pROC::roc(response, prob_yes); pROC::auc(...)     |
# +------------------------+---------------------------------------------------+
# ==============================================================================
