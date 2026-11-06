library(readxl)

df2025 <- read_excel("2025.xlsx")
df2026 <- read_excel("2026.xlsx") 

cols <- c(
"ATP", "Date", "Tournament",
"Series", "Court", "Surface", "Round",
"Best of", "Winner", "Loser", "WRank", "LRank",
"WPts", "LPts", "Wsets", "Lsets", "AvgW", "AvgL", "Comment"
)
df2025 <- df2025[, cols]
df2026 <- df2026[, cols]
# join data
df <- rbind(df2025, df2026)
head(df)

# convert date column
df$Date <- as.Date(df$Date)
# split into training and test sets
cutoff_date <- as.Date("2026-08-30")