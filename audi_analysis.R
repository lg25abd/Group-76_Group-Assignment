# ===============================================================================
# AUDI DATASET ANALYSIS - DATA ANALYTICS ASSIGNMENT
# Course: 7COM1079

# SECTION 1: LOAD REQUIRED LIBRARIES
# ===============================================================================
# Install packages if not already installed
required_packages <- c("ggplot2", "dplyr", "tidyr", "corrplot", "car", 
                       "moments", "gridExtra", "scales")

for(package in required_packages) {
  if(!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# ===============================================================================
# SECTION 2: DATA LOADING AND INITIAL EXPLORATION
# ===============================================================================

# Load the dataset
audi_data <- read.csv("audi.csv", stringsAsFactors = FALSE)

# Display basic information about the dataset
cat("=== DATASET OVERVIEW ===\n")
cat("Number of observations:", nrow(audi_data), "\n")
cat("Number of variables:", ncol(audi_data), "\n\n")

cat("=== DATASET STRUCTURE ===\n")
str(audi_data)

cat("\n=== FIRST FEW ROWS ===\n")
head(audi_data)

cat("\n=== SUMMARY STATISTICS ===\n")
summary(audi_data)

cat("\n=== MISSING VALUES ===\n")
colSums(is.na(audi_data))

