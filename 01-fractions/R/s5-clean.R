# Create clean Table S5: Baseline characteristics for CPRD Aurum
# Converting all values to numeric, handling special characters

# Create raw data frame with original values
table_s5_raw <- data.frame(
  Characteristic = c(
    "Number of patients",
    "Sex: male",
    "Age, median",
    "Age groups, years",
    "18-29",
    "30-39",
    "40-49",
    "50-59",
    "60-69",
    "70-79",
    "80-89",
    "90+",
    "Prior history, days, median",
    "General conditions (any time prior)",
    "Atrial fibrillation",
    "Cerebrovascular disease",
    "Chronic liver disease",
    "Chronic obstructive lung disease",
    "Coronary arteriosclerosis",
    "Crohn's disease",
    "Dementia",
    "Depressive disorder",
    "Diabetes mellitus",
    "Gastroesophageal reflux disease",
    "Gastrointestinal haemorrhage",
    "Heart disease",
    "Heart failure",
    "Hepatitis C",
    "HIV",
    "Hyperlipidemia",
    "Hypertensive disorder",
    "Ischemic heart disease",
    "Lesion of liver",
    "Obesity",
    "Osteoarthritis",
    "Peripheral vascular disease",
    "Pneumonia",
    "Psoriasis",
    "Pulmonary embolism",
    "Renal impairment",
    "Rheumatoid arthritis",
    "Schizophrenia",
    "Ulcerative colitis",
    "Urinary tract infectious disease",
    "Venous thrombosis",
    "Visual system disorder"
  ),
  CPRD_Aurum = c(
    "88540",
    "48819",
    "73",
    "",
    "45",
    "274",
    "1880",
    "8981",
    "23278",
    "31897",
    "19488",
    "2697",
    "6775",
    "",
    "7056",
    "8298",
    "530",
    "23599",
    "1261",
    "346",
    "1656",
    "12974",
    "12822",
    "3058",
    "6691",
    "24166",
    "4276",
    "126",
    "23",
    "10449",
    "35469",
    "14929",
    "1556",
    "2403",
    "25083",
    "4932",
    "6741",
    "3740",
    "1923",
    "13373",
    "2104",
    "420",
    "472",
    "12018",
    "5252",
    "36552"
  ),
  stringsAsFactors = FALSE
)

# Function to convert a column to numeric, handling special cases
convert_to_numeric <- function(column) {
  # Process each value
  result <- sapply(column, function(value) {
    # If empty, return NA
    if (value == "") {
      return(NA)
    }
    # Handle special cases like "<5"
    else if (value == "<5") {
      return(2) # Set to a conservative value (2) when less than 5
    }
    # Try to convert to numeric
    else {
      # Extract numeric part if there's any non-numeric characters
      return(as.numeric(value))
    }
  })

  return(result)
}

# Create clean version with numeric values
table_s5_clean <- data.frame(
  Characteristic = table_s5_raw$Characteristic,
  CPRD_Aurum = convert_to_numeric(table_s5_raw$CPRD_Aurum)
)

# Write to CSV
write.csv(table_s5_clean, "table_s5_clean.csv", row.names = FALSE)

# Preview the cleaned table
head(table_s5_clean, 20)
