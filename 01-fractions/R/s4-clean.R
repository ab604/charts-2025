# Create clean Table S4: Baseline characteristics stratified by smoking status
# Converting all values to numeric, handling special characters

# Create raw data frame with original values
table_s4_raw <- data.frame(
  Characteristic = c(
    "Number of lung cancer patients",
    "Sex: male",
    "Age (years), median",
    "Age groups (years)",
    "18-29",
    "30-39",
    "40-49",
    "50-59",
    "60-69",
    "70-79",
    "80-89",
    "90+",
    "Prior history, days",
    "General conditions (any time prior)",
    "Atrial Fibrillation",
    "Cerebrovascular Disease",
    "Chronic Liver Disease",
    "Chronic Obstructive Lung Disease",
    "Coronary Arteriosclerosis",
    "Crohn's Disease",
    "Dementia",
    "Depressive Disorder",
    "Diabetes Mellitus",
    "Gastroesophageal Reflux Disease",
    "Gastrointestinal Hemorrhage",
    "Heart Disease",
    "Heart Failure",
    "HIV",
    "Hyperlipidemia",
    "Hypertensive disorder",
    "Ischemic Heart Disease",
    "Obesity",
    "Osteoarthritis",
    "Peripheral Vascular Disease",
    "Pneumonia",
    "Pulmonary Embolism",
    "Renal Impairment",
    "Ulcerative Colitis",
    "Venous Thrombosis"
  ),
  Non_smoker = c(
    "7154",
    "3487",
    "77",
    "",
    "14",
    "38",
    "111",
    "377",
    "1263",
    "2536",
    "2366",
    "449",
    "3626.5",
    "",
    "687",
    "606",
    "25",
    "905",
    "114",
    "19",
    "155",
    "793",
    "1101",
    "229",
    "590",
    "1979",
    "368",
    "<5",
    "823",
    "2420",
    "1115",
    "186",
    "1829",
    "208",
    "377",
    "170",
    "1466",
    "34",
    "463"
  ),
  Former_smoker = c(
    "543",
    "315",
    "72",
    "",
    "0",
    "<5",
    "9",
    "37",
    "182",
    "231",
    "75",
    "8",
    "3198",
    "",
    "36",
    "47",
    "7",
    "181",
    "7",
    "<5",
    "<5",
    "64",
    "73",
    "23",
    "39",
    "148",
    "31",
    "<5",
    "62",
    "152",
    "103",
    "11",
    "117",
    "30",
    "37",
    "<5",
    "67",
    "<5",
    "32"
  ),
  Smoker = c(
    "22019",
    "11446",
    "69",
    "",
    "<5",
    "61",
    "681",
    "3109",
    "7436",
    "7678",
    "2885",
    "165",
    "3676",
    "",
    "1119",
    "1812",
    "153",
    "6588",
    "304",
    "87",
    "305",
    "3721",
    "2406",
    "556",
    "1353",
    "4422",
    "707",
    "7",
    "2227",
    "5587",
    "2728",
    "391",
    "4163",
    "1332",
    "1156",
    "309",
    "2353",
    "60",
    "908"
  ),
  Missing = c(
    "15847",
    "9321",
    "75",
    "",
    "6",
    "29",
    "229",
    "1137",
    "3368",
    "6300",
    "4220",
    "558",
    "3662",
    "",
    "1348",
    "1360",
    "57",
    "3450",
    "250",
    "47",
    "302",
    "1811",
    "1725",
    "448",
    "1117",
    "4131",
    "895",
    "<5",
    "1469",
    "4226",
    "2282",
    "340",
    "3721",
    "685",
    "963",
    "309",
    "2244",
    "83",
    "915"
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
table_s4_clean <- data.frame(
  Characteristic = table_s4_raw$Characteristic,
  Non_smoker = convert_to_numeric(table_s4_raw$Non_smoker),
  Former_smoker = convert_to_numeric(table_s4_raw$Former_smoker),
  Smoker = convert_to_numeric(table_s4_raw$Smoker),
  Missing = convert_to_numeric(table_s4_raw$Missing)
)

# Write to CSV
write.csv(table_s4_clean, "table_s4_clean.csv", row.names = FALSE)

# Preview the cleaned table
head(table_s4_clean, 20)
