library(dplyr)
library(stringr)
library(purrr)

library("dplyr")
library("lubridate")
library("eeptools")
library("data.table")
library("stringr")
library("writexl")
library("xlsx")


med <- Medications


# Filter the data frame for Patient using anti-hypertensives
filtered_med_ht <- med %>%
  filter(grepl("\\b(ipril|april|upril|opril|capoten|accuretic|renitec|zestril|lisodur|privinil|coversyl|idaprex|perindo|tritace|
               ramace|tryzan|gopten|sartan|atacand|candesan|adesan|teveten|avapro|karvea|cozaar|lozan|olmetec|micardis|pritor|
               diovan|dipine|norvasc|amlo|norvapine|diltiazem|cardizem|vasocardol|felodur|plendil|zanidip|lercadip|adalat|adefin|
               verapamil|isoptin|veracaps|cordilox|olol|noten|tenormin|bicor|dilatrend|talol|trandate|betaloc|lopressor|minax|nebilet|
               visken|inderal|deralin|sotacor|travoprost|thiazide|chlortalidone|hygroton|indapamide|natrilix|dapa-tabs|furosemide|
               lasix|urex|burinex|edecrin|amiloride|amizide|spironolactone|aldactone|spiractin|eplerenone|inspra|doxsig|prazosin|
               minipress|terazosin|hytrin|hydralazine|minoxidil|clonidine|catapres|methyldopa|aldomet|moxonidine|physiotens|exforge|
               avapro|sevikar)\\b",
               DRUGNAME, ignore.case = TRUE)) %>%
  select(Patient_UUID, DRUGNAME, PrescribedDate)


distinct.filtered_med <- filtered_med |>
  distinct(Patient_UUID)

filtered_med_ht <- filtered_med_ht %>%
  filter(!grepl("^SYMBICORT\\b", DRUGNAME, ignore.case = TRUE))

keywords_ht <- "\\b(ipril|april|upril|opril|capoten|accuretic|renitec|zestril|lisodur|privinil|coversyl|idaprex|perindo|tritace|
ramace|tryzan|gopten|sartan|atacand|candesan|adesan|teveten|avapro|karvea|cozaar|lozan|olmetec|micardis|pritor|diovan|
dipine|norvasc|amlo|norvapine|diltiazem|cardizem|vasocardol|felodur|plendil|zanidip|lercadip|adalat|adefin|verapamil|isoptin|
veracaps|cordilox|olol|noten|tenormin|bicor|dilatrend|talol|trandate|betaloc|lopressor|minax|nebilet|visken|inderal|deralin|
sotacor|travoprost|thiazide|chlortalidone|hygroton|indapamide|natrilix|dapa-tabs|furosemide|lasix|urex|burinex|edecrin|amiloride|
amizide|spironolactone|aldactone|spiractin|eplerenone|inspra|doxsig|prazosin|minipress|terazosin|hytrin|hydralazine|minoxidil|
clonidine|catapres|methyldopa|aldomet|moxonidine|physiotens|exforge|sevikar|lopresor|abisart|prilace|twynsta)\\b"



filtered_med_ht <- med %>%
  filter(str_detect(str_to_lower(DRUGNAME), regex(keywords_ht, ignore_case = TRUE))) %>%
  select(Patient_UUID, DRUGNAME, PrescribedDate)



# Step 1: Convert to lower case
keywords_raw <- tolower(antidm$antidm)

# Step 2: Split on common separators
keywords_split <- str_split(keywords_raw, "\\s|\\+|/|,", simplify = FALSE)

# Step 3: Flatten and clean
keywords_clean <- keywords_split %>%
  unlist() %>%
  str_replace_all("[^a-z0-9]", "") %>%       # Remove non-alphanumeric characters
  discard(~ .x == "" | nchar(.x) < 3 | .x %in% c("mg", "tab", "tablet", "capsule", "sr", "xr", "mr"))

# Step 4: Remove duplicates
keywords_unique <- unique(keywords_clean)

# Step 5: Create regex pattern
antidm_pattern <- str_c(keywords_unique, collapse = "|")
print(antidm_pattern)


keywords_dm <- c(
  "acarbose", "actos", "amaryl", "dapagliflozin", "dulaglutide", "empagliflozin", "linagliptin", "glibenclamide",
  "glimepiride", "glipizide", "glucagen", "hypokit", "glucagon","glucophage", "glucose", "glyxambi", "insulin", "glargine",
  "invokana", "janumet", "januvia", "jardiance", "liraglutide","metformin", "nesina", "onglyza", "pioglitazone", "qtern",
  "rosiglitazone","saxagliptin", "saxenda", "sitagliptin", "steglatro", "trulicity", "victoza", "apogliclazide", "apometformin",
  "ardix", "gliclazide", "exenatide", "isophane", "metforminbc", "metforminga", "pharmacor metformin"
)

filtered_med_dm <- med %>%
  filter(str_detect(DRUGNAME_lower, str_c(keywords_dm, collapse = "|"))) %>%
  select(Patient_UUID, DRUGNAME, PrescribedDate)




################### lipid lowering ##############

# Step 1: Convert to lower case
keywords_raw_lipid <- tolower(lipidlow$lipidlow)

# Step 2: Split on common separators
keywords_split_lipid <- str_split(keywords_raw_lipid, "\\s|\\+|/|,", simplify = FALSE)

# Step 3: Flatten and clean
keywords_clean_lipid <- keywords_split_lipid %>%
  unlist() %>%
  str_replace_all("[^a-z0-9]", "") %>%       # Remove non-alphanumeric characters
  discard(~ .x == "" | nchar(.x) < 3 | .x %in% c("mg", "tab", "tablet", "capsule", "sr", "xr", "mr"))

# Step 4: Remove duplicates
keywords_unique_lipid <- unique(keywords_clean_lipid)

# Step 5: Create regex pattern
lipidlow_pattern <- str_c(keywords_unique_lipid, collapse = "|")
print(lipidlow_pattern)

keywords_lipid <- c(
  "atorvastatin", "crestor", "ezetimibe", "simvastatin", "fenofibrate", "fluvastatin", "lescol", "lipitor", "omacor", "pravachol",
  "pravastatin", "rosuvastatin", "vytorin", "zocor", "apoatorvastatin", "apopravastatin", "aporosuvastatin", "aposimvastatin",
  "cholestyramine", "gemfibrozil", "griseostatin", "pravastatinga", "rbx", "simvastatindp"
)

filtered_med_lipid <- med %>%
  filter(str_detect(DRUGNAME_lower, str_c(keywords_lipid, collapse = "|"))) %>%
  select(Patient_UUID, DRUGNAME, PrescribedDate)


########### anti coagulant ##############

# Step 1: Convert to lower case
keywords_raw_coag <- tolower(anticoag$anticoag)

# Step 2: Split on common separators
keywords_split_coag <- str_split(keywords_raw_coag, "\\s|\\+|/|,", simplify = FALSE)

# Step 3: Flatten and clean
keywords_clean_coag <- keywords_split_coag %>%
  unlist() %>%
  str_replace_all("[^a-z0-9]", "") %>%       # Remove non-alphanumeric characters
  discard(~ .x == "" | nchar(.x) < 3 | .x %in% c("mg", "tab", "tablet", "capsule", "sr", "xr", "mr"))

# Step 4: Remove duplicates
keywords_unique_coag <- unique(keywords_clean_coag)

# Step 5: Create regex pattern
anticoag_pattern <- str_c(keywords_unique_coag, collapse = "|")
print(anticoag_pattern)

keywords_anticoag <- c(
  "apixaban", "coumadin", "dabigatran", "etexilate", "dalteparin", "eliquis", "enoxaparin", "fragmin", "heparin", "pradaxa",
  "rivaroxaban", "warfarin", "xarelto", "apoclopidogrel", "aspirin", "clopidogrel", "clopidogrelga", "heparinoid", "heparinoids",
  "prasugrel", "ticagrelor"
)


filtered_med_coag <- med %>%
  filter(str_detect(DRUGNAME_lower, str_c(keywords_anticoag, collapse = "|"))) %>%
  select(Patient_UUID, DRUGNAME, PrescribedDate)

#################### adding DoB #################

unique_dob <- data %>%
  select(Patient_UUID, DateOfBirth) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
filtered_med_ht <- filtered_med_ht %>%
  left_join(unique_dob, by = "Patient_UUID")

filtered_med_dm <- filtered_med_dm %>%
  left_join(unique_dob, by = "Patient_UUID")

filtered_med_lipid <- filtered_med_lipid %>%
  left_join(unique_dob, by = "Patient_UUID")

filtered_med_coag <- filtered_med_coag %>%
  left_join(unique_dob, by = "Patient_UUID")

####################################

# Ensure dates are correctly parsed
filtered_med_ht <- filtered_med_ht %>%
  mutate(
    PrescribedDate = dmy(PrescribedDate)    # Convert PrescribedDate to Date
  )


# Ensure dates are correctly parsed
filtered_med_dm <- filtered_med_dm %>%
  mutate(
    PrescribedDate = dmy(PrescribedDate)    # Convert PrescribedDate to Date
  )


# Ensure dates are correctly parsed
filtered_med_lipid <- filtered_med_lipid %>%
  mutate(
    PrescribedDate = dmy(PrescribedDate)    # Convert PrescribedDate to Date
  )


# Ensure dates are correctly parsed
filtered_med_coag <- filtered_med_coag %>%
  mutate(
    PrescribedDate = dmy(PrescribedDate)    # Convert PrescribedDate to Date
  )

#############Obtaining prescription_age#####################

filtered_med_ht <- filtered_med_ht %>%
  mutate(
    PrescribedDate = as.Date(PrescribedDate),
    DateOfBirth = as.Date(DateOfBirth),
    prescription_age = time_length(interval(DateOfBirth, PrescribedDate), unit = "years")
  )


filtered_med_dm <- filtered_med_dm %>%
  mutate(
    PrescribedDate = as.Date(PrescribedDate),
    DateOfBirth = as.Date(DateOfBirth),
    prescription_age = time_length(interval(DateOfBirth, PrescribedDate), unit = "years")
  )


filtered_med_lipid <- filtered_med_lipid %>%
  mutate(
    PrescribedDate = as.Date(PrescribedDate),
    DateOfBirth = as.Date(DateOfBirth),
    prescription_age = time_length(interval(DateOfBirth, PrescribedDate), unit = "years")
  )

filtered_med_coag <- filtered_med_coag %>%
  mutate(
    PrescribedDate = as.Date(PrescribedDate),
    DateOfBirth = as.Date(DateOfBirth),
    prescription_age = time_length(interval(DateOfBirth, PrescribedDate), unit = "years")
  )

##########################

under_55_med_ht <- filtered_med_ht %>%
  filter(prescription_age < 55) %>%
  distinct(Patient_UUID) %>%
  mutate(med_ht = 1)



BPV_55_CVD_5BP_med <- BPV_55_CVD_S3_5BP_final %>%
  left_join(under_55_med_ht %>% select(Patient_UUID, med_ht), by = "Patient_UUID") %>%
  mutate(med_ht = if_else(is.na(med_ht), 0L, med_ht))


under_55_med_dm <- filtered_med_dm %>%
  filter(prescription_age < 55) %>%
  distinct(Patient_UUID) %>%
  mutate(med_dm = 1)



BPV_55_CVD_5BP_med <- BPV_55_CVD_5BP_med %>%
  left_join(under_55_med_dm %>% select(Patient_UUID, med_dm), by = "Patient_UUID") %>%
  mutate(med_dm = if_else(is.na(med_dm), 0L, med_dm))


under_55_med_lipid <- filtered_med_lipid %>%
  filter(prescription_age < 55) %>%
  distinct(Patient_UUID) %>%
  mutate(med_lipid = 1)



BPV_55_CVD_5BP_med <- BPV_55_CVD_5BP_med %>%
  left_join(under_55_med_lipid %>% select(Patient_UUID, med_lipid), by = "Patient_UUID") %>%
  mutate(med_lipid = if_else(is.na(med_lipid), 0L, med_lipid))


under_55_med_coag <- filtered_med_coag %>%
  filter(prescription_age < 55) %>%
  distinct(Patient_UUID) %>%
  mutate(med_coag = 1)



BPV_55_CVD_5BP_med <- BPV_55_CVD_5BP_med %>%
  left_join(under_55_med_coag %>% select(Patient_UUID, med_coag), by = "Patient_UUID") %>%
  mutate(med_coag = if_else(is.na(med_coag), 0L, med_coag))




