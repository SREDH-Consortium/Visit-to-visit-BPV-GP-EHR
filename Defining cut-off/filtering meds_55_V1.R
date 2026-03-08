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
  filter(grepl(
    paste(
      c("ipril", "april", "upril", "opril", "capoten", "accuretic", "dilol", "renitec", "zestril", "lisodur", "privinil",
        "coversyl", "idaprex", "perindo", "tritace", "ramace", "tryzan", "gopten", "sartan", "atacand", "candesan", "adesan",
        "teveten", "avapro", "karvea", "cozaar", "lozan", "olmetec", "micardis", "pritor", "diovan", "dipine", "norvasc",
        "amlo", "norvapine", "diltiazem", "cardizem", "vasocardol", "felodur", "plendil", "zanidip", "lercadip", "adalat",
        "adefin", "verapamil", "isoptin", "veracaps", "cordilox", "olol", "noten", "tenormin", "bicor", "dilatrend", "talol",
        "trandate", "betaloc", "twynsta", "minax", "nebilet", "visken", "inderal", "deralin", "sotacor", "travoprost",
        "thiazide", "chlortalidone", "hygroton", "indapamide", "natrilix", "dapa-tabs", "furosemide", "lasix", "urex",
        "burinex", "edecrin", "amiloride", "amizide", "spironolactone", "aldactone", "spiractin", "eplerenone", "inspra",
        "doxsig", "prazosin", "minipress", "terazosin", "hytrin", "hydralazine", "minoxidil", "clonidine", "catapres",
        "methyldopa", "aldomet", "moxonidine", "physiotens", "exforge", "sevikar", "lopresor", "abisart", "prilace"),
      collapse = "|"),
    DRUGNAME, ignore.case = TRUE)) %>%
  select(Patient_UUID, DRUGNAME, PrescribedDate)


# lowercase for comparison
med$DRUGNAME_lower <- tolower(med$DRUGNAME)

library(dplyr)
library(stringr)

# Add lowercase drugname column
med <- med %>%
  mutate(DRUGNAME_lower = tolower(DRUGNAME))

# Just use the **core names**, not the full strings
keywords <- c("ramipril", "felodipine", "verapamil", "nifedipine",
              "ipril", "april", "upril", "opril", "capoten", "accuretic",
              "dilol", "renitec", "zestril", "lisodur", "privinil",
              "coversyl", "idaprex", "perindo", "tritace", "ramace",
              "tryzan", "gopten", "sartan", "atacand", "candesan", "adesan",
              "teveten", "avapro", "karvea", "cozaar", "lozan", "olmetec",
              "micardis", "pritor", "diovan", "dipine", "norvasc",
              "amlo", "norvapine", "diltiazem", "cardizem", "vasocardol",
              "felodur", "plendil", "zanidip", "lercadip", "adalat",
              "adefin", "isoptin", "veracaps", "cordilox", "olol",
              "noten", "tenormin", "bicor", "dilatrend", "talol",
              "trandate", "betaloc", "twynsta", "minax", "nebilet",
              "visken", "inderal", "deralin", "sotacor", "travoprost",
              "thiazide", "chlortalidone", "hygroton", "indapamide",
              "natrilix", "dapa-tabs", "furosemide", "lasix", "urex",
              "burinex", "edecrin", "amiloride", "amizide",
              "spironolactone", "aldactone", "spiractin", "eplerenone",
              "inspra", "doxsig", "prazosin", "minipress", "terazosin",
              "hytrin", "hydralazine", "minoxidil", "clonidine",
              "catapres", "methyldopa", "aldomet", "moxonidine",
              "physiotens", "exforge", "sevikar", "lopresor",
              "abisart", "prilace", "caduet")

filtered_med_ht <- med %>%
  filter(str_detect(DRUGNAME_lower, str_c(keywords, collapse = "|"))) %>%
  select(Patient_UUID, DRUGNAME, PrescribedDate)


# Get frequency count
drug_summary_ht <- filtered_med_ht %>%
  group_by(DRUGNAME) %>%
  summarise(count = n()) %>%
  arrange(desc(count))



# Create a lowercase version of DRUGNAME for comparison
filtered_med_ht <- filtered_med_ht %>%
  mutate(DRUGNAME_lower = tolower(DRUGNAME))

# Filter antiHT to only those that are NOT found in any DRUGNAME
unmatched_antiHT <- antiht %>%
  filter(!sapply(tolower(AntiHT), function(keyword) {
    any(str_detect(filtered_med_ht$DRUGNAME_lower, fixed(keyword)))
  }))



distinct.filtered_med <- filtered_med |>
  distinct(Patient_UUID)



# Ensure dates are correctly parsed
filtered_med <- filtered_med %>%
  mutate(
    PrescribedDate = dmy(PrescribedDate)    # Convert PrescribedDate to Date
  )

# Extract the first time a medication was recorded for each Patient_UUID
first_med <- filtered_med %>%
  group_by(Patient_UUID) %>%
  slice_min(order_by = PrescribedDate, n = 1) %>%
  ungroup() %>%
  distinct(Patient_UUID, .keep_all = TRUE)


#write.csv(filtered_med, "filtered_med_V2", row.names = FALSE)
#write_xlsx(filtered_med, "filtered_med.xlsx")

med.l <- filtered_med %>%
  filter(Patient_UUID %in% CPH.l.4656$Patient_UUID)
distinct.med.l <- med.l %>%
  distinct(Patient_UUID)

med.BP <- data %>%
  filter(Patient_UUID %in% distinct.filtered_med$Patient_UUID)

# Extract the first time a medication was recorded for each Patient_UUID
first_med <- filtered_med %>%
  group_by(Patient_UUID) %>%
  slice_min(order_by = PrescribedDate, n = 1) %>%
  ungroup() %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform a left join to add PrescribedDate to med.BP
med.BP.1st.pres <- med.BP %>%
  left_join(first_med %>% select(Patient_UUID, PrescribedDate), by = "Patient_UUID")

# Filter rows where ObservationDate is after PrescribedDate for each Patient_UUID
filtered.med.BP <- med.BP.1st.pres %>%
  filter(ObservationDate > PrescribedDate)

filtered.med.BP %>%
  group_by(Patient_UUID) %>%
  summarise(
    min_obs_date = min(ObservationDate),
    prescribed_date = first(PrescribedDate)
  )

# Ensure dates are correctly parsed
filtered.med.BP <- filtered.med.BP %>%
  mutate(
    ObservationDate = ymd(ObservationDate),  # Convert ObservationDate to Date
    PrescribedDate = dmy(PrescribedDate)    # Convert PrescribedDate to Date
  )


#write.csv(filtered.med.BP, "filtered_med.BP", row.names = FALSE)
#write_xlsx(filtered.med.BP, "filtered_med.BP.xlsx")


filtered.med.BP <- filtered_med

# Filter the data frame for patient with optimal BP after taking anti HT
optimal.BP.pt <- filtered.med.BP %>%
  group_by(Patient_UUID) %>%
  filter(
    any(Observation == "systolic" & ObservationValue < 140) &
      any(Observation == "diastolic" & ObservationValue < 90)
  ) %>%
  ungroup() %>%
  distinct(Patient_UUID)

# Get the BP of patient with optimal BP
filtered.optimal.BP.pt <- filtered.med.BP %>%
  filter(Patient_UUID %in% optimal.BP.pt$Patient_UUID)

summary.stats.filtered.optimal.BP.pt <- filtered.optimal.BP.pt %>%
  summarize(
    mean_BPobs_duration = mean(BPobs_duration, na.rm = TRUE),
    sd_BPobs_duration = sd(BPobs_duration, na.rm = TRUE),
    min_BPobs_duration = min(BPobs_duration, na.rm = TRUE),
    max_BPobs_duration = max(BPobs_duration, na.rm = TRUE)
  )

filtered.optimal.BP.pt.60.syst <- filtered.optimal.BP.pt %>%
  filter(BPobs_duration >=25 & BPobs_duration <= 60 & Observation == "systolic")

distinct.filtered.optimal.BP.pt.60 <- filtered.optimal.BP.pt.60 |>
  distinct(Patient_UUID)

filtered.optimal.BP.pt.60.syst <- filtered.optimal.BP.pt.60.syst %>%
  mutate(ObservationValue = as.numeric(as.character(ObservationValue)))

summary.stats.filtered.optimal.BP.pt.60.syst <- filtered.optimal.BP.pt.60.syst %>%
  summarize(
    mean_BPobs_duration = mean(BPobs_duration, na.rm = TRUE),
    sd_BPobs_duration = sd(BPobs_duration, na.rm = TRUE),
    min_BPobs_duration = min(BPobs_duration, na.rm = TRUE),
    max_BPobs_duration = max(BPobs_duration, na.rm = TRUE),
    mean_Obs_Value = mean(ObservationValue, na.rm = TRUE),
    sd_Obs_Value = sd(ObservationValue, na.rm = TRUE),
    min_Obs_Value = min(ObservationValue, na.rm = TRUE),
    max_Obs_Value = max(ObservationValue, na.rm = TRUE)
  )


