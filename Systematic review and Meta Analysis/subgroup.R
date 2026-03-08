#install.packages("forestplot")
library("metafor")
library("meta")
library("ggplot2")
library("forestplot")
library("dplyr")

#predicted pooled risk ratio and corresponding CI/PI
predict(res1CV_EHR, transf=exp, digits=2)

dtmetareg <- all_SBP_V2
inf <- metagen (dtmetareg$logHR, dtmetareg$selogHR, studlab = paste(dtmetareg$Author), sm = "HR")
inf
sub_metrics <- update(inf, subgroup = dtmetareg$Metric, tau.common = FALSE)
sub_metrics

# Step 1: Filter data before creating meta object
dtmetareg_noNA <- dtmetareg[!is.na(dtmetareg$NumberBP_cat), ]

# Step 2: Recreate meta-analysis object with filtered data
# (replace this line with your original call to 'metagen' or 'rma')
inf_noNA <- metagen(dtmetareg_noNA$logHR, dtmetareg_noNA$selogHR, data = dtmetareg_noNA, sm = "HR")  # example

# Step 3: Run subgroup analysis
sub_NumberBP <- update(inf_noNA, subgroup = dtmetareg_noNA$NumberBP_cat, tau.common = FALSE)

sub_NumberBP

############Atrial Fibrillation########

# Step 1: Filter data before creating meta object
dtmetareg_noNA <- dtmetareg[dtmetareg$AF != "NA" & !is.na(dtmetareg$AF), ]

# Step 2: Recreate meta-analysis object with filtered data
# (replace this line with your original call to 'metagen' or 'rma')
inf_noNA <- metagen(dtmetareg_noNA$logHR, dtmetareg_noNA$selogHR, data = dtmetareg_noNA, sm = "HR")  # example

# Step 3: Run subgroup analysis
sub_AF <- update(inf_noNA, subgroup = dtmetareg_noNA$AF, tau.common = FALSE)

sub_AF


# Remove rows with NA in NumberBP_cat before updating
dtmetareg_noNA <- dtmetareg[!is.na(dtmetareg$NumberBP_cat), ]

# Recreate or update meta object with filtered data
inf_noNA <- update(inf, data = dtmetareg_noNA)

# Now run subgroup analysis
sub_NumberBP <- update(inf_noNA, subgroup = dtmetareg_noNA$NumberBP_cat, tau.common = FALSE)


inf <- metagen (dtmetareg$logHR, dtmetareg$selogHR, studlab = paste(dtmetareg$Author), sm = "HR")
inf
subEHR <- update(inf, subgroup = dtmetareg$Source, tau.common = FALSE)
subEHR

subPopulation <- update.meta(inf, subgroup = dtmetareg$population, tau.common = FALSE)
subPopulation

subage <- update.meta(inf, subgroup = dtmetareg$mean_age, tau.common = FALSE)
subage



update.meta(inf, subgroup = dtmetareg$mean_age, tau.common = FALSE)

update.meta(inf, subgroup = dtmetareg$FU, tau.common = FALSE)

update.meta(inf, subgroup = dtmetareg$BPVobs, tau.common = FALSE)

update.meta(inf, subgroup = dtmetareg$PrevCVD, tau.common = FALSE)

update.meta(inf, subgroup = dtmetareg$eGFR, tau.common = FALSE)

update.meta(inf, subgroup = dtmetareg$`BPV Metrics`, tau.common = FALSE)


data <- all_SBP_V2
SDonly <- subset (data, Metric == "SD")



SDonly_inf <- metagen (SDonly$logHR, SDonly$selogHR, studlab = paste(SDonly$Author), sm = "HR")
SDonly_inf

sub_populationSD <- update(SDonly_inf, subgroup = SDonly$population, tau.common = FALSE)
sub_populationSD

update(SDonly_inf, subgroup = SDonly$Source, tau.common = FALSE)

update(SDonly_inf, subgroup = SDonly$FU, tau.common = FALSE)

update(SDonly_inf, subgroup = SDonly$eGFR, tau.common = FALSE)

update(SDonly_inf, subgroup = SDonly$PrevCVD, tau.common = FALSE)

update(SDonly_inf, subgroup = SDonly$population, tau.common = FALSE)


#####EHR subgroup in VIM only
#data <- CVD_1_SD_sensitivity_V2
data <- all
VIMonly <- subset (data, Metric == "VIM")
VIMonly_inf <- metagen (VIMonly$logHR, VIMonly$selogHR, studlab = paste(VIMonly$Author), sm = "HR")
VIMonly_inf

update(VIMonly_inf, subgroup = VIMonly$Source, tau.common = FALSE)

update(VIMonly_inf, subgroup = VIMonly$FU, tau.common = FALSE)

update(VIMonly_inf, subgroup = VIMonly$eGFR, tau.common = FALSE)

update(VIMonly_inf, subgroup = VIMonly$PrevCVD, tau.common = FALSE)

update(VIMonly_inf, subgroup = VIMonly$population, tau.common = FALSE)

update(VIMonly_inf, subgroup = VIMonly$mean_age, tau.common = FALSE)


#####EHR subgroup in CV only
CVonly <- subset (data, Metric == "CV")
CVonly_inf <- metagen (CVonly$logHR, CVonly$selogHR, studlab = paste(CVonly$Author), sm = "HR")
CVonly_inf
update(CVonly_inf, subgroup = CVonly$Source, tau.common = FALSE)

update(CVonly_inf, subgroup = CVonly$mean_age, tau.common = FALSE)

update(CVonly_inf, subgroup = CVonly$FU, tau.common = FALSE)

update(CVonly_inf, subgroup = CVonly$eGFR, tau.common = FALSE)

update(CVonly_inf, subgroup = CVonly$PrevCVD, tau.common = FALSE)

update(CVonly_inf, subgroup = CVonly$population, tau.common = FALSE)

#####EHR subgroup in ARV only
ARVonly <- subset (data, Metric == "ARV")
ARVonly_inf <- metagen (ARVonly$logHR, ARVonly$selogHR, studlab = paste(ARVonly$Author), sm = "HR")
ARVonly_inf
update(ARVonly_inf, subgroup = ARVonly$Source, tau.common = FALSE)

update(ARVonly_inf, subgroup = ARVonly$mean_age, tau.common = FALSE)

update(ARVonly_inf, subgroup = ARVonly$FU, tau.common = FALSE)

update(ARVonly_inf, subgroup = ARVonly$eGFR, tau.common = FALSE)

update(ARVonly_inf, subgroup = ARVonly$PrevCVD, tau.common = FALSE)

update(ARVonly_inf, subgroup = ARVonly$population, tau.common = FALSE)
