#install.packages("metafor")
#install.packages("forestplot")
#install.packages("dplyr")
library("metafor")
library("forestplot")
library("dplyr")
library("meta")



#Analyse on 1-SD increase only
dtSDonly <- SDonly

res1SD <- rma(yi=logHR, sei=selogHR, data = dtSDonly)
res1SD
#predicted pooled risk ratio and corresponding CI/PI
predict(res1SD, transf=exp, digits=2)

#forest plot
forest(res1SD)
forest(res1SD, addpred=TRUE, header=TRUE)
print(forest(res1SD, addpred=TRUE, header=TRUE))
forest(res1SD, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(res1SD, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp)
forest(res1SD, addpred=TRUE, showweights = TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, at=log(c(.5, .75, 1, 1.25, 2.25)), 
       slab=paste(dtSDonly$Author, sep = ", "))

# Analyse on CV only
#data <- CVD_1_SD_sensitivity_V2
#dtCVonly <- subset (data, BPVMet == "CV-SBP")

dtCVonly <- CVonly

res1CV <- rma(yi=logHR, sei=selogHR, data = dtCVonly)
res1CV

#predicted pooled risk ratio and corresponding CI/PI
predict(res1CV, transf=exp, digits=2)
#forest plot
forest(res1CV)
forest(res1CV, addpred=TRUE, header=TRUE)
print(forest(res1CV, addpred=TRUE, header=TRUE))
forest(res1CV, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(res1CV, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp)
forest(res1CV, addpred=TRUE, showweights = TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, at=log(c(.5, .75, 1, 1.25, 2.25)), 
       slab=paste(dtCVonly$Author, sep = ", "))


# Analyse on ARV only

#dtARVonly <- subset (data, BPVMet == "ARV-SBP")

dtARVonly <- ARVonly


res1ARV <- rma(yi=logHR, sei=selogHR, data = dtARVonly)
res1ARV

#predicted pooled risk ratio and corresponding CI/PI
predict(res1ARV, transf=exp, digits=2)
#forest plot
forest(res1ARV)
forest(res1ARV, addpred=TRUE, header=TRUE)
print(forest(res1ARV, addpred=TRUE, header=TRUE))
forest(res1ARV, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(res1ARV, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp)
forest(res1ARV, addpred=TRUE, showweights = TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, at=log(c(.5, .75, 1, 1.25, 2.25)), 
       slab=paste(dtARVonly$Author, sep = ", "))



# Analyse on VIM only

#dtVIMonly <- subset (data, BPVMet == "VIM-SBP")

dtVIMonly <- VIMonly

res1VIM <- rma(yi=logHR, sei=selogHR, data = dtVIMonly)
res1VIM

#predicted pooled risk ratio and corresponding CI/PI
predict(res1VIM, transf=exp, digits=2)
#forest plot
forest(res1VIM)
forest(res1VIM, addpred=TRUE, header=TRUE)
print(forest(res1VIM, addpred=TRUE, header=TRUE))
forest(res1VIM, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(res1VIM, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp)
forest(res1VIM, addpred=TRUE, showweights = TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, at=log(c(.5, .75, 1, 1.25, 2.25)), 
       slab=paste(dtVIMonly$Author, sep = ", "))

# Analyse on all studies

#dtallstudies <- subset (data, BPVMet == "VIM-SBP")

all <- all_SBP_V2

resall <- rma(yi=logHR, sei=selogHR, data = all)
resall


#predicted pooled risk ratio and corresponding CI/PI
predict(resall, transf=exp, digits=2)
#forest plot
forest(resall)

forest(resall, addpred=TRUE, header=TRUE)
print(forest(resall, addpred=TRUE, header=TRUE))
forest(resall, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(resall, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, showweights = TRUE)
forest(resall, addpred = TRUE, showweights = TRUE, header = TRUE, xlim = c(-1.29, 2), 
       atransf = exp, at = log(c(.5, .75, 1, 1.25, 2.25)), 
       slab = paste(all$Author, all$"BPV Metric", all$Outcome, sep = "  "), 
       cex = 0.65)

all <- all %>%
  rename (DataSource = `Data Source`)

all <- all_SBP_V2

resall <- rma(yi=logHR, sei=selogHR, data = all)
resall


#predicted pooled risk ratio and corresponding CI/PI
predict(resall, transf=exp, digits=2)
#forest plot
forest(resall)

# Calculate the confidence intervals
all$lower <- exp(all$logHR - 1.96 * all$selogHR)
all$upper <- exp(all$logHR + 1.96 * all$selogHR)
# Calculate the Weight as the inverse variance (1 / variance of logHR)
# Weight formula: 1 / (selogHR^2)
all$Weight <- 1 / (all$selogHR^2)

# Adding padding to the DataSource column
all$DataSource <- paste0("  ", all$DataSource, "  ")  # Add spaces before and after the text

# Create a label matrix for additional columns
label_matrix <- cbind(
  all$Author,
  all$Year,
  all$Outcome,
  all$DataSource
)

# Create a matrix for Weight and Estimate (95% CI)
# Estimate format: HR [lower CI, upper CI]
weight_estimate_matrix <- cbind(
  paste0(round(all$Weight, 1), "%"),                            # Weight
  paste0(round(all$HR, 2), " [", round(all$lower, 2), ", ", round(all$upper, 2), "]")  # Estimate (95% CI)
)

# Plot the forest plot
forestplot(
  labeltext = label_matrix,                          # Add Study, Year, Outcome, Data Source
  mean = all$HR,                                     # Hazard Ratio (HR)
  lower = all$lower,                                 # Lower Confidence Interval
  upper = all$upper,                                 # Upper Confidence Interval
  zero = 1,                                          # Reference line at HR = 1
  xlab = "Hazard Ratio",                             # Label for the x-axis
  title = "Forest Plot with Additional Columns",     # Title of the plot
  new_page = TRUE,                                   # Start new page for the plot
  boxsize = 0.2,                                     # Size of the boxes for HR estimates
  lineheight = unit(2, "cm"),                        # Increase line height for more spacing
  col = fpColors(box="black", lines="gray"),         # Color for the boxes and lines
  is.summary = FALSE,                                # Remove summary style
  txt_gp = fpTxtGp(ticks = gpar(cex = 0.3),          # Decrease font size for ticks
                   label = gpar(cex = 0.3),          # Decrease font size for labels
                   xlab  = gpar(cex = 0.3)),         # Decrease font size for x-axis label
  graph.pos = 4,                                     # Position the graph in the fourth column
  graphwidth = unit(10, "cm"),                       # Increase the width of the plot
  align = c("l", "r", "r", "r", "r"),                # Align the labels (left for Study, right for other columns)
  hrzl_lines = gpar(col = "black"),                  # Add horizontal lines for separation
  rtext = weight_estimate_matrix,                    # Add Weight and Estimate (95% CI) on the right
  clip = c(0.5, 3),                                  # Clip the HR axis between 0.5 and 3 for better visualization
  mar = unit(c(1, 1, 1, 3), "cm")                    # Add margins to prevent text from overlapping
)


all2 <- all_DBP_V2

resall2 <- rma(yi=yi, sei=sei, data = all2)
resall2

#predicted pooled risk ratio and corresponding CI/PI
predict(resall2, transf=exp, digits=2)
#forest plot
forest(resall2)
forest(resall2, addpred=TRUE, header=TRUE)
print(forest(resall2, addpred=TRUE, header=TRUE))
forest(resall2, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(resall2, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, showweights = TRUE)
forest(resall2, addpred = TRUE, showweights = TRUE, header = TRUE, xlim = c(-1.29, 2), 
       atransf = exp, at = log(c(.5, .75, 1, 1.25, 2.25)), 
       slab = paste(all$Author, all$"BPV Metric", all$Outcome, sep = "  "), 
       cex = 0.65)





library(stringr)
warnings()

# Define num_rows based on the number of rows in dtallstudies
num_rows <- nrow(all)

# Example forest plot code
# Construct custom labels with left-aligned columns
custom_labels <- paste(
  str_pad(all$Author, width = 15, side = "right"),
  str_pad(all$Year, width = 10, side = "right"),
  str_pad(all$BPVMet, width = 10, side = "right"),
  str_pad(all$Outcome, width = 15, side = "right"),
  str_pad(all$dsource, width = 10, side = "right")
)

# Set the margins of the plot
par(mar = c(5, 12, 4, 2) + 0.1)  # Increase left margin for better label visibility

# Create the forest plot
forest(resall, addpred = TRUE, showweights = TRUE, header = FALSE, xlim = c(-3, 2), 
       atransf = exp, at = log(c(.5, .75, 1, 1.25, 2.25)), 
       slab = custom_labels, cex = 0.65, cex.axis = 0.65, cex.lab = 0.65,
       ylim = c(1, nrow(all) + 3), font = 2, family = "mono")  # Adjust ylim as needed

 # Add custom column headers
text(+1.7, nrow(all) + 4, "Author", pos = 4, cex = 0.65, font = 2)
text(-2.6, nrow(all) + 4, "BPV Metric", pos = 4, cex = 0.65, font = 2)
text(-2.2, nrow(all) + 4, "Outcome", pos = 4, cex = 0.65, font = 2)
text(+ 1.2, nrow(all) + 4, "Weight", pos = 4, cex = 0.65, font = 2)
text(+1.4, nrow(all) + 4, "Estimate [95% CI]", pos = 4, cex = 0.65, font = 2)



library(ggplot2)

# Analyse on non-EHR data
dt4 <- CVD_1_SD_increase_HR_non_EHR


res1SD_non_EHR <- rma(yi=yi, sei=sei, data = dt4)
res1SD_non_EHR


#install.packages("forestplot")
#install.packages("dplyr")
library("forestplot")
library("dplyr")


#predicted pooled risk ratio and corresponding CI/PI
predict(res1SD_non_EHR, transf=exp, digits=2)
#forest plot
forest(res1SD_non_EHR)
forest(res1SD_non_EHR, addpred=TRUE, header=TRUE)
print(forest(res1SD_non_EHR, addpred=TRUE, header=TRUE))
forest(res1SD_non_EHR, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(res1SD_non_EHR, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp)
forest(res1SD_non_EHR, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, at=log(c(.5, .75, 1, 1.25, 2.5)),
       slab=paste(dt4$Author, dt4$Year, sep = ", "))

#funnel plot
funnel(res)

# Analyse on DBP non EHR data

dtDBPn <- CVD_1_SD_increase_DBP_non_EHR

resDBP_nonEHR <- rma(yi=yi, sei=sei, data = dtDBPn)
resDBP_nonEHR

#predicted pooled risk ratio and corresponding CI/PI
predict(resDBP_nonEHR, transf=exp, digits=2)
#forest plot
forest(resDBP_nonEHR)
forest(resDBP_nonEHR, addpred=TRUE, header=TRUE)
print(forest(resDBP_nonEHR, addpred=TRUE, header=TRUE))
forest(resDBP_nonEHR, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(resDBP_nonEHR, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp)
forest(resDBP_nonEHR, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, at=log(c(.5, .75, 1, 1.25, 2)), 
       slab=paste(dtDBPn$Author, dtDBPn$Year, sep = ", "))

# Analyse on DBP EHR data

dtDBPn2 <- CVD_1_SD_increase_DBP_EHR

resDBP_EHR <- rma(yi=yi, sei=sei, data = dtDBPn2)
resDBP_EHR

#predicted pooled risk ratio and corresponding CI/PI
predict(resDBP_nonEHR, transf=exp, digits=2)
#forest plot
forest(resDBP_nonEHR)
forest(resDBP_nonEHR, addpred=TRUE, header=TRUE)
print(forest(resDBP_nonEHR, addpred=TRUE, header=TRUE))
forest(resDBP_nonEHR, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(resDBP_nonEHR, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp)
forest(resDBP_nonEHR, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, at=log(c(.5, .75, 1, 1.25, 2)), 
       slab=paste(dtDBPn$Author, dtDBPn$Year, sep = ", "))


library("metafor")
dt5 <- CVD_1_SD_increase_HR_EHR_DM

res4 <- rma(yi=yi, sei=sei, data = dt5)
res4
#install.packages("forestplot")
#install.packages("dplyr")
#library("forestplot")
#library("dplyr")

#predicted pooled risk ratio and corresponding CI/PI
predict(res4, transf=exp, digits=2)
#forest plot
forest(res4)
forest(res4, addpred=TRUE, header=TRUE)
print(forest(res4, addpred=TRUE, header=TRUE))
forest(res4, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(res4, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp)
forest(res4, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, at=log(c(.37, .75, 1, 1.5, 3)))


dt6 <- CVD_1_SD_increase_HR_non_EHR_non_DM

res5 <- rma(yi=yi, sei=sei, data = dt6)
res5
#install.packages("forestplot")
#install.packages("dplyr")
#library("forestplot")
#library("dplyr")

#predicted pooled risk ratio and corresponding CI/PI
predict(res5, transf=exp, digits=2)
#forest plot
forest(res5)
forest(res5, addpred=TRUE, header=TRUE)
print(forest(res5, addpred=TRUE, header=TRUE))
forest(res5, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(res5, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp)
forest(res5, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, at=log(c(.37, .75, 1, 1.5, 3)))

dt7 <- CVD_1_SD_increase_HR_non_EHR_non_DM

res5 <- rma(yi=yi, sei=sei, data = dt6)
res5
#install.packages("forestplot")
#install.packages("dplyr")
#library("forestplot")
#library("dplyr")

#predicted pooled risk ratio and corresponding CI/PI
predict(res5, transf=exp, digits=2)
#forest plot
forest(res5)
forest(res5, addpred=TRUE, header=TRUE)
print(forest(res5, addpred=TRUE, header=TRUE))
forest(res5, addpred=TRUE, header=TRUE, xlim=c(-1.29,2))
forest(res5, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp)
forest(res5, addpred=TRUE, header=TRUE, xlim=c(-1.29,2), atransf=exp, at=log(c(.37, .75, 1, 1.5, 3)))

#funnel plot
funnel(res)


