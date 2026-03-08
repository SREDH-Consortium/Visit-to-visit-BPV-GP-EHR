library("metafor")
library("forestplot")
library("dplyr")
library("meta")

all <- all_SBP_V2

# Meta-analysis model for the pooled HR
resall <- rma(yi=logHR, sei=selogHR, data = all)

# Extract predicted pooled HR and corresponding CI
predicted_resall <- predict(resall, transf = exp, digits = 2)

# Get pooled HR and confidence intervals
pooled_hr <- round(predicted_resall$pred, 2)
pooled_lower <- round(predicted_resall$ci.lb, 2)
pooled_upper <- round(predicted_resall$ci.ub, 2)

# Calculate the individual study confidence intervals
all$lower <- round(exp(all$logHR - 1.96 * all$selogHR), 2)
all$upper <- round(exp(all$logHR + 1.96 * all$selogHR), 2)

# Combine label matrix with the BPV Metric column and individual study data
label_matrix <- rbind(
  cbind(
    all$Author,
    all$Year,
    all$Outcome,
    all$Source,   # Data Source
    all$Metric,   # BPV Metric
    paste0(round(all$HR, 2), " [", round(all$lower, 2), ", ", round(all$upper, 2), "]")  # Estimate (95% CI)
  ),
  c("RE Model", "", "", "", "", paste0(pooled_hr, " [", pooled_lower, ", ", pooled_upper, "]"))  # Pooled result row
)

# Save the forest plot as a high-resolution PNG
png("forest_plot_high_res_SBP5.png", width = 15, height = 15, units = "in", res = 300)

# Adjust your forest plot settings
forestplot(
  labeltext = label_matrix,                          # Add Study, Year, Outcome, Data Source, BPV Metric, and Estimate (95% CI)
  mean = c(all$HR, pooled_hr),                       # HR for individual studies + pooled HR
  lower = c(all$lower, pooled_lower),                # Lower CI for individual studies + pooled lower CI
  upper = c(all$upper, pooled_upper),                # Upper CI for individual studies + pooled upper CI
  zero = 1,                                          # Reference line at HR = 1
  xlab = "Hazard Ratio",                             # Label for the x-axis
  title = "Forest Plot of Visit-to-Visit Systolic BPV",  # Title of the plot
  new_page = TRUE,                                   # Start new page for the plot
  boxsize = c(rep(0.2, nrow(all)), 0.3),             # Box size for studies + pooled result (bigger for pooled)
  lineheight = unit(1.8, "cm"),                      # Increase line height for better spacing
  col = fpColors(box = "black", lines = "gray"),     # Color for boxes and lines
  is.summary = c(rep(FALSE, nrow(all)), TRUE),       # Mark the pooled HR row as a summary
  txt_gp = fpTxtGp(ticks = gpar(cex = 0.7),          # Adjust font size for ticks
                   label = gpar(cex = 0.65),         # Adjust font size for labels
                   xlab  = gpar(cex = 0.65)),        # Adjust font size for x-axis label
  graph.pos = 6,                                     # Shift the graph to the right column
  graphwidth = unit(10, "cm"),                       # Increase graph width if needed for the BPV Metric column
  align = c("l", "l", "l", "l", "l", "l", "c"),      # Align the labels: Study name left, other columns right
  hrzl_lines = gpar(col = "black"),                  # Add horizontal lines for separation
  clip = c(0.5, 3),                                  # Clip the HR axis between 0.5 and 3 for better visualization
  mar = unit(c(1, 1, 1, 5), "cm")                    # Adjust margins for better spacing
)

# Close the device to save the file
dev.off()

#### SD Only #######
SD <- SDonly2

# Meta-analysis model for the pooled HR
resSD <- rma(yi = logHR, sei = selogHR, data = SD)

# Extract predicted pooled HR and corresponding CI
predicted_resSD <- predict(resSD, transf = exp, digits = 2)

# Get pooled HR and confidence intervals
pooled_hr_sd <- round(predicted_resSD$pred, 2)
pooled_lower_sd <- round(predicted_resSD$ci.lb, 2)
pooled_upper_sd <- round(predicted_resSD$ci.ub, 2)

# Calculate the individual study confidence intervals
SD$lower <- round(exp(SD$logHR - 1.96 * SD$selogHR), 2)
SD$upper <- round(exp(SD$logHR + 1.96 * SD$selogHR), 2)

# Combine label matrix with the BPV Metric column and individual study data
label_matrix <- rbind(
  cbind(
    SD$Author,
    SD$Year,
    SD$Outcome,
    SD$Source,   # Data Source
    SD$Metric,   # BPV Metric
    paste0(round(SD$HR, 2), " [", round(SD$lower, 2), ", ", round(SD$upper, 2), "]")  # Estimate (95% CI)
  ),
  c("RE Model", "", "", "", "", paste0(pooled_hr_sd, " [", pooled_lower_sd, ", ", pooled_upper_sd, "]"))  # Pooled result row
)

# Save the forest plot as a high-resolution PNG
png("forest_plot_high_res_SD6.png", width = 15, height = 15, units = "in", res = 300)

# Adjust your forest plot settings
forestplot(
  labeltext = label_matrix,                          # Add Study, Year, Outcome, Data Source, BPV Metric, and Estimate (95% CI)
  mean = c(SD$HR, pooled_hr_sd),                     # HR for individual studies + pooled HR
  lower = c(SD$lower, pooled_lower_sd),              # Lower CI for individual studies + pooled lower CI
  upper = c(SD$upper, pooled_upper_sd),              # Upper CI for individual studies + pooled upper CI
  zero = 1,                                          # Reference line at HR = 1
  xlab = "Hazard Ratio",                             # Label for the x-axis
  title = "Forest Plot of Visit-to-Visit Systolic BPV based on Standard Deviation",  # Title of the plot
  new_page = TRUE,                                   # Start new page for the plot
  boxsize = c(rep(0.2, nrow(SD)), 0.3),              # Box size for studies + pooled result (bigger for pooled)
  lineheight = unit(0.1, "cm"),                      # Line height for spacing
  col = fpColors(box = "black", lines = "gray"),     # Color for boxes and lines
  is.summary = c(rep(FALSE, nrow(SD)), TRUE),        # Mark the pooled HR row as a summary
  txt_gp = fpTxtGp(
    ticks = gpar(cex = 0.6),
    label = gpar(cex = 0.6),
    xlab  = gpar(cex = 0.6)
  ),
  graph.pos = 6,                                     # Shift the graph to the right column
  graphwidth = unit(10, "cm"),                       # Increase graph width if needed for the BPV Metric column
  align = c("l", "l", "l", "l", "l", "l", "l"),      # Align the labels: Study name left, other columns right
  hrzl_lines = gpar(col = "black"),                  # Add horizontal lines for separation
  clip = c(0.5, 3),                                  # Clip the HR axis between 0.5 and 3 for better visualization
  mar = unit(c(0.5, 0.5, 0.5, 3), "cm")              # Reduce margins
)

# Close the device to save the file
dev.off()

###### All DBP ############

allD <- all_DBP_V2

# Calculate the pooled hazard ratio using a random-effects model
pooled_result <- rma(yi = logHR, sei = selogHR, data = allD, method = "REML")

# Extract the pooled HR and confidence intervals
pooled_hr <- exp(pooled_result$b)
pooled_lower <- exp(pooled_result$ci.lb)
pooled_upper <- exp(pooled_result$ci.ub)

# Add the pooled HR as a summary row in the label matrix
pooled_label <- c("RE Model", "", "", "", "", 
                  paste0(round(pooled_hr, 2), " [", round(pooled_lower, 2), ", ", round(pooled_upper, 2), "]"))

label_matrix <- rbind(
  cbind(
    allD$Author,
    allD$Year,
    allD$Outcome,
    allD$Source,   # Include Data Source
    allD$Metric,   # Include BPV Metric
    paste0(round(allD$HR, 2), " [", round(allD$lower, 2), ", ", round(allD$upper, 2), "]")  # Estimate (95% CI)
  ),
  pooled_label  # Add the pooled result as the last row
)

# Save the forest plot as a high-resolution PNG
png("forest_plot_high_res_DBP5.png", width = 15, height = 15, units = "in", res = 300)

# Adjust your forest plot settings
forestplot(
  labeltext = label_matrix,                          # Add Study, Year, Outcome, Data Source, BPV Metric, and Estimate (95% CI)
  mean = c(allD$HR, pooled_hr),                      # Hazard Ratio (HR) including the pooled HR
  lower = c(allD$lower, pooled_lower),               # Lower Confidence Interval including the pooled lower CI
  upper = c(allD$upper, pooled_upper),               # Upper Confidence Interval including the pooled upper CI
  zero = 1,                                          # Reference line at HR = 1
  xlab = "Hazard Ratio",                             # Label for the x-axis
  title = "Forest Plot of Visit-to-Visit Diastolic BPV",  # Title of the plot
  new_page = TRUE,                                   # Start new page for the plot
  boxsize = c(rep(0.2, nrow(allD)), 0.3),            # Size of the boxes for HR estimates, larger for pooled HR
  lineheight = unit(1.8, "cm"),                      # Increase line height for better spacing
  col = fpColors(box="black", lines="gray"),         # Color for the boxes and lines
  is.summary = c(rep(FALSE, nrow(allD)), TRUE),      # Mark the pooled HR row as a summary
  txt_gp = fpTxtGp(ticks = gpar(cex = 0.7),          # Adjust font size for ticks
                   label = gpar(cex = 0.65),         # Adjust font size for labels
                   xlab  = gpar(cex = 0.65)),        # Adjust font size for x-axis label
  graph.pos = 6,                                     # Shift the graph to the right column
  graphwidth = unit(10, "cm"),                       # Increase graph width if needed for the BPV Metric column
  align = c("l", "l", "l", "l", "l", "l", "c"),      # Align the labels: Study name left, other columns right
  hrzl_lines = gpar(col = "black"),                  # Add horizontal lines for separation
  clip = c(-0.5, 3),                                 # Clip the HR axis between 0.5 and 3 for better visualization
  mar = unit(c(1, 1, 1, 5), "cm")                    # Adjust margins for better spacing
)

# Close the device to save the file
dev.off()


