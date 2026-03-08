png("forest_CVD_fixed.png", width = 9, height = 6, units = "in", res = 600)

forest(resCVD,
       addpred = TRUE,
       showweights = TRUE,
       header = TRUE,
       atransf = exp,
       xlim = c(-5, 2.25),
       at = log(c(0.5, 0.75, 1, 1.25, 2.25)),
       slab = cvd_data$Author_Year,
       ilab = cbind(cvd_data$`Data Source`, cvd_data$`BPV Metric`),
       ilab.xpos = c(-3.3, -2.1),
       cex = 0.58)

text(x = -3.3, y = length(resCVD$yi) + 2, labels = "Data Source", font = 2, cex = 0.6)
text(x = -2.1, y = length(resCVD$yi) + 2, labels = "BPV Metric", font = 2, cex = 0.6)

text(x = -4.8, y = -2.2, 
     labels = paste0("Heterogeneity: I² = ", round(resCVD$I2, 1), "%, ",
                     "τ² = ", signif(resCVD$tau2, 3), ", ",
                     "p = ", format.pval(resCVD$QEp, digits = 2, eps = .001)),
     cex = 0.58, pos = 4)

dev.off()


#####################



png("forest_CVA_fixed.png", width = 9, height = 6, units = "in", res = 600)

forest(resCVA,
       addpred = TRUE,
       showweights = TRUE,
       header = TRUE,
       atransf = exp,
       xlim = c(-5, 2.25),
       at = log(c(0.5, 0.75, 1, 1.25, 2.25)),
       slab = cva_data$Author_Year,
       ilab = cbind(cva_data$`Data Source`, cva_data$`BPV Metric`),
       ilab.xpos = c(-3.3, -2.1),
       cex = 0.58)

text(x = -3.3, y = length(resCVA$yi) + 2, labels = "Data Source", font = 2, cex = 0.6)
text(x = -2.1, y = length(resCVA$yi) + 2, labels = "BPV Metric", font = 2, cex = 0.6)

text(x = -4.8, y = -2.2, 
     labels = paste0("Heterogeneity: I² = ", round(resCVA$I2, 1), "%, ",
                     "τ² = ", signif(resCVA$tau2, 3), ", ",
                     "p = ", format.pval(resCVA$QEp, digits = 2, eps = .001)),
     cex = 0.58, pos = 4)

dev.off()


#######################

# Open a PNG graphics device
png("forest_HF_fixed.png", width = 9, height = 6, units = "in", res = 600)

# Generate the forest plot
forest(resHF,
       addpred = TRUE,
       showweights = TRUE,
       header = TRUE,
       atransf = exp,
       xlim = c(-5, 2.25),
       at = log(c(0.5, 0.75, 1, 1.25, 2.25)),
       slab = hf_data$Author_Year,
       ilab = cbind(hf_data$`Data Source`, hf_data$`BPV Metric`),
       ilab.xpos = c(-3.3, -2.1),
       cex = 0.58)

# Add column labels manually
text(x = -3.3, y = length(resHF$yi) + 2, labels = "Data Source", font = 2, cex = 0.6)
text(x = -2.1, y = length(resHF$yi) + 2, labels = "BPV Metric", font = 2, cex = 0.6)

# Add heterogeneity stats
text(x = -4.8, y = -2, 
     labels = paste0("Heterogeneity: I² = ", round(resHF$I2, 1), "%, ",
                     "τ² = ", signif(resHF$tau2, 3), ", ",
                     "p = ", format.pval(resHF$QEp, digits = 2, eps = .001)),
     cex = 0.58, pos = 4)

dev.off()

##############################


# Open a PNG graphics device
png("forest_AF_fixed.png", width = 9, height = 6, units = "in", res = 600)

forest(resAF,
       addpred = TRUE,
       showweights = TRUE,
       header = TRUE,
       atransf = exp,
       xlim = c(-5, 2.25),
       at = log(c(0.5, 0.75, 1, 1.25, 2.25)),
       slab = af_data$Author_Year,
       ilab = cbind(af_data$`Data Source`, af_data$`BPV Metric`),
       ilab.xpos = c(-3.3, -2.1),
       cex = 0.58)

text(x = -3.3, y = length(resAF$yi) + 2, labels = "Data Source", font = 2, cex = 0.6)
text(x = -2.1, y = length(resAF$yi) + 2, labels = "BPV Metric", font = 2, cex = 0.6)

text(x = -4.8, y = -1.5, 
     labels = paste0("Heterogeneity: I² = ", round(resAF$I2, 1), "%, ",
                     "τ² = ", signif(resAF$tau2, 3), ", ",
                     "p = ", format.pval(resAF$QEp, digits = 2, eps = .001)),
     cex = 0.58, pos = 4)

dev.off()


###############################

# Open a PNG graphics device
png("forest_CVM_fixed.png", width = 9, height = 6, units = "in", res = 600)

forest(resCVM,
       addpred = TRUE,
       showweights = TRUE,
       header = TRUE,
       atransf = exp,
       xlim = c(-5, 2.25),
       at = log(c(0.5, 0.75, 1, 1.25, 2.25)),
       slab = cvm_data$Author_Year,
       ilab = cbind(cvm_data$`Data Source`, cvm_data$`BPV Metric`),
       ilab.xpos = c(-3.3, -2.1),
       cex = 0.58)

text(x = -3.3, y = length(resCVM$yi) + 2, labels = "Data Source", font = 2, cex = 0.6)
text(x = -2.1, y = length(resCVM$yi) + 2, labels = "BPV Metric", font = 2, cex = 0.6)

text(x = -4.8, y = -1.5, 
     labels = paste0("Heterogeneity: I² = ", round(resCVM$I2, 1), "%, ",
                     "τ² = ", signif(resCVM$tau2, 3), ", ",
                     "p = ", format.pval(resCVM$QEp, digits = 2, eps = .001)),
     cex = 0.58, pos = 4)

dev.off()

###########################

# Open a PNG graphics device
png("forest_MI_fixed.png", width = 9, height = 6, units = "in", res = 600)

forest(resMI,
       addpred = TRUE,
       showweights = TRUE,
       header = TRUE,
       atransf = exp,
       xlim = c(-5, 2.25),
       at = log(c(0.5, 0.75, 1, 1.25, 2.25)),
       slab = mi_data$Author_Year,
       ilab = cbind(mi_data$`Data Source`, mi_data$`BPV Metric`),
       ilab.xpos = c(-3.3, -2.1),
       cex = 0.58)

text(x = -3.3, y = length(resMI$yi) + 2, labels = "Data Source", font = 2, cex = 0.6)
text(x = -2.1, y = length(resMI$yi) + 2, labels = "BPV Metric", font = 2, cex = 0.6)

text(x = -4.8, y = -2, 
     labels = paste0("Heterogeneity: I² = ", round(resMI$I2, 1), "%, ",
                     "τ² = ", signif(resMI$tau2, 3), ", ",
                     "p = ", format.pval(resMI$QEp, digits = 2, eps = .001)),
     cex = 0.58, pos = 4)

dev.off()


################## CVD_DBP ##########################

cvd_d <- read_excel("C:/Users/Administrator/OneDrive - UNSW/4. Uni of Brawijaya ACS HF/3.Tika PhD(June 2022-Nov2025)/4. Study 1- Systematic Review/Rcode/MA dataset for R/Outcome/CVD_DBP.xlsx")

#Analyse on CVD outcome only on DBP

resCVD_D <- rma(yi=logHR, sei=selogHR, data = cvd_d, method = "REML")
resCVD_D
#predicted pooled risk ratio and corresponding CI/PI
predict(resCVD_D, transf=exp, digits=2)

# Assuming resCVD is your rma model and cvd_data contains relevant columns
# Optional: create a combined 'Author, Year' label
cvd_d$Author_Year <- paste(cvd_d$Author, cvd_d$Year, sep = ", ")



########################

# Open a PNG graphics device
png("forest_CVD_D.png", width = 8, height = 6, units = "in", res = 600)

# Generate the forest plot
forest(resCVD_D,
       addpred = TRUE,
       showweights = TRUE,
       header = TRUE,
       atransf = exp,
       xlim = c(-4.5, 2),  # shifted right
       at = log(c(0.5, 0.75, 1, 1.25, 2.25)),
       slab = cvd_d$Author_Year,
       ilab = cbind(cvd_d$Source, cvd_d$Metric),
       ilab.xpos = c(-3.5, -1),  # shifted both label columns right
       cex = 0.6)

# Add column headers (also shifted right)
text(x = -3.5, y = length(resCVD_D$yi) + 2, labels = "Data Source", font = 2, cex = 0.7)
text(x = -1, y = length(resCVD_D$yi) + 2, labels = "BPV Metric", font = 2, cex = 0.7)

# Add heterogeneity stats (aligned better with left margin)
text(x = -3.8, y = -1.5, 
     labels = paste0("Heterogeneity: I² = ", round(resCVD_D$I2, 1), "%, ",
                     "τ² = ", signif(resCVD_D$tau2, 3), ", ",
                     "p = ", format.pval(resCVD_D$QEp, digits = 2, eps = .001)),
     cex = 0.6, pos = 4)

# Close graphics device
dev.off()


################## CVA_DBP ##########################

cva_d <- read_excel("C:/Users/Administrator/OneDrive - UNSW/4. Uni of Brawijaya ACS HF/3.Tika PhD(June 2022-Nov2025)/4. Study 1- Systematic Review/Rcode/MA dataset for R/Outcome/Stroke_DBP.xlsx")

#Analyse on CVA only on DBP

resCVA_D <- rma(yi=logHR, sei=selogHR, data = cva_d, method = "REML")
resCVA_D
#predicted pooled risk ratio and corresponding CI/PI
predict(resCVA_D, transf=exp, digits=2)

# Assuming resCVD is your rma model and cvd_data contains relevant columns
# Optional: create a combined 'Author, Year' label
cva_d$Author_Year <- paste(cva_d$Author, cva_d$Year, sep = ", ")



########################

# Open a PNG graphics device
png("forest_CVA_D.png", width = 8, height = 6, units = "in", res = 600)

# Generate the forest plot
forest(resCVA_D,
       addpred = TRUE,
       showweights = TRUE,
       header = TRUE,
       atransf = exp,
       xlim = c(-4.5, 2),  # shifted right
       at = log(c(0.5, 0.75, 1, 1.25, 2.25)),
       slab = cva_d$Author_Year,
       ilab = cbind(cva_d$Source, cva_d$Metric),
       ilab.xpos = c(-3.5, -2),  # shifted both label columns right
       cex = 0.55)

# Add column headers (also shifted right)
text(x = -3.5, y = length(resCVA_D$yi) + 2, labels = "Data Source", font = 2, cex = 0.55)
text(x = -2, y = length(resCVA_D$yi) + 2, labels = "BPV Metric", font = 2, cex = 0.55)

# Add heterogeneity stats (aligned better with left margin)
text(x = -3.8, y = -1.5, 
     labels = paste0("Heterogeneity: I² = ", round(resCVA_D$I2, 1), "%, ",
                     "τ² = ", signif(resCVA_D$tau2, 3), ", ",
                     "p = ", format.pval(resCVA_D$QEp, digits = 2, eps = .001)),
     cex = 0.6, pos = 4)

# Close graphics device
dev.off()


################## HF_DBP ##########################

hf_d <- read_excel("C:/Users/Administrator/OneDrive - UNSW/4. Uni of Brawijaya ACS HF/3.Tika PhD(June 2022-Nov2025)/4. Study 1- Systematic Review/Rcode/MA dataset for R/Outcome/HF_DBP.xlsx")

#Analyse on HF only on DBP

resHF_D <- rma(yi=logHR, sei=selogHR, data = hf_d, method = "REML")
resHF_D
#predicted pooled risk ratio and corresponding CI/PI
predict(resHF_D, transf=exp, digits=2)

# Assuming resCVD is your rma model and cvd_data contains relevant columns
# Optional: create a combined 'Author, Year' label
hf_d$Author_Year <- paste(hf_d$Author, hf_d$Year, sep = ", ")



########################

# Open a PNG graphics device
png("forest_HF_D.png", width = 8, height = 6, units = "in", res = 600)

# Generate the forest plot
forest(resHF_D,
       addpred = TRUE,
       showweights = TRUE,
       header = TRUE,
       atransf = exp,
       xlim = c(-4.5, 2),  # shifted right
       at = log(c(0.5, 0.75, 1, 1.25, 2.25)),
       slab = hf_d$Author_Year,
       ilab = cbind(hf_d$Source, hf_d$Metric),
       ilab.xpos = c(-3.5, -2),  # shifted both label columns right
       cex = 0.6)

# Add column headers (also shifted right)
text(x = -3.5, y = length(resHF_D$yi) + 2, labels = "Data Source", font = 2, cex = 0.6)
text(x = -2, y = length(resHF_D$yi) + 2, labels = "BPV Metric", font = 2, cex = 0.6)

# Add heterogeneity stats (aligned better with left margin)
text(x = -3.8, y = -1.5, 
     labels = paste0("Heterogeneity: I² = ", round(resHF_D$I2, 1), "%, ",
                     "τ² = ", signif(resHF_D$tau2, 3), ", ",
                     "p = ", format.pval(resHF_D$QEp, digits = 2, eps = .001)),
     cex = 0.6, pos = 4)

# Close graphics device
dev.off()



################## AF_DBP ##########################

af_d <- read_excel("C:/Users/Administrator/OneDrive - UNSW/4. Uni of Brawijaya ACS HF/3.Tika PhD(June 2022-Nov2025)/4. Study 1- Systematic Review/Rcode/MA dataset for R/Outcome/AF_DBP.xlsx")

#Analyse on AF only on DBP

resAF_D <- rma(yi=logHR, sei=selogHR, data = af_d, method = "REML")
resAF_D
#predicted pooled risk ratio and corresponding CI/PI
predict(resAF_D, transf=exp, digits=2)

# Assuming resCVD is your rma model and cvd_data contains relevant columns
# Optional: create a combined 'Author, Year' label
af_d$Author_Year <- paste(af_d$Author, af_d$Year, sep = ", ")



########################

# Open a PNG graphics device
png("forest_AF_D.png", width = 8, height = 6, units = "in", res = 600)

# Generate the forest plot
forest(resAF_D,
       addpred = TRUE,
       showweights = TRUE,
       header = TRUE,
       atransf = exp,
       xlim = c(-4.5, 2),  # shifted right
       at = log(c(0.5, 0.75, 1, 1.25, 2.25)),
       slab = af_d$Author_Year,
       ilab = cbind(af_d$Source, af_d$Metric),
       ilab.xpos = c(-3.5, -2),  # shifted both label columns right
       cex = 0.6)

# Add column headers (also shifted right)
text(x = -3.5, y = length(resAF_D$yi) + 2, labels = "Data Source", font = 2, cex = 0.6)
text(x = -2, y = length(resAF_D$yi) + 2, labels = "BPV Metric", font = 2, cex = 0.6)

# Add heterogeneity stats (aligned better with left margin)
text(x = -3.8, y = -1.5, 
     labels = paste0("Heterogeneity: I² = ", round(resAF_D$I2, 1), "%, ",
                     "τ² = ", signif(resAF_D$tau2, 3), ", ",
                     "p = ", format.pval(resAF_D$QEp, digits = 2, eps = .001)),
     cex = 0.6, pos = 4)

# Close graphics device
dev.off()


