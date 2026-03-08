#install.packages("metabias")
library("metabias")
library("meta")
library("metafor")
library ("dmetar")

all <- all_SBP_V2

resall <- rma(yi=logHR, sei=selogHR, data = all)
resall
predict(resall, transf=exp, digits=2)


SDonly <- subset (all, Metric == "SD")
CVonly <- subset (all, Metric == "CV")
ARVonly <- subset (all, Metric == "ARV")
VIMonly <- subset (all, Metric == "VIM")

funnel(resall)

pb_sd <- rma(yi=logHR, sei=selogHR, data = SDonly)
pb_cv <- rma(yi=logHR, sei=selogHR, data = CVonly)
pb_arv <- rma(yi=logHR, sei=selogHR, data = ARVonly)
pb_vim <- rma(yi=logHR, sei=selogHR, data = VIMonly)

pb_sd
predict(pb_sd, transf=exp, digits=2)
#inf <- metagen (SDonly$logHR, SDonly$selogHR, studlab = paste(SDonly$Author), sm = "HR")
#inf

inf <- metagen (SDonly$logHR, SDonly$selogHR, studlab = paste(SDonly$Author), sm = "HR")
inf




leave1out <- metainf(inf_all)
forest(leave1out)
cum_meta <- metacum(inf_all)
forest(cum_meta)
metabias(inf_all, method.bias = "Begg")
ranktest(resall)


# Egger's test using metafor
regtest(resall, model = "lm")
regtest(pb_sd, model = "lm")
regtest(pb_cv, model = "lm")
regtest(pb_arv, model = "lm")
regtest(pb_vim, model = "lm")




metabias(inf, method = "linreg")


res.et <- regtest(pb, model = "lm")
res.et
plot(res.et, bg = "lightblue")

?metafor::regtest

tf1 <- trimfill(inf)
tf1

tf_all <- trimfill(inf_all)
tf_all

funnel(tf1, backtransf = FALSE)
funnel(tf1, pch = ifelse(tf1$trimfill, 1, 16), level = 0.9, random = TRUE,
       backtransf = FALSE)



# Define fill colors for contour
col.contour = c("gray50", "gray75", "gray95")

# Generate funnel plot (we do not include study labels here)
meta::funnel(pb, xlim = c(-0.5, 2),
             contour = c(0.9, 0.95, 0.99),
             col.contour = col.contour)

meta::funnel(inf_all,
             xlim = c(0.5, 2),
             studlab = TRUE)


# Add a legend
legend(x = 1.6, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)


find.outliers(resall)
find.outliers(pb_sd)
find.outliers(pb_cv)
find.outliers(pb_arv)
find.outliers(pb_vim)
predict(resall, transf=exp, digits=2)


rows_to_remove <- c(7, 8, 11, 12, 17, 19, 20, 21, 22, 24, 29, 35, 42, 46, 47, 53, 57, 60, 61, 64, 67, 70, 71, 72, 73, 75, 79, 80)
all.no.out <- all[-rows_to_remove, ]
resall.no.out <- rma(yi=logHR, sei=selogHR, data = all.no.out)
resall.no.out
predict(resall.no.out, transf=exp, digits=2)

rows_to_remove_SD <- c(7, 8, 11, 12, 17, 19, 20, 21, 22, 24, 29, 35)
SD.no.out <- SDonly[-rows_to_remove_SD, ]
resall.no.out_SD <- rma(yi=logHR, sei=selogHR, data = SD.no.out)
resall.no.out_SD
predict(resall.no.out_SD, transf=exp, digits=2)


# Initial meta-analysis
resall <- rma(yi=logHR, sei=selogHR, data = all)

# Identify and remove outliers
outliers <- c(7, 8, 11, 12, 17, 19, 20, 21, 22, 24, 29, 35, 42, 46, 47, 53, 57, 60, 61, 64, 67, 70, 71, 72, 73, 75, 79, 80)
cleaned_data <- all[-outliers, ]

# Recalculate meta-analysis without outliers
res_cleaned <- rma(yi=logHR, sei=selogHR, data = cleaned_data)
res_cleaned

# Predict new estimates
pred_cleaned <- predict(res_cleaned, transf = exp, digits = 2)

# Display results
print(pred_cleaned)

# Using all studies

tf_all <- trimfill(resall)
tf_all

tf.no.out_all <- trimfill(update( resall, 
                             subset = -c(1, 5, 9, 14, 19, 21, 23, 24, 25, 27, 39, 41, 44, 46, 53, 56, 57, 60, 65, 67, 68, 70)))
tf.no.out_all

predict(tf.no.out_all, transf=exp, digits=2)
funnel(tf.no.out_all)



regtest(tf.no.out_all, model = "lm")

# Using all studies with sd
tf <- trimfill(pb_sd)
tf

# Analyze with outliers removed
tf.no.out <- trimfill(update(pb_sd, 
                             subset = -c(1, 5, 9, 14, 19, 21, 23, 25, 27)))
tf.no.out

predict(tf.no.out, transf=exp, digits=2)

# Using all studies
tf <- trimfill(pb_sd)
tf

# Analyze with outliers removed
tf.no.out <- trimfill(update(pb_sd, 
                             subset = -c(1, 5, 9, 14, 19, 21, 23, 25, 27)))
tf.no.out

predict(tf.no.out, transf=exp, digits=2)

### Influential Analysis #####

inf_all <- metagen (all$logHR, all$selogHR, studlab = all$Author, sm = "HR")
inf_all

infall <- InfluenceAnalysis(inf_all, random = TRUE)
infall
plot(infall, "influence")


plot(infall, "baujat")
