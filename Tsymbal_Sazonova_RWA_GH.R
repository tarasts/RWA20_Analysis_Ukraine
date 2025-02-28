library(tidyverse)
library(foreign)
library(lavaan)
library(sem)
library(semPlot)
library(semTools)
library(psych)
library(mgcv)
library(ggplot2)
library(corpcor)
library(irr)


#read in dataset from a csv filee
auth_data <- read.csv("2023-2024-student_retest_RWA_Oes_merged.csv", sep = ";")




#create a vector with codes of all RWA items
all_items_rwa20 <- c("rwa03", "rwa04", "rwa05", "rwa06", "rwa07", "rwa08", "rwa09", "rwa10", "rwa11", "rwa12", "rwa13",
                     "rwa14", "rwa15", "rwa16", "rwa17", "rwa18", "rwa19", "rwa20", "rwa21", "rwa22")


# Define the pro-trait and con-trait items for the 1st wave
pro_trait_items_rwa_w1 <- c("rwa03_w1", "rwa05_w1", "rwa07_w1", "rwa10_w1", "rwa12_w1", "rwa14_w1", "rwa16_w1", "rwa17_w1", "rwa19_w1", "rwa22_w1")
con_trait_items_rwa_w1 <- c("rwa04_w1", "rwa06_w1", "rwa08_w1", "rwa09_w1", "rwa11_w1", "rwa13_w1", "rwa15_w1", "rwa18_w1", "rwa20_w1", "rwa21_w1")
all_items_rwa_w1 <- c("rwa03_w1", "rwa04_w1", "rwa05_w1", "rwa06_w1", "rwa07_w1", "rwa08_w1", "rwa09_w1", "rwa10_w1", "rwa11_w1", "rwa12_w1", "rwa13_w1",
                      "rwa14_w1", "rwa15_w1", "rwa16_w1", "rwa17_w1", "rwa18_w1", "rwa19_w1", "rwa20_w1", "rwa21_w1", "rwa22_w1")


#calculate items scores corrected for Acquiescent Response Style (ARS)
# Step 1: Reverse con-trait items back to their unrecoded form (before recoding)
auth_data[con_trait_items_rwa_w1] <- 10 - auth_data[con_trait_items_rwa_w1]

# Step 2: Calculate the acquiescence score as the mean of all items (pro-trait and unrecoded con-trait)
auth_data_z <- scale(auth_data[, c(all_items_rwa_w1)])                            # Standardize the items to z-scores
auth_data$rwa_acquiescence_score_w1 <- rowMeans(auth_data_z, na.rm = TRUE)        # Acquiescence score

# Step 3: Re-reverse the con-trait items to their recoded form (aligned with pro-trait items)
auth_data[con_trait_items_rwa_w1] <- 10 - auth_data[con_trait_items_rwa_w1]       # Recoding back to align with pro-trait items

# Step 4: Regress each item on the acquiescence score to obtain residuals (ARS-corrected scores)
for (item in c(all_items_rwa_w1)) {
  auth_data[[paste0(item, "_residual")]] <- residuals(lm(auth_data[[item]] ~ auth_data$rwa_acquiescence_score_w1))
}

#calculate RWA raw score and ARS-corrected score for the 1st wave
auth_data$rwa_score_w1 <- rowMeans(auth_data[c(all_items_rwa_w1)])
auth_data$rwa_residual_score_w1 <- auth_data %>% 
  dplyr::select(dplyr::contains("rwa") & dplyr::contains("residual") & dplyr::contains("w1")) %>% rowMeans()



###repeating the same operations for wave 2
# Define the pro-trait and con-trait items for the 2nd wave
pro_trait_items_rwa_w2 <- c("rwa03_w2", "rwa05_w2", "rwa07_w2", "rwa10_w2", "rwa12_w2", "rwa14_w2", "rwa16_w2", "rwa17_w2", "rwa19_w2", "rwa22_w2")
con_trait_items_rwa_w2 <- c("rwa04_w2", "rwa06_w2", "rwa08_w2", "rwa09_w2", "rwa11_w2", "rwa13_w2", "rwa15_w2", "rwa18_w2", "rwa20_w2", "rwa21_w2")
all_items_rwa_w2 <- c("rwa03_w2", "rwa04_w2", "rwa05_w2", "rwa06_w2", "rwa07_w2", "rwa08_w2", "rwa09_w2", "rwa10_w2", "rwa11_w2", "rwa12_w2", "rwa13_w2",
                      "rwa14_w2", "rwa15_w2", "rwa16_w2", "rwa17_w2", "rwa18_w2", "rwa19_w2", "rwa20_w2", "rwa21_w2", "rwa22_w2")

# Step 1: Reverse con-trait items back to their unrecoded form (before recoding)
auth_data[con_trait_items_rwa_w2] <- 10 - auth_data[con_trait_items_rwa_w2]

# Step 2: Calculate the acquiescence score as the mean of all items (pro-trait and unrecoded con-trait)
auth_data_z <- scale(auth_data[, c(all_items_rwa_w2)])                          # Standardize the items to z-scores
auth_data$rwa_acquiescence_score_w2 <- rowMeans(auth_data_z, na.rm = TRUE)      # Acquiescence score

# Step 3: Re-reverse the con-trait items to their recoded form (aligned with pro-trait items)
auth_data[con_trait_items_rwa_w2] <- 10 - auth_data[con_trait_items_rwa_w2]     # Recoding back to align with pro-trait items

# Step 4: Regress each item on the acquiescence score to obtain residuals (ARS-corrected scores)
for (item in c(all_items_rwa_w2)) {
  auth_data[[paste0(item, "_residual")]] <- residuals(lm(auth_data[[item]] ~ auth_data$rwa_acquiescence_score_w2))
}

#calculate RWA raw score and ARS-corrected score for the 2nd wave
auth_data$rwa_score_w2 <- rowMeans(auth_data[c(all_items_rwa_w2)])
auth_data$rwa_residual_score_w2 <- auth_data %>% 
  dplyr::select(dplyr::contains("rwa") & dplyr::contains("residual") & dplyr::contains("w2")) %>% rowMeans()


#ARS-corrected data  
#define pro-trait, con-trait and all items of the ARS-corrected 1st wave dataset
pro_trait_items_rwa_w1_residual <- c("rwa03_w1_residual", "rwa05_w1_residual", "rwa07_w1_residual", "rwa10_w1_residual", "rwa12_w1_residual", "rwa14_w1_residual", "rwa16_w1_residual", "rwa17_w1_residual", "rwa19_w1_residual", "rwa22_w1_residual")
con_trait_items_rwa_w1_residual <- c("rwa04_w1_residual", "rwa06_w1_residual", "rwa08_w1_residual", "rwa09_w1_residual", "rwa11_w1_residual", "rwa13_w1_residual", "rwa15_w1_residual", "rwa18_w1_residual", "rwa20_w1_residual", "rwa21_w1_residual")
all_items_rwa_w1_residual <- c("rwa03_w1_residual", "rwa04_w1_residual", "rwa05_w1_residual", "rwa06_w1_residual", "rwa07_w1_residual", 
                               "rwa08_w1_residual", "rwa09_w1_residual", "rwa10_w1_residual", "rwa11_w1_residual", "rwa12_w1_residual", 
                               "rwa13_w1_residual", "rwa14_w1_residual", "rwa15_w1_residual", "rwa16_w1_residual", "rwa17_w1_residual", 
                               "rwa18_w1_residual", "rwa19_w1_residual", "rwa20_w1_residual", "rwa21_w1_residual", "rwa22_w1_residual")

#define pro-trait, con-trait and all items of the ARS-corrected 2nd wave  dataset
pro_trait_items_rwa_w2_residual <- c("rwa03_w2_residual", "rwa05_w2_residual", "rwa07_w2_residual", "rwa10_w2_residual", "rwa12_w2_residual", "rwa14_w2_residual", "rwa16_w2_residual", "rwa17_w2_residual", "rwa19_w2_residual", "rwa22_w2_residual")
con_trait_items_rwa_w2_residual <- c("rwa04_w2_residual", "rwa06_w2_residual", "rwa08_w2_residual", "rwa09_w2_residual", "rwa11_w2_residual", "rwa13_w2_residual", "rwa15_w2_residual", "rwa18_w2_residual", "rwa20_w2_residual", "rwa21_w2_residual")
all_items_rwa_w2_residual <- c("rwa03_w2_residual", "rwa04_w2_residual", "rwa05_w2_residual", "rwa06_w2_residual", "rwa07_w2_residual", 
                               "rwa08_w2_residual", "rwa09_w2_residual", "rwa10_w2_residual", "rwa11_w2_residual", "rwa12_w2_residual", 
                               "rwa13_w2_residual", "rwa14_w2_residual", "rwa15_w2_residual", "rwa16_w2_residual", "rwa17_w2_residual", 
                               "rwa18_w2_residual", "rwa19_w2_residual", "rwa20_w2_residual", "rwa21_w2_residual", "rwa22_w2_residual")





#cleaning the dataset
##wave 1: inspecting RWA score and ARS score
hist(auth_data$rwa_score_w1, main = "Histogram of raw RWA Scores, wave 1", xlab = "Scores")
hist(auth_data$rwa_acquiescence_score_w1, main = "Histogram of Acquiescense Scores, wave 1", xlab = "Scores")

##wave 2: inspecting RWA score and ARS score
hist(auth_data$rwa_score_w2, main = "Histogram of raw RWA Scores, wave 2", xlab = "Scores")
hist(auth_data$rwa_acquiescence_score_w2, main = "Histogram of Acquiescense Scores, wave 2", xlab = "Scores")


#calculation of correlation between respondent's responses in two waves
# Define the columns for the two waves
wave1_columns <- paste0("rwa", sprintf("%02d", 3:22), "_w1")
wave2_columns <- paste0("rwa", sprintf("%02d", 3:22), "_w2")

# Calculate the correlation for each respondent
auth_data$rwa_correlation <- apply(auth_data, 1, function(row) {
  # Extract responses for wave 1 and wave 2
  responses_wave1 <- as.numeric(row[wave1_columns])
  responses_wave2 <- as.numeric(row[wave2_columns])
  
  # Calculate correlation
  cor(responses_wave1, responses_wave2)
})

hist(auth_data$rwa_correlation, main = "RWA interwave correlations", xlab = "Scores")

#calculating discrepancy in scores between two waves
auth_data$rwa_residual_dif_abs <- abs(auth_data$rwa_residual_score_w1 - auth_data$rwa_residual_score_w2)
hist(auth_data$rwa_residual_dif_abs)

#final data screening
sub_auth_data <- auth_data %>%  na.omit() %>% 
  dplyr::filter(rwa_correlation >= quantile(rwa_correlation, 0.01)) %>%
  dplyr::filter(rwa_residual_dif_abs <= quantile(rwa_residual_dif_abs, 0.99))



#final data inspection
#wave 1
hist(sub_auth_data$rwa_score_w1, main = "Histogram of raw RWA Scores, wave 1", xlab = "Scores")
hist(sub_auth_data$rwa_residual_score_w1, main = "Histogram of ARS-corrected RWA Scores, wave 1", xlab = "Scores")
hist(sub_auth_data$rwa_acquiescence_score_w1, main = "Histogram of Acquiescense Scores, wave 1", xlab = "Scores")

#wave2
hist(sub_auth_data$rwa_score_w2, main = "Histogram of raw RWA Scores, wave 2", xlab = "Scores")
hist(sub_auth_data$rwa_residual_score_w2, main = "Histogram of ARS-corrected RWA Scores, wave 2", xlab = "Scores")
hist(sub_auth_data$rwa_acquiescence_score_w2, main = "Histogram of Acquiescense Scores, wave 2", xlab = "Scores")

hist(sub_auth_data$rwa_correlation, main = "RWA interwave correlations", xlab = "Scores")
hist(sub_auth_data$rwa_residual_dif_abs)




#recalculating ARS-corrected item scores and ARS bias for the cleaned dataset
# Step 1: Reverse con-trait items back to their unrecoded form (before recoding)
sub_auth_data[con_trait_items_rwa_w1] <- 10 - sub_auth_data[con_trait_items_rwa_w1]

# Step 2: Calculate the acquiescence score as the mean of all items (pro-trait and unrecoded con-trait)
sub_auth_data_z <- scale(sub_auth_data[, c(all_items_rwa_w1)])                      # Standardize the items to z-scores
sub_auth_data$rwa_acquiescence_score_w1 <- rowMeans(sub_auth_data_z, na.rm = TRUE)  # Acquiescence score

# Step 3: Re-reverse the con-trait items to their recoded form (aligned with pro-trait items)
sub_auth_data[con_trait_items_rwa_w1] <- 10 - sub_auth_data[con_trait_items_rwa_w1]  # Recoding back to align with pro-trait items

# Step 4: Regress each item on the acquiescence score to obtain residuals (ARS-corrected scores)
for (item in c(all_items_rwa_w1)) {
  sub_auth_data[[paste0(item, "_residual")]] <- residuals(lm(sub_auth_data[[item]] ~ sub_auth_data$rwa_acquiescence_score_w1))
}

#calculate RWA raw score and ARS-corrected score for the wave 1
sub_auth_data$rwa_residual_score_w1 <- sub_auth_data %>% 
  dplyr::select(all_of(all_items_rwa_w1_residual)) %>% rowMeans()



###repeating the same operations for wave 2
# Step 1: Reverse con-trait items back to their unrecoded form (before recoding)
sub_auth_data[con_trait_items_rwa_w2] <- 10 - sub_auth_data[con_trait_items_rwa_w2]

# Step 2: Calculate the acquiescence score as the mean of all items (pro-trait and unrecoded con-trait)
sub_auth_data_z <- scale(sub_auth_data[, c(all_items_rwa_w2)])                      # Standardize the items to z-scores
sub_auth_data$rwa_acquiescence_score_w2 <- rowMeans(sub_auth_data_z, na.rm = TRUE)  # Acquiescence score

# Step 3: Re-reverse the con-trait items to their recoded form (aligned with pro-trait items)
sub_auth_data[con_trait_items_rwa_w2] <- 10 - sub_auth_data[con_trait_items_rwa_w2]  # Recoding back to align with pro-trait items

# Step 4: Regress each item on the acquiescence score to obtain residuals (ARS-corrected scores)
for (item in c(all_items_rwa_w2)) {
  sub_auth_data[[paste0(item, "_residual")]] <- residuals(lm(sub_auth_data[[item]] ~ sub_auth_data$rwa_acquiescence_score_w2))
}

#calculate RWA raw score and bias-corrected score for the wave 2
sub_auth_data$rwa_residual_score_w2 <- sub_auth_data %>% 
  dplyr::select(all_of(all_items_rwa_w2_residual)) %>% rowMeans()


#shape raw data for factor analysis
data_w1 <- sub_auth_data %>% dplyr::select(all_of(all_items_rwa_w1)) %>%  
  rename_with(~c(
    "rwa03", "rwa04", "rwa05", "rwa06", "rwa07", "rwa08", "rwa09", "rwa10", "rwa11", 
    "rwa12", "rwa13", "rwa14", "rwa15", "rwa16", "rwa17", "rwa18", "rwa19", "rwa20", "rwa21", "rwa22"
    
  ), .cols = everything())

data_w2 <- sub_auth_data %>% dplyr::select(all_of(all_items_rwa_w2)) %>%  
  rename_with(~c(
    "rwa03", "rwa04", "rwa05", "rwa06", "rwa07", "rwa08", "rwa09", "rwa10", "rwa11", 
    "rwa12", "rwa13", "rwa14", "rwa15", "rwa16", "rwa17", "rwa18", "rwa19", "rwa20", "rwa21", "rwa22"
    
  ), .cols = everything())


#extracting common method factor
#specify bifactor model to isolate CMB
model_rwa_1factor_meth <- "
      AUTH =~ rwa03 + rwa04 + rwa05 + rwa06 + rwa07 + rwa08 + rwa09 + rwa10 + rwa11 + rwa12 + 
              rwa13 + rwa14 + rwa15 + rwa16 + rwa17 + rwa18 + rwa19 + rwa20 + rwa21 + rwa22
 
      CMB =~ rwa03 + rwa04 + rwa05 + rwa06 + rwa07 + rwa08 + rwa09 + rwa10 + rwa11 + rwa12 + 
              rwa13 + rwa14 + rwa15 + rwa16 + rwa17 + rwa18 + rwa19 + rwa20 + rwa21 + rwa22

      AUTH ~~ 0*CMB
      AUTH ~~ 1*AUTH
      "

#run model fit for both waves
cfa_output_w1 <- lavaan::cfa(model_rwa_1factor_meth, data = data_w1)
cfa_output_w2 <- lavaan::cfa(model_rwa_1factor_meth, data = data_w2)
fitmeasures(cfa_output_w1, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "ecvi", "aic", "bic", "bic2", "gfi"))
fitmeasures(cfa_output_w2, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "ecvi", "aic", "bic", "bic2", "gfi"))
#summary(cfa_output_w1, standardized = TRUE, fit.measures = TRUE)
#summary(cfa_output_w2, standardized = TRUE, fit.measures = TRUE)
semPaths(cfa_output_w1, what = "paths", whatLabels = "stand")
semPaths(cfa_output_w2, what = "paths", whatLabels = "stand")

#save factor scores to dataframes
prediction_w1 <- lavPredict(cfa_output_w1) %>% as.data.frame()
prediction_w2 <- lavPredict(cfa_output_w2) %>% as.data.frame()

#save CMB scores to vectors
meth_w1 <- prediction_w1$CMB
meth_w2 <- prediction_w2$CMB

#save CMB scores to the main dataset
sub_auth_data$rwa_CMB_score_w1 <- prediction_w1$CMB
sub_auth_data$rwa_CMB_score_w2 <- prediction_w2$CMB

#save content factor scores to vectors
auth_w1 <- prediction_w1$AUTH
auth_w2 <- prediction_w2$AUTH

#inspect correlations
cor.test(meth_w1, sub_auth_data$rwa_acquiescence_score_w1)
cor.test(meth_w2, sub_auth_data$rwa_acquiescence_score_w2)
cor.test(meth_w1, meth_w2)
cor.test(sub_auth_data$rwa_acquiescence_score_w1, sub_auth_data$rwa_acquiescence_score_w2)

#calculate items scores corrected for CMB
demeth_w1 <- apply(sub_auth_data[all_items_rwa_w1], 2, function(var) residuals(lm(var ~ sub_auth_data$rwa_CMB_score_w1))) %>% as.data.frame()
demeth_w2 <- apply(sub_auth_data[all_items_rwa_w2], 2, function(var) residuals(lm(var ~ sub_auth_data$rwa_CMB_score_w2))) %>% as.data.frame()

#define sets of CMB-corrected items
##1st wave
all_items_rwa_w1_decmb <- all_items_rwa_w1_residual %>% gsub("_residual", "_decmb", .)
pro_trait_items_rwa_w1_decmb <- pro_trait_items_rwa_w1_residual %>% gsub("_residual", "_decmb", .)
con_trait_items_rwa_w1_decmb <- con_trait_items_rwa_w1_residual %>% gsub("_residual", "_decmb", .)

#2nd wave
all_items_rwa_w2_decmb <- all_items_rwa_w2_residual %>% gsub("_residual", "_decmb", .)
pro_trait_items_rwa_w2_decmb <- pro_trait_items_rwa_w2_residual %>% gsub("_residual", "_decmb", .)
con_trait_items_rwa_w2_decmb <- con_trait_items_rwa_w2_residual %>% gsub("_residual", "_decmb", .)

#rename columns of CMB-corrected items for differentiation
colnames(demeth_w1) <- all_items_rwa_w1_decmb
colnames(demeth_w2) <- all_items_rwa_w2_decmb

#add CMB-corrected datasets to the main dataframe
sub_auth_data <- cbind(sub_auth_data, demeth_w1, demeth_w2)

#calculating RWA score for CMB-corrected data
#wave 1
sub_auth_data$rwa_decmb_score_w1 <- sub_auth_data %>% 
  dplyr::select(all_of(all_items_rwa_w1_decmb)) %>% rowMeans()
#wave 2
sub_auth_data$rwa_decmb_score_w2 <- sub_auth_data %>% 
  dplyr::select(all_of(all_items_rwa_w2_decmb)) %>% rowMeans()



#calculate sum of agreements score (see Billiet 2000) to validate bias scores

# Re-reverse the con-trait items back to their original scale
sub_auth_data[con_trait_items_rwa_w1] <- 10 - sub_auth_data[con_trait_items_rwa_w1]
sub_auth_data[con_trait_items_rwa_w2] <- 10 - sub_auth_data[con_trait_items_rwa_w2]

# Calculate the sum of agreements (responses > 5) across all items
sum_of_agreements_w1 <- rowSums(
  sub_auth_data[all_items_rwa_w1] > 5,
  na.rm = TRUE                                                                   # Exclude missing values from the calculation
)

sum_of_agreements_w2 <- rowSums(
  sub_auth_data[all_items_rwa_w2] > 5,
  na.rm = TRUE                                                                   # Exclude missing values from the calculation
)


# revert con-trait items back to their prior state
sub_auth_data[con_trait_items_rwa_w1] <- 10 - sub_auth_data[con_trait_items_rwa_w1]
sub_auth_data[con_trait_items_rwa_w2] <- 10 - sub_auth_data[con_trait_items_rwa_w2]

# Add the sum_of_agreements variable to the dataframe
sub_auth_data$sum_of_agreements_w1 <- sum_of_agreements_w1
sub_auth_data$sum_of_agreements_w2 <- sum_of_agreements_w2

#validate bias scores (ARS and CMB)
#ARS score should strongly correlate with sum of agreements variable
cor.test(sub_auth_data$rwa_acquiescence_score_w1, sub_auth_data$sum_of_agreements_w1)
cor.test(sub_auth_data$rwa_acquiescence_score_w2, sub_auth_data$sum_of_agreements_w2)

#CMB score should strongly correlate with sum of agreements variable
cor.test(sub_auth_data$rwa_CMB_score_w1, sub_auth_data$sum_of_agreements_w1)
cor.test(sub_auth_data$rwa_CMB_score_w2, sub_auth_data$sum_of_agreements_w2)





#calculating Intraclass Correlation Coefficient (test-retest reliability)
# Initialize a dataframe to store ICC values
retest_reliability <- matrix(ncol = 4, nrow = 20)  %>% as.data.frame()
colnames(retest_reliability) <- c("item", "raw", "de_acquiescence", "de_cmb")

# Loop through each item and calculate ICC for test-retest reliability
i <- 1
for (item in all_items_rwa_w1) {
  retest_reliability$item[i] <- item
  
  # Get corresponding wave 2 item name
  item_w2 <- gsub("w1", "w2", item)
  
  # Calculate ICC for raw items
  rwa_raw <- sub_auth_data[, c(item, item_w2)]
  retest_reliability$raw[i] <- icc(rwa_raw, model = "twoway", type = "agreement", unit = "single")$value
  
  # Calculate ICC for ARS-corrected items
  item_residual_w1 <- paste0(item, "_residual")
  item_residual_w2 <- gsub("w1", "w2", item_residual_w1)
  rwa_de_acquiescence <- sub_auth_data[, c(item_residual_w1, item_residual_w2)]
  retest_reliability$de_acquiescence[i] <- icc(rwa_de_acquiescence, model = "twoway", type = "agreement", unit = "single")$value
  
  # Calculate ICC for CMB-corrected items
  item_decmb_w1 <- paste0(item, "_decmb")
  item_decmb_w2 <- gsub("w1", "w2", item_decmb_w1)
  rwa_de_cmb <- sub_auth_data[, c(item_decmb_w1, item_decmb_w2)]
  retest_reliability$de_cmb[i] <- icc(rwa_de_cmb, model = "twoway", type = "agreement", unit = "single")$value
  
  i <- i + 1
}

# Clean item names for readability
retest_reliability$item <- gsub("_w1", "", retest_reliability$item)

# Round values to 2 decimal places
retest_reliability[,2:4] <- round(retest_reliability[,2:4], digits = 2)

# View final ICC-based retest reliability table
retest_reliability

#write.table(retest_reliability, "RWA20_retest_reliability_ICC.csv", row.names = FALSE, sep=";")


#calculate aggregate ICCs
#raw data scores for the two waves
scale_scores <- sub_auth_data[, c("rwa_score_w1", "rwa_score_w2")]
icc_scale <- icc(scale_scores, model = "twoway", type = "agreement", unit = "single")
print(icc_scale)

# ARS-corrected scale scores for the two waves
scale_scores <- sub_auth_data[, c("rwa_residual_score_w1", "rwa_residual_score_w2")]
icc_scale <- icc(scale_scores, model = "twoway", type = "agreement", unit = "single")
print(icc_scale)

# CMB-corrected scale scores for the two waves
scale_scores <- sub_auth_data[, c("rwa_decmb_score_w1", "rwa_decmb_score_w2")]
icc_scale <- icc(scale_scores, model = "twoway", type = "agreement", unit = "single")
print(icc_scale)





#alpha-reliability for raw data
#wave 1
psych::alpha(sub_auth_data[all_items_rwa_w1])
#wave 2
psych::alpha(sub_auth_data[all_items_rwa_w2])

#alpha-reliability for ARS-corrected data
#wave 1
psych::alpha(sub_auth_data[all_items_rwa_w1_residual])
#wave 2
psych::alpha(sub_auth_data[all_items_rwa_w2_residual])

#alpha-reliability for CMB-corrected data
#wave 1
psych::alpha(sub_auth_data[all_items_rwa_w1_decmb])
#wave 2
psych::alpha(sub_auth_data[all_items_rwa_w2_decmb])


#McDonald's omega for raw data
psych::omega(sub_auth_data[all_items_rwa_w1], nfactors = 1)
psych::omega(sub_auth_data[all_items_rwa_w2], nfactors = 1)

#McDonald's omega for ARS-corrected data
psych::omega(sub_auth_data[all_items_rwa_w1_residual], nfactors = 1)
psych::omega(sub_auth_data[all_items_rwa_w2_residual], nfactors = 1)

#McDonald's omega for CMB-corrected data
psych::omega(sub_auth_data[all_items_rwa_w1_decmb], nfactors = 1)
psych::omega(sub_auth_data[all_items_rwa_w2_decmb], nfactors = 1)






#detection of method effect
#1st wave, raw data

#create a dataframe holding all 184756 possible unique combinations of 10 elements from the sequence of 20 numbers
all_combinations <- combn(1:20, 10, simplify = TRUE) %>% t() %>% as.data.frame()

#select a random sample of 10000 combinations
set.seed(1234)
selected_combinations <- all_combinations[sample(nrow(all_combinations), 10000, replace = FALSE), ]

#create a temporary dataframe storing all relevant scale items
component_data <- sub_auth_data[all_items_rwa_w1] %>%
  dplyr::select(order(names(.))) %>%
  dplyr::mutate(across(everything(), as.numeric))

#create an empty vector with 10000 slots to keep correlations between half-scales
correlations <- vector(length = 10000)

#calculate correlations between scores of 10000 selected half-scales and scores of remaining items
for (i in 1:10000) {
  half1 <- unlist(selected_combinations[i, ])
  score_1 <- rowMeans(component_data[, half1])
  score_2 <- rowMeans(component_data[, setdiff(1:20, half1)])
  correlations[i] <- cor(score_1, score_2)
}

#calculate correlation between pro-trait half-scale and con-trait half-scale
pro_con_correlation <- cor(rowMeans(component_data[pro_trait_items_rwa_w1]), 
                           rowMeans(component_data[con_trait_items_rwa_w1]))
#calculate mean correlation
mean_correlation <- mean(correlations)


# Open a PNG graphics device
png("corr_density_w1_raw.png", width = 1200, height = 1100, res = 300)
#set margins
par(mar = c(3, 3, 2, 0.5), mgp = c(3, 0.5, 0))

#density plot (correlations)
density(correlations) %>% plot(main = "", xlab = "", ylab = "", lwd = 2, 
                               xlim = c(min(correlations), max(correlations)+max(correlations)*0.1),
                               ylim = c(0, 12.5))

# Add title and axis labels with adjusted offsets
title(main = "Raw data", line = 0.5, , cex.main = 0.8)  # Adjust title offset
mtext("Correlations", side = 1, line = 1.6)  # Adjust x-axis label offset
mtext("Density", side = 2, line = 1.6)  # Adjust x-axis label offset

#add vertical line for correlation between pro-trait half-scale and con-trait half-scale
abline(v = pro_con_correlation,
       col = 'red', lwd = 2, lty = 'dashed')
#add vertical lines for 3 standard deviations from the mean correlation
abline(v = mean(correlations) - 3*sd(correlations), col = 'blue', lwd = 2, lty = 'dotted')
abline(v = mean(correlations) + 3*sd(correlations), col = 'blue', lwd = 2, lty = 'dotted')
#add vertical line for the mean correlation
abline(v = mean_correlation, col = 'blue', lwd = 2)

#add normal curve
x_vals <- seq(min(correlations), max(correlations), length.out = 100)
y_vals <- dnorm(x_vals, mean = mean(correlations), sd = sd(correlations))
lines(x_vals, y_vals, col = "green", lty='dashed', lwd = 2, add=TRUE)

# Annotate the mean value
text(x = mean_correlation+mean_correlation*0.015, y = 1, 
     labels = paste0("Mean Corr = ", round(mean_correlation, 2)), col = 'blue', srt = 90, adj = 0, cex = 0.6)

# Annotate the pro/con correlation value
text(x = pro_con_correlation+pro_con_correlation*0.02, y = 1, 
     labels = paste0("Pro/Con Corr = ", round(pro_con_correlation, 2)), col = 'red', srt = 90, adj = 0, cex = 0.6)
# Close the graphics device
dev.off()

#calculating skewness and kurtosis of the correlation distribution
semTools::skew(correlations)
semTools::kurtosis(correlations)


#1st wave ARS-corrected data (the same combinations are used)
component_data <- sub_auth_data[all_items_rwa_w1_residual] %>%
  dplyr::select(order(names(.))) %>%
  dplyr::mutate(across(everything(), as.numeric))

#create an empty vector with 10000 slots to keep correlations between half-scales
correlations <- vector(length = 10000)

#calculate correlations between scores of 10000 selected half-scales and scores of remaining items
for (i in 1:10000) {
  half1 <- unlist(selected_combinations[i, ])
  score_1 <- rowMeans(component_data[, half1])
  score_2 <- rowMeans(component_data[, setdiff(1:20, half1)])
  correlations[i] <- cor(score_1, score_2)
}

#calculate correlation between pro-trait half-scale and con-trait half-scale
pro_con_correlation <- cor(rowMeans(component_data[pro_trait_items_rwa_w1_residual]), 
                           rowMeans(component_data[con_trait_items_rwa_w1_residual]))
#calculate mean correlation
mean_correlation <- mean(correlations)

# Open a PNG graphics device
png("corr_density_w1_ARS-corrected.png", width = 1200, height = 1100, res = 300)
#set margins
par(mar = c(3, 3, 2, 0.5), mgp = c(3, 0.5, 0))

#density plot (correlations)
density(correlations) %>% plot(main = "", xlab = "", ylab = "", lwd = 2, xlim = c(min(correlations), 1),
                               ylim = c(0, 12.5))

# Add title and axis labels with adjusted offsets
title(main = "ARS-corrected data", line = 0.5, , cex.main = 0.8)  # Adjust title offset
mtext("Correlations", side = 1, line = 1.6)  # Adjust x-axis label offset
mtext("Density", side = 2, line = 1.6)  # Adjust x-axis label offset

#add vertical line for correlation between pro-trait half-scale and con-trait half-scale
abline(v = pro_con_correlation,
       col = 'red', lwd = 2, lty = 'dashed')
#add vertical lines for 3 standard deviations from the mean correlation
abline(v = mean(correlations) - 3*sd(correlations), col = 'blue', lwd = 2, lty = 'dotted')
abline(v = mean(correlations) + 3*sd(correlations), col = 'blue', lwd = 2, lty = 'dotted')
#add vertical line for the mean correlation
abline(v = mean_correlation, col = 'blue', lwd = 2)

#add normal curve
x_vals <- seq(min(correlations), max(correlations), length.out = 100)
y_vals <- dnorm(x_vals, mean = mean(correlations), sd = sd(correlations))
lines(x_vals, y_vals, col = "green", lty='dashed', lwd = 2, add=TRUE)

# Annotate the mean value
text(x = mean_correlation+mean_correlation*0.015, y = 1, 
     labels = paste0("Mean Corr = ", round(mean_correlation, 2)), col = 'blue', srt = 90, adj = 0, cex = 0.6)

# Annotate the pro/con correlation value
text(x = pro_con_correlation+pro_con_correlation*0.01, y = 1, 
     labels = paste0("Pro/Con Corr = ", round(pro_con_correlation, 2)), col = 'red', srt = 90, adj = 0, cex = 0.6)
# Close the graphics device
dev.off()

#calculating skewness and kurtosis of correlations distribution
semTools::skew(correlations)
semTools::kurtosis(correlations)



#1st wave CMB-corrected data (the same combinations are used)
component_data <- sub_auth_data[all_items_rwa_w1_decmb] %>%
  dplyr::select(order(names(.))) %>%
  dplyr::mutate(across(everything(), as.numeric))

#create an empty vector with 10000 slots to keep correlations between half-scales
correlations <- vector(length = 10000)

#calculate correlations between scores of 10000 selected half-scales and scores of remaining items
for (i in 1:10000) {
  half1 <- unlist(selected_combinations[i, ])
  score_1 <- rowMeans(component_data[, half1])
  score_2 <- rowMeans(component_data[, setdiff(1:20, half1)])
  correlations[i] <- cor(score_1, score_2)
}

#calculate correlation between pro-trait half-scale and con-trait half-scale
pro_con_correlation <- cor(rowMeans(component_data[pro_trait_items_rwa_w1_decmb]), 
                           rowMeans(component_data[con_trait_items_rwa_w1_decmb]))
#calculate mean correlation
mean_correlation <- mean(correlations)

# Open a PNG graphics device
png("corr_density_w1_CMB-corrected.png", width = 1200, height = 1100, res = 300)
#set margins
par(mar = c(3, 3, 2, 0.5), mgp = c(3, 0.5, 0))

#density plot (correlations)
density(correlations) %>% plot(main = "", xlab = "", ylab = "", lwd = 2, xlim = c(min(correlations), 1),
                               ylim = c(0, 12.5))

# Add title and axis labels with adjusted offsets
title(main = "CMB-corrected data", line = 0.5, , cex.main = 0.8)  # Adjust title offset
mtext("Correlations", side = 1, line = 1.6)  # Adjust x-axis label offset
mtext("Density", side = 2, line = 1.6)  # Adjust x-axis label offset

#add vertical line for correlation between pro-trait half-scale and con-trait half-scale
abline(v = pro_con_correlation,
       col = 'red', lwd = 2, lty = 'dashed')
#add vertical lines for 3 standard deviations from the mean correlation
abline(v = mean(correlations) - 3*sd(correlations), col = 'blue', lwd = 2, lty = 'dotted')
abline(v = mean(correlations) + 3*sd(correlations), col = 'blue', lwd = 2, lty = 'dotted')
#add vertical line for the mean correlation
abline(v = mean_correlation, col = 'blue', lwd = 2)

#add normal curve
x_vals <- seq(min(correlations), max(correlations), length.out = 100)
y_vals <- dnorm(x_vals, mean = mean(correlations), sd = sd(correlations))
lines(x_vals, y_vals, col = "green", lty='dashed', lwd = 2, add=TRUE)

# Annotate the mean value
text(x = mean_correlation+mean_correlation*0.015, y = 1, 
     labels = paste0("Mean Corr = ", round(mean_correlation, 2)), col = 'blue', srt = 90, adj = 0, cex = 0.6)

# Annotate the pro/con correlation value
text(x = pro_con_correlation+pro_con_correlation*0.01, y = 4, 
     labels = paste0("Pro/Con Corr = ", round(pro_con_correlation, 2)), col = 'red', srt = 90, adj = 0, cex = 0.6)
# Close the graphics device
dev.off()

#calculating skewness and kurtosis of correlations distribution
semTools::skew(correlations)
semTools::kurtosis(correlations)







#2nd wave, raw data (the same combinations are used)
component_data <- sub_auth_data[all_items_rwa_w2] %>%
  dplyr::select(order(names(.))) %>%
  dplyr::mutate(across(everything(), as.numeric))

#create an empty vector with 10000 slots to keep correlations between half-scales
correlations <- vector(length = 10000)

#calculate correlations between scores of 10000 selected half-scales and scores of remaining items
for (i in 1:10000) {
  half1 <- unlist(selected_combinations[i, ])
  score_1 <- rowMeans(component_data[, half1])
  score_2 <- rowMeans(component_data[, setdiff(1:20, half1)])
  correlations[i] <- cor(score_1, score_2)
}

#calculate correlation between pro-trait half-scale and con-trait half-scale
pro_con_correlation <- cor(rowMeans(component_data[pro_trait_items_rwa_w2]), 
                           rowMeans(component_data[con_trait_items_rwa_w2]))
#calculate mean correlation
mean_correlation <- mean(correlations)

# Open a PNG graphics device
png("corr_density_w2_raw.png", width = 1200, height = 1100, res = 300)
#set margins
par(mar = c(3, 3, 2, 0.5), mgp = c(3, 0.5, 0))

#density plot (correlations)
density(correlations) %>% plot(main = "", xlab = "", ylab = "", lwd = 2, 
                               xlim = c(min(correlations), max(correlations)+max(correlations)*0.1),
                               ylim = c(0, 12.5))
# Add title and axis labels with adjusted offsets
title(main = "Raw data", line = 0.5, , cex.main = 0.8)  # Adjust title offset
mtext("Correlations", side = 1, line = 1.6)  # Adjust x-axis label offset
mtext("Density", side = 2, line = 1.6)  # Adjust x-axis label offset

#add vertical line for correlation between pro-trait half-scale and con-trait half-scale
abline(v = pro_con_correlation,
       col = 'red', lwd = 2, lty = 'dashed')
#add vertical lines for 3 standard deviations from the mean correlation
abline(v = mean(correlations) - 3*sd(correlations), col = 'blue', lwd = 2, lty = 'dotted')
abline(v = mean(correlations) + 3*sd(correlations), col = 'blue', lwd = 2, lty = 'dotted')
#add vertical line for the mean correlation
abline(v = mean_correlation, col = 'blue', lwd = 2)

#add normal curve
x_vals <- seq(min(correlations), max(correlations), length.out = 100)
y_vals <- dnorm(x_vals, mean = mean(correlations), sd = sd(correlations))
lines(x_vals, y_vals, col = "green", lty='dashed', lwd = 2, add=TRUE)

# Annotate the mean value
text(x = mean_correlation+mean_correlation*0.015, y = 1, 
     labels = paste0("Mean Corr = ", round(mean_correlation, 2)), col = 'blue', srt = 90, adj = 0, cex = 0.6)

# Annotate the pro/con correlation value
text(x = pro_con_correlation+pro_con_correlation*0.02, y = 1, 
     labels = paste0("Pro/Con Corr = ", round(pro_con_correlation, 2)), col = 'red', srt = 90, adj = 0, cex = 0.6)
# Close the graphics device
dev.off()

#calculating skewness and kurtosis of correlations distribution
semTools::skew(correlations)
semTools::kurtosis(correlations)




#2nd wave ARS-corrected data (the same combinations are used)
component_data <- sub_auth_data[all_items_rwa_w2_residual] %>%
  dplyr::select(order(names(.))) %>%         # Arrange variables alphabetically
  dplyr::mutate(across(everything(), as.numeric)) # Convert all variables to numeric

#create an empty vector with 10000 slots to keep correlations between half-scales
correlations <- vector(length = 10000)

#calculate correlations between scores of 10000 selected half-scales and scores of remaining items
for (i in 1:10000) {
  half1 <- unlist(selected_combinations[i, ])
  score_1 <- rowMeans(component_data[, half1])
  score_2 <- rowMeans(component_data[, setdiff(1:20, half1)])
  correlations[i] <- cor(score_1, score_2)
}

#calculate correlation between pro-trait half-scale and con-trait half-scale
pro_con_correlation <- cor(rowMeans(component_data[pro_trait_items_rwa_w2_residual]), 
                           rowMeans(component_data[con_trait_items_rwa_w2_residual]))
#calculate mean correlation
mean_correlation <- mean(correlations)

# Open a PNG graphics device
png("corr_density_w2_ARS-corrected.png", width = 1200, height = 1100, res = 300)
#set margins
par(mar = c(3, 3, 2, 0.5), mgp = c(3, 0.5, 0))

#density plot (correlations)
density(correlations) %>% plot(main = "", xlab = "", ylab = "", lwd = 2, xlim = c(min(correlations), 1),
                               ylim = c(0, 12.5))

# Add title and axis labels with adjusted offsets
title(main = "ARS-corrected data", line = 0.5, , cex.main = 0.8)  # Adjust title offset
mtext("Correlations", side = 1, line = 1.6)  # Adjust x-axis label offset
mtext("Density", side = 2, line = 1.6)  # Adjust x-axis label offset

#add vertical line for correlation between pro-trait half-scale and con-trait half-scale
abline(v = pro_con_correlation,
       col = 'red', lwd = 2, lty = 'dashed')
#add vertical lines for 3 standard deviations from the mean correlation
abline(v = mean(correlations) - 3*sd(correlations), col = 'blue', lwd = 2, lty = 'dotted')
abline(v = mean(correlations) + 3*sd(correlations), col = 'blue', lwd = 2, lty = 'dotted')
#add vertical line for the mean correlation
abline(v = mean_correlation, col = 'blue', lwd = 2)

#add normal curve
x_vals <- seq(min(correlations), max(correlations), length.out = 100)
y_vals <- dnorm(x_vals, mean = mean(correlations), sd = sd(correlations))
lines(x_vals, y_vals, col = "green", lty='dashed', lwd = 2, add=TRUE)

# Annotate the mean value
text(x = mean_correlation+mean_correlation*0.015, y = 1, 
     labels = paste0("Mean Corr = ", round(mean_correlation, 2)), col = 'blue', srt = 90, adj = 0, cex = 0.6)

# Annotate the pro/con correlation value
text(x = pro_con_correlation+pro_con_correlation*0.01, y = 1, 
     labels = paste0("Pro/Con Corr = ", round(pro_con_correlation, 2)), col = 'red', srt = 90, adj = 0, cex = 0.6)
# Close the graphics device
dev.off()

  #calculating skewness and kurtosis of correlations distribution
semTools::skew(correlations)
semTools::kurtosis(correlations)



#2nd wave CMB-corrected data (the same combinations are used)
component_data <- sub_auth_data[all_items_rwa_w2_decmb] %>%
  dplyr::select(order(names(.))) %>%         # Arrange variables alphabetically
  dplyr::mutate(across(everything(), as.numeric)) # Convert all variables to numeric

#create an empty vector with 10000 slots to keep correlations between half-scales
correlations <- vector(length = 10000)

#calculate correlations between scores of 10000 selected half-scales and scores of remaining items
for (i in 1:10000) {
  half1 <- unlist(selected_combinations[i, ])
  score_1 <- rowMeans(component_data[, half1])
  score_2 <- rowMeans(component_data[, setdiff(1:20, half1)])
  correlations[i] <- cor(score_1, score_2)
}

#calculate correlation between pro-trait half-scale and con-trait half-scale
pro_con_correlation <- cor(rowMeans(component_data[pro_trait_items_rwa_w2_decmb]), 
                           rowMeans(component_data[con_trait_items_rwa_w2_decmb]))
#calculate mean correlation
mean_correlation <- mean(correlations)

# Open a PNG graphics device
png("corr_density_w2_CMB-corrected.png", width = 1200, height = 1100, res = 300)
#set margins
par(mar = c(3, 3, 2, 0.5), mgp = c(3, 0.5, 0))

#density plot (correlations)
density(correlations) %>% plot(main = "", xlab = "", ylab = "", lwd = 2, xlim = c(min(correlations), 1),
                               ylim = c(0, 12.5))

# Add title and axis labels with adjusted offsets
title(main = "CMB-corrected data", line = 0.5, , cex.main = 0.8)  # Adjust title offset
mtext("Correlations", side = 1, line = 1.6)  # Adjust x-axis label offset
mtext("Density", side = 2, line = 1.6)  # Adjust x-axis label offset

#add vertical line for correlation between pro-trait half-scale and con-trait half-scale
abline(v = pro_con_correlation,
       col = 'red', lwd = 2, lty = 'dashed')
#add vertical lines for 3 standard deviations from the mean correlation
abline(v = mean(correlations) - 3*sd(correlations), col = 'blue', lwd = 2, lty = 'dotted')
abline(v = mean(correlations) + 3*sd(correlations), col = 'blue', lwd = 2, lty = 'dotted')
#add vertical line for the mean correlation
abline(v = mean_correlation, col = 'blue', lwd = 2)

#add normal curve
x_vals <- seq(min(correlations), max(correlations), length.out = 100)
y_vals <- dnorm(x_vals, mean = mean(correlations), sd = sd(correlations))
lines(x_vals, y_vals, col = "green", lty='dashed', lwd = 2, add=TRUE)

# Annotate the mean value
text(x = mean_correlation+mean_correlation*0.015, y = 1, 
     labels = paste0("Mean Corr = ", round(mean_correlation, 2)), col = 'blue', srt = 90, adj = 0, cex = 0.6)

# Annotate the pro/con correlation value
text(x = pro_con_correlation+pro_con_correlation*0.01, y = 4, 
     labels = paste0("Pro/Con Corr = ", round(pro_con_correlation, 2)), col = 'red', srt = 90, adj = 0, 
     cex = 0.6)
# Close the graphics device
dev.off()

#calculating skewness and kurtosis of correlations distribution
semTools::skew(correlations)
semTools::kurtosis(correlations)




#calculate shares of content- and style-related variance in the scale items
#share of variance attributable to content and style
#wave 1
# Initialize a dataframe to store variance shares
variance_shares <- data.frame(
  Item = all_items_rwa_w1,
  Content_Share = NA,
  Style_Share = NA,
  Residual_Share = NA
)

# Loop through each item and calculate variance shares
for (item in all_items_rwa_w1) {
  # Fit a regression model for the item
  model <- lm(
    formula = sub_auth_data[[item]] ~ sub_auth_data$rwa_residual_score_w1 + sub_auth_data$rwa_acquiescence_score_w1,
    data = sub_auth_data
  )
  
  # Extract total variance of the item
  total_variance <- var(sub_auth_data[[item]], na.rm = TRUE)
  
  # Extract fitted values for each predictor
  content_fitted <- coef(model)["sub_auth_data$rwa_residual_score_w1"] * sub_auth_data$rwa_residual_score_w1
  style_fitted <- coef(model)["sub_auth_data$rwa_acquiescence_score_w1"] * sub_auth_data$rwa_acquiescence_score_w1
  
  # Calculate variances
  content_variance <- var(content_fitted, na.rm = TRUE)
  style_variance <- var(style_fitted, na.rm = TRUE)
  residual_variance <- total_variance - content_variance - style_variance
  
  # Calculate shares
  variance_shares[variance_shares$Item == item, "Content_Share"] <- round(content_variance / total_variance, digits = 2)
  variance_shares[variance_shares$Item == item, "Style_Share"] <- round(style_variance / total_variance, digits = 2)
  variance_shares[variance_shares$Item == item, "Residual_Share"] <- round(residual_variance / total_variance, digits = 2)
}

# View the variance shares
variance_shares
#write.table(variance_shares, "RWA_variance_shares_ARS_w1.csv", sep = ";", row.names = FALSE)


#shares of variance attributable to content and style
#wave 2
# Initialize a dataframe to store variance shares
variance_shares <- data.frame(
  Item = all_items_rwa_w2,
  Content_Share = NA,
  Style_Share = NA,
  Residual_Share = NA
)

# Loop through each item and calculate variance shares
for (item in all_items_rwa_w2) {
  # Fit a regression model for the item
  model <- lm(
    formula = sub_auth_data[[item]] ~ sub_auth_data$rwa_residual_score_w2 + sub_auth_data$rwa_acquiescence_score_w2,
    data = sub_auth_data
  )
  
  # Extract total variance of the item
  total_variance <- var(sub_auth_data[[item]], na.rm = TRUE)
  
  # Extract fitted values for each predictor
  content_fitted <- coef(model)["sub_auth_data$rwa_residual_score_w2"] * sub_auth_data$rwa_residual_score_w2
  style_fitted <- coef(model)["sub_auth_data$rwa_acquiescence_score_w2"] * sub_auth_data$rwa_acquiescence_score_w2
  
  # Calculate variances
  content_variance <- var(content_fitted, na.rm = TRUE)
  style_variance <- var(style_fitted, na.rm = TRUE)
  residual_variance <- total_variance - content_variance - style_variance
  
  # Calculate shares
  variance_shares[variance_shares$Item == item, "Content_Share"] <- round(content_variance / total_variance, digits = 2)
  variance_shares[variance_shares$Item == item, "Style_Share"] <- round(style_variance / total_variance, digits = 2)
  variance_shares[variance_shares$Item == item, "Residual_Share"] <- round(residual_variance / total_variance, digits = 2)
}

# View the variance shares
variance_shares
#write.table(variance_shares, "RWA_variance_shares_ARS_w2.csv", sep = ";", row.names = FALSE)



#measure share of content- and method-related variance in the scale items
#the same, using CMB procedure
#wave 1
# Initialize a dataframe to store variance shares
variance_shares <- data.frame(
  Item = all_items_rwa_w1,
  Content_Share = NA,
  Style_Share = NA,
  Residual_Share = NA
)

# Loop through each item and calculate variance shares
for (item in all_items_rwa_w1) {
  # Fit a regression model for the item
  model <- lm(
    formula = sub_auth_data[[item]] ~ sub_auth_data$rwa_decmb_score_w1 + sub_auth_data$rwa_CMB_score_w1,
    data = sub_auth_data
  )
  
  # Extract total variance of the item
  total_variance <- var(sub_auth_data[[item]], na.rm = TRUE)
  
  # Extract fitted values for each predictor
  content_fitted <- coef(model)["sub_auth_data$rwa_decmb_score_w1"] * sub_auth_data$rwa_decmb_score_w1
  style_fitted <- coef(model)["sub_auth_data$rwa_CMB_score_w1"] * sub_auth_data$rwa_CMB_score_w1
  
  # Calculate variances
  content_variance <- var(content_fitted, na.rm = TRUE)
  style_variance <- var(style_fitted, na.rm = TRUE)
  residual_variance <- total_variance - content_variance - style_variance
  
  # Calculate shares
  variance_shares[variance_shares$Item == item, "Content_Share"] <- round(content_variance / total_variance, digits = 2)
  variance_shares[variance_shares$Item == item, "Style_Share"] <- round(style_variance / total_variance, digits = 2)
  variance_shares[variance_shares$Item == item, "Residual_Share"] <- round(residual_variance / total_variance, digits = 2)
}

# View the variance shares
variance_shares
#write.table(variance_shares, "RWA_variance_shares_CMB_w1.csv", sep = ";", row.names = FALSE)


#share of variance attributable to content and style
#wave 2
# Initialize a dataframe to store variance shares
variance_shares <- data.frame(
  Item = all_items_rwa_w2,
  Content_Share = NA,
  Style_Share = NA,
  Residual_Share = NA
)

# Loop through each item and calculate variance shares
for (item in all_items_rwa_w2) {
  # Fit a regression model for the item
  model <- lm(
    formula = sub_auth_data[[item]] ~ sub_auth_data$rwa_decmb_score_w2 + sub_auth_data$rwa_CMB_score_w2,
    data = sub_auth_data
  )
  
  # Extract total variance of the item
  total_variance <- var(sub_auth_data[[item]], na.rm = TRUE)
  
  # Extract fitted values for each predictor
  content_fitted <- coef(model)["sub_auth_data$rwa_decmb_score_w2"] * sub_auth_data$rwa_decmb_score_w2
  style_fitted <- coef(model)["sub_auth_data$rwa_CMB_score_w2"] * sub_auth_data$rwa_CMB_score_w2
  
  # Calculate variances
  content_variance <- var(content_fitted, na.rm = TRUE)
  style_variance <- var(style_fitted, na.rm = TRUE)
  residual_variance <- total_variance - content_variance - style_variance
  
  # Calculate shares
  variance_shares[variance_shares$Item == item, "Content_Share"] <- round(content_variance / total_variance, digits = 2)
  variance_shares[variance_shares$Item == item, "Style_Share"] <- round(style_variance / total_variance, digits = 2)
  variance_shares[variance_shares$Item == item, "Residual_Share"] <- round(residual_variance / total_variance, digits = 2)
}

# View the variance shares
variance_shares
#write.table(variance_shares, "RWA_variance_shares_CMB_w2.csv", sep = ";", row.names = FALSE)











#exploratory factor analysis
#wave 1

#create separate datasets for each wave and data type with uniform variable names
data_deARS_w1 <- sub_auth_data %>% dplyr::select(all_of(all_items_rwa_w1_residual)) %>%  
  rename_with(~c(
    "rwa03", "rwa04", "rwa05", "rwa06", "rwa07", "rwa08", "rwa09", "rwa10", "rwa11", 
    "rwa12", "rwa13", "rwa14", "rwa15", "rwa16", "rwa17", "rwa18", "rwa19", "rwa20", "rwa21", "rwa22"
    
  ), .cols = everything())

data_deCMB_w1 <- sub_auth_data %>% dplyr::select(all_of(all_items_rwa_w1_decmb)) %>%  
  rename_with(~c(
    "rwa03", "rwa04", "rwa05", "rwa06", "rwa07", "rwa08", "rwa09", "rwa10", "rwa11", 
    "rwa12", "rwa13", "rwa14", "rwa15", "rwa16", "rwa17", "rwa18", "rwa19", "rwa20", "rwa21", "rwa22"
    
  ), .cols = everything())


#apply shrinkage procedure to enhance covariance matrix

raw_matrix_shrink_cov_w1 <- cov.shrink(data_w1) %>% unclass()
deARS_shrink_cov_w1 <- cov.shrink(data_deARS_w1) %>% unclass()
deCMB_shrink_cov_w1 <- cov.shrink(data_deCMB_w1) %>% unclass()



#scree plots to detect the number of factors
scree(data_w1, pc = TRUE, fa = TRUE, main = "1st wave, raw scores")
scree(raw_matrix_shrink_cov_w1, pc = TRUE, fa = TRUE, main = "1st wave, raw scores") 
scree(data_deARS_w1, pc = TRUE, fa = TRUE, main = "1st wave, ARS-corrected scores") 
scree(deARS_shrink_cov_w1, pc = TRUE, fa = TRUE, main = "1st wave, ARS-corrected scores") 
scree(data_deCMB_w1, pc = TRUE, fa = TRUE, main = "1st wave, CMB-corrected scores") 
scree(deCMB_shrink_cov_w1, pc = TRUE, fa = TRUE, main = "1st wave, CMB-corrected scores") 


eigen(cor(data_w1))$values %>% round(digits = 3)
eigen(cor(raw_matrix_shrink_cov_w1))$values %>% round(digits = 3)
eigen(cor(data_deARS_w1))$values %>% round(digits = 3)
eigen(cor(deARS_shrink_cov_w1))$values %>% round(digits = 3)
eigen(cor(data_deCMB_w1))$values %>% round(digits = 3)
eigen(deCMB_shrink_cov_w1)$values %>% round(digits = 3)


#1-factor solution
##raw data
data_fa <- fa(raw_matrix_shrink_cov_w1, n.obs = nrow(data_w1), nfactors = 1, rotate = "promax", fm="pa")
summary(data_fa, fit.measures = TRUE)
data_fa$loadings
print(data_fa$loadings, cutoff = 0.2)
#write.table(data_fa$loadings %>% round(digits = 3), "RWA_EFA_RAW_1f_w1.csv", sep = ";")

##deARS data
data_fa_deARS <- fa(deARS_shrink_cov_w1, n.obs = nrow(data_deARS_w1), nfactors = 1, rotate = "promax", fm="pa")
summary(data_fa_deARS, fit.measures = TRUE)
data_fa_deARS$loadings
print(data_fa_deARS$loadings, cutoff = 0.2)
#write.table(data_fa_deARS$loadings %>% round(digits = 3), "RWA_EFA_ARS_corrected_1f_w1.csv", sep = ";")

##deCMB data
data_fa_deCMB <- fa(deCMB_shrink_cov_w1, n.obs = nrow(data_deCMB_w1), nfactors = 1, rotate = "promax", fm="pa")
summary(data_fa_deCMB, fit.measures = TRUE)
data_fa_deCMB$loadings
print(data_fa_deCMB$loadings, cutoff = 0.2)
#write.table(data_fa_deCMB$loadings %>% round(digits = 3), "RWA_EFA_CMB_corrected_1f_w1.csv", sep = ";")


#2-factor solution
##raw data
data_fa <- fa(raw_matrix_shrink_cov_w1, n.obs = nrow(data_w1), nfactors = 2, rotate = "promax", fm="pa")
summary(data_fa, fit.measures = TRUE)
data_fa$loadings
print(data_fa$loadings, cutoff = 0.2)
#write.table(data_fa$loadings %>% round(digits = 3), "RWA_EFA_RAW_2f_w1.csv", sep = ";")

##deARS data
data_fa_deARS <- fa(deARS_shrink_cov_w1, n.obs = nrow(data_deARS_w1), nfactors = 2, rotate = "promax", fm="pa")
summary(data_fa_deARS, fit.measures = TRUE)
data_fa_deARS$loadings
print(data_fa_deARS$loadings, cutoff = 0.2)
#write.table(data_fa_deARS$loadings %>% round(digits = 3), "RWA_EFA_ARS_corrected_2f_w1.csv", sep = ";")

##deCMB data
data_fa_deCMB <- fa(deCMB_shrink_cov_w1, n.obs = nrow(data_deCMB_w1), nfactors = 2, rotate = "promax", fm="pa")
summary(data_fa_deCMB, fit.measures = TRUE)
data_fa_deCMB$loadings
print(data_fa_deCMB$loadings, cutoff = 0.2)
#write.table(data_fa_deCMB$loadings %>% round(digits = 3), "RWA_EFA_CMB_corrected_2f_w1.csv", sep = ";")


#3-factor solution
##raw data
data_fa <- fa(raw_matrix_shrink_cov_w1, n.obs = nrow(data_w1), nfactors = 3, rotate = "promax", fm="pa")
summary(data_fa, fit.measures = TRUE)
data_fa$loadings
print(data_fa$loadings, cutoff = 0.2)
#write.table(data_fa$loadings %>% round(digits = 3), "RWA_EFA_RAW_3f_w1.csv", sep = ";")

##deARS data
data_fa_deARS <- fa(deARS_shrink_cov_w1, n.obs = nrow(data_deARS_w1), nfactors = 3, rotate = "promax", fm="pa")
summary(data_fa_deARS, fit.measures = TRUE)
data_fa_deARS$loadings
print(data_fa_deARS$loadings, cutoff = 0.2)
#write.table(data_fa_deARS$loadings %>% round(digits = 3), "RWA_EFA_ARS_corrected_3f_w1.csv", sep = ";")

##deCMB data
data_fa_deCMB <- fa(deCMB_shrink_cov_w1, n.obs = nrow(data_deCMB_w1), nfactors = 3, rotate = "promax", fm="pa")
summary(data_fa_deCMB, fit.measures = TRUE)
data_fa_deCMB$loadings
print(data_fa_deCMB$loadings, cutoff = 0.2)
#write.table(data_fa_deCMB$loadings %>% round(digits = 3), "RWA_EFA_CMB_corrected_3f_w1.csv", sep = ";")



#exploratory factor analysis
#wave 2
#create separate datasets for each wave and data type with uniform variable names

data_deARS_w2 <- sub_auth_data %>% dplyr::select(all_of(all_items_rwa_w2_residual)) %>%  
  rename_with(~c(
    "rwa03", "rwa04", "rwa05", "rwa06", "rwa07", "rwa08", "rwa09", "rwa10", "rwa11", 
    "rwa12", "rwa13", "rwa14", "rwa15", "rwa16", "rwa17", "rwa18", "rwa19", "rwa20", "rwa21", "rwa22"
    
  ), .cols = everything())

data_deCMB_w2 <- sub_auth_data %>% dplyr::select(all_of(all_items_rwa_w2_decmb)) %>%  
  rename_with(~c(
    "rwa03", "rwa04", "rwa05", "rwa06", "rwa07", "rwa08", "rwa09", "rwa10", "rwa11", 
    "rwa12", "rwa13", "rwa14", "rwa15", "rwa16", "rwa17", "rwa18", "rwa19", "rwa20", "rwa21", "rwa22"
    
  ), .cols = everything())

#apply shrinkage procedure to enhance covariance matrix
raw_matrix_shrink_cov_w2 <- cov.shrink(data_w2) %>% unclass()
deARS_shrink_cov_w2 <- cov.shrink(data_deARS_w2) %>% unclass()
deCMB_shrink_cov_w2 <- cov.shrink(data_deCMB_w2) %>% unclass()


#scree plots to detect the number of factors
scree(data_w2, pc = TRUE, fa = TRUE, main = "2nd wave, raw scores")  
scree(raw_matrix_shrink_cov_w2, pc = TRUE, fa = TRUE, main = "2nd wave, raw scores") 
scree(data_deARS_w2, pc = TRUE, fa = TRUE, main = "2nd wave, ARS-corrected scores") 
scree(deARS_shrink_cov_w2, pc = TRUE, fa = TRUE, main = "2nd wave, ARS-corrected scores") 
scree(data_deCMB_w2, pc = TRUE, fa = TRUE, main = "2nd wave, CMB-corrected scores") 
scree(deCMB_shrink_cov_w2, pc = TRUE, fa = TRUE, main = "2nd wave, CMB-corrected scores") 


eigen(cor(data_w2))$values %>% round(digits = 3)
eigen(raw_matrix_shrink_cov_w2)$values %>% round(digits = 3)
eigen(cor(data_deARS_w2))$values %>% round(digits = 3)
eigen(deARS_shrink_cov_w2)$values %>% round(digits = 3)
eigen(cor(data_deCMB_w2))$values %>% round(digits = 3)
eigen(deCMB_shrink_cov_w2)$values %>% round(digits = 3)


#1-factor solution
##raw data
data_fa <- fa(raw_matrix_shrink_cov_w2, n.obs = nrow(data_w2), nfactors = 1, rotate = "promax", fm="pa")
summary(data_fa, fit.measures = TRUE)
data_fa$loadings
print(data_fa$loadings, cutoff = 0.2)
#write.table(data_fa$loadings %>% round(digits = 3), "RWA_EFA_RAW_1f_w2.csv", sep = ";")

##deARS data
data_fa_deARS <- fa(deARS_shrink_cov_w2, n.obs = nrow(data_deARS_w2), nfactors = 1, rotate = "promax", fm="pa")
summary(data_fa_deARS, fit.measures = TRUE)
data_fa_deARS$loadings
print(data_fa_deARS$loadings, cutoff = 0.2)
#write.table(data_fa_deARS$loadings %>% round(digits = 3), "RWA_EFA_ARS_corrected_1f_w2.csv", sep = ";")

##deCMB data
data_fa_deCMB <- fa(deCMB_shrink_cov_w2, n.obs = nrow(data_deCMB_w2), nfactors = 1, rotate = "promax", fm="pa")
summary(data_fa_deCMB, fit.measures = TRUE)
data_fa_deCMB$loadings
print(data_fa_deCMB$loadings, cutoff = 0.2)
#write.table(data_fa_deCMB$loadings %>% round(digits = 3), "RWA_EFA_CMB_corrected_1f_w2.csv", sep = ";")


#2-factor solution
##raw data
data_fa <- fa(raw_matrix_shrink_cov_w2, n.obs = nrow(data_w2), nfactors = 2, rotate = "promax", fm="pa")
summary(data_fa, fit.measures = TRUE)
data_fa$loadings
print(data_fa$loadings, cutoff = 0.2)
#write.table(data_fa$loadings %>% round(digits = 3), "RWA_EFA_RAW_2f_w2.csv", sep = ";")

##deARS data
data_fa_deARS <- fa(deARS_shrink_cov_w2, n.obs = nrow(data_deARS_w2), nfactors = 2, rotate = "promax", fm="pa")
summary(data_fa_deARS, fit.measures = TRUE)
data_fa_deARS$loadings
print(data_fa_deARS$loadings, cutoff = 0.2)
#write.table(data_fa_deARS$loadings %>% round(digits = 3), "RWA_EFA_ARS_corrected_2f_w2.csv", sep = ";")

##deCMB data
data_fa_deCMB <- fa(deCMB_shrink_cov_w2, n.obs = nrow(data_deCMB_w2), nfactors = 2, rotate = "promax", fm="pa")
summary(data_fa_deCMB, fit.measures = TRUE)
data_fa_deCMB$loadings
print(data_fa_deCMB$loadings, cutoff = 0.2)
#write.table(data_fa_deCMB$loadings %>% round(digits = 3), "RWA_EFA_CMB_corrected_2f_w2.csv", sep = ";")


#3-factor solution
##raw data
data_fa <- fa(raw_matrix_shrink_cov_w2, n.obs = nrow(data_w2), nfactors = 3, rotate = "promax", fm="pa")
summary(data_fa, fit.measures = TRUE)
data_fa$loadings
print(data_fa$loadings, cutoff = 0.2)
#write.table(data_fa$loadings %>% round(digits = 3), "RWA_EFA_RAW_3f_w2.csv", sep = ";")

##deARS data
data_fa_deARS <- fa(deARS_shrink_cov_w2, n.obs = nrow(data_deARS_w2), nfactors = 3, rotate = "promax", fm="pa")
summary(data_fa_deARS, fit.measures = TRUE)
data_fa_deARS$loadings
print(data_fa_deARS$loadings, cutoff = 0.2)
#write.table(data_fa_deARS$loadings %>% round(digits = 3), "RWA_EFA_ARS_corrected_3f_w2.csv", sep = ";")

##deCMB data
data_fa_deCMB <- fa(deCMB_shrink_cov_w2, n.obs = nrow(data_deCMB_w2), nfactors = 3, rotate = "promax", fm="pa")
summary(data_fa_deCMB, fit.measures = TRUE)
data_fa_deCMB$loadings
print(data_fa_deCMB$loadings, cutoff = 0.2)
#write.table(data_fa_deCMB$loadings %>% round(digits = 3), "RWA_EFA_CMB_corrected_3f_w2.csv", sep = ";")


#confirmatory factor analysis
#function to calculate SABIC
SABIC <- function(cfa_output) {
  # Extract required fit measures
  logl <- fitmeasures(cfa_output, "logl") %>% as.vector()
  npar <- fitmeasures(cfa_output, "npar") %>% as.vector()
  ntotal <- fitmeasures(cfa_output, "ntotal") %>% as.vector()
  
  # Compute SABIC
  sabic <- -2 * logl + npar * log((ntotal + 2) / 24)
  
  # Return SABIC value
  return(sabic)
}

#function to calculate AICc
AICc <- function(cfa_output) {
  # Extract required fit measures
  logl <- fitmeasures(cfa_output, "logl") %>% as.vector()
  npar <- fitmeasures(cfa_output, "npar") %>% as.vector()
  ntotal <- fitmeasures(cfa_output, "ntotal") %>% as.vector()
  
  # Compute AIC
  AIC <- -2 * logl + 2 * npar
  
  # Compute AICc (corrected AIC)
  AICc <- AIC + (2 * npar * (npar + 1)) / (ntotal - npar - 1)
  
  # Return AICc value
  return(AICc)
}




#Confirmatory Factor Analysis
#1-factor
model_rwa_1factor <- "
      AUTH =~ rwa03 + rwa04 + rwa05 + rwa06 + rwa07 + rwa08 + rwa09 + rwa10 + rwa11 + rwa12 + 
              rwa13 + rwa14 + rwa15 + rwa16 + rwa17 + rwa18 + rwa19 + rwa20 + rwa21 + rwa22 
      "
#Fit 1st wave raw data
cfa_output <- lavaan::cfa(model_rwa_1factor, data_w1, std.lv=TRUE, estimator="MLR")
fitmeasures(cfa_output, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "ecvi", "aic", "bic", "bic2", "gfi"))


#Fit 2nd wave raw data
cfa_output <- lavaan::cfa(model_rwa_1factor, data_w2, std.lv=TRUE, estimator="MLR")
fitmeasures(cfa_output, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "ecvi", "aic", "bic", "bic2", "gfi"))


#Fit 1st wave de-ARS data
cfa_output <- lavaan::cfa(model_rwa_1factor, sample.cov = deARS_shrink_cov_w1, 
                          sample.nobs = nrow(data_deARS_w1))
fitmeasures(cfa_output, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "ecvi", "aic", "bic", "bic2", "gfi"))


#Fit 2nd wave de-ARS data
cfa_output <- lavaan::cfa(model_rwa_1factor, sample.cov = deARS_shrink_cov_w2, 
                          sample.nobs = nrow(data_deARS_w2))
fitmeasures(cfa_output, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "ecvi", "aic", "bic", "bic2", "gfi"))


#Fit 1st wave de-CMB data
cfa_output <- lavaan::cfa(model_rwa_1factor, sample.cov = deCMB_shrink_cov_w1, 
                          sample.nobs = nrow(data_deCMB_w1), std.lv=TRUE)
fitmeasures(cfa_output, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "ecvi", "aic", "bic", "bic2", "gfi"))


#Fit 2nd wave de-CMB data
cfa_output <- lavaan::cfa(model_rwa_1factor, sample.cov = deCMB_shrink_cov_w2, 
                          sample.nobs = nrow(data_deCMB_w2), std.lv=TRUE)
fitmeasures(cfa_output, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "ecvi", "aic", "bic", "bic2", "gfi"))

SABIC(cfa_output)
AICc(cfa_output)
summary(cfa_output, standardized = TRUE, fit.measures = TRUE)
semPaths(cfa_output, what = "paths", whatLabels = "stand")
inspect(cfa_output, "r2")
modindices(cfa_output, minimum.value = 5, sort = TRUE)



#2-factor model for raw data, wave 1
model_rwa_2factor <- "
      F1 =~ rwa04 + rwa06 + rwa09 + rwa11 + rwa12 + rwa13 + rwa16 + rwa18 + rwa20 + rwa21 + 
            rwa22 + rwa10
      F2 =~ rwa03 + rwa05 + rwa07 + rwa08 + rwa10 + rwa14 + rwa15 + rwa17 + rwa19 + rwa12 + rwa16 + 
            rwa22
      AUTH =~ a*F1 + a*F2
      "
cfa_output <- lavaan::cfa(model_rwa_2factor, data_w1, std.lv=TRUE, estimator="MLR")
fitmeasures(cfa_output, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "ecvi", "aic", "bic", "bic2", "gfi"))
SABIC(cfa_output)
AICc(cfa_output)

prediction_20_raw_2f_w1 <- lavPredict(cfa_output)

#2-factor model for raw data, wave 2
model_rwa_2factor <- "
      F1 =~ rwa04 + rwa06 + rwa11 + rwa12 + rwa13 + rwa15 + rwa16 + rwa18 + rwa20 + rwa21 + rwa09
      F2 =~ rwa03 + rwa05 + rwa07 + rwa08 + rwa09 + rwa10 + rwa14 + rwa17 + rwa19 + rwa22 + rwa12 + rwa16
      AUTH =~ a*F1 + a*F2
      "
cfa_output <- lavaan::cfa(model_rwa_2factor, data_w2, std.lv=TRUE, estimator="MLR")
fitmeasures(cfa_output, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "ecvi", "aic", "bic", "bic2", "gfi"))
SABIC(cfa_output)
AICc(cfa_output)

prediction_20_raw_2f_w2 <- lavPredict(cfa_output)

#3-factor model for raw data, wave 1
model_rwa_3factor <- "
                F1 =~ rwa04 + rwa06 + rwa11 + rwa12 + rwa13 + rwa16 + rwa18 + rwa20 + rwa21 + 
                      rwa22 + rwa09 + rwa10
                F2 =~ rwa03 + rwa05 + rwa07 + rwa10 + rwa14 + rwa17 + rwa19 + rwa12 + rwa16
                F3 =~ rwa08 + rwa09 + rwa15 + rwa06 + rwa13 + rwa17 + rwa19 + rwa21 + rwa22
                AUTH =~ F1 + F2 + F3
"
cfa_output <- lavaan::cfa(model_rwa_3factor, data=data_w1, std.lv=TRUE, estimator="MLR")
fitmeasures(cfa_output, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "ecvi", "aic", "bic", "bic2", "gfi"))
SABIC(cfa_output)
AICc(cfa_output)

prediction_20_raw_w1 <- lavPredict(cfa_output)
cor(prediction_20_raw_w1)

#3-factor model for raw data, wave 2
model_rwa_3factor <- "
                F1 =~ rwa04 + rwa11 + rwa12 + rwa13 + rwa15 + rwa18 + rwa20 + rwa21 + rwa06 + 
                      rwa09 + rwa16
                F2 =~ rwa03 + rwa05 + rwa06 + rwa07 + rwa10 + rwa14 + rwa16 + rwa17 + rwa19 + rwa12
                F3 =~ rwa09 + rwa08 + rwa22 + rwa04 + rwa10 + rwa11 + rwa14
                AUTH =~ F1 + F2 + F3
"
cfa_output <- lavaan::cfa(model_rwa_3factor, data=data_w2, std.lv=TRUE, estimator="MLR")
fitmeasures(cfa_output, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "ecvi", "aic", "bic", "bic2", "gfi"))
summary(cfa_output, standardized = TRUE, fit.measures = TRUE)
SABIC(cfa_output)
AICc(cfa_output)

prediction_20_raw_w2 <- lavPredict(cfa_output)
cor(prediction_20_raw_w2)

#CMB-corrected data
#2-factor model, wave 1, CMB-corrected
model_rwa_2factor <- "
      F1 =~ rwa03 + rwa04 + rwa05 + rwa06 + rwa07 + rwa10 + rwa11 + rwa12 + rwa14 + rwa16 + rwa18 +
              rwa20 + rwa21 +
              #cross-loadings:
              rwa13 + rwa22
      F2 =~ rwa08 + rwa09 + rwa13 + rwa15 + rwa17 + rwa19 + rwa22 +
              #cross-loadings:
              rwa06 + rwa21
      AUTH =~ a*F1 + a*F2
      "

cfa_output <- lavaan::cfa(model_rwa_2factor, sample.cov = deCMB_shrink_cov_w1, 
                          sample.nobs = nrow(data_deCMB_w1), std.lv=TRUE)
fitmeasures(cfa_output, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                          "rni", "mfi", "gfi"))
summary(cfa_output, standardized = TRUE, fit.measures = TRUE)
SABIC(cfa_output)
AICc(cfa_output)


#2-factor model, wave 2, CMB-corrected
model_rwa_2factor <- "
      F1 =~ rwa03 + rwa05 + rwa06 + rwa07 + rwa12 + rwa16 + rwa17 + rwa18 + rwa19 + rwa20 + 
              rwa21 + rwa22 + 
              #cross-loadings:
              rwa10 + rwa13
      F2 =~ rwa04 + rwa08 + rwa09 + rwa10 + rwa11 + rwa13 + rwa14 + rwa15 + 
              #cross-loadings:
              rwa12 + rwa21 + rwa22
      AUTH =~ a*F1 + a*F2
      "

cfa_output <- lavaan::cfa(model_rwa_2factor, sample.cov = deCMB_shrink_cov_w2, 
                          sample.nobs = nrow(data_deCMB_w2), std.lv=TRUE)
fitmeasures(cfa_output, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                          "rni", "mfi", "gfi"))
summary(cfa_output, standardized = TRUE, fit.measures = TRUE)
AICc(cfa_output)
SABIC(cfa_output)


#3-factor model, wave 1, CMB-corrected
model_rwa_3factor <- "
                F1 =~ rwa03 + rwa04 + rwa05 + rwa07 + rwa12 + rwa16 + rwa18 + rwa22 + 
                        #cross-loadings:
                        rwa10 + rwa21
                F2 =~ rwa10 + rwa11 + rwa14 + rwa20 + rwa21 + 
                        #cross-loadings:
                        rwa03 + rwa06 + rwa07 + rwa09 + rwa12 + rwa13 + rwa16
                F3 =~ rwa06 + rwa08 + rwa09 + rwa13 + rwa15 + rwa17 + rwa19 + 
                        #cross-loadings:
                        rwa21 + rwa22
                AUTH =~ F1 + F2 + F3
                    "

cfa_output_3f_w1 <- lavaan::cfa(model_rwa_3factor, sample.cov = deCMB_shrink_cov_w1, 
                          sample.nobs = nrow(data_deCMB_w1), std.lv=TRUE)
fitmeasures(cfa_output_3f_w1, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                          "rni", "mfi", "gfi")) 
summary(cfa_output_3f_w1, standardized = TRUE, fit.measures = TRUE)
semPaths(cfa_output_3f_w1, what = "paths", whatLabels = "stand")
AICc(cfa_output_3f_w1)
SABIC(cfa_output_3f_w1)


#3-factor model, wave 2, CMB-corrected
model_rwa_3factor <- "
                F1 =~ rwa04 + rwa08 + rwa09 + rwa10 + rwa11 + rwa12 + rwa21 + rwa22 + 
                        #cross-loadings:
                        rwa07 + rwa13
                F2 =~ rwa03 + rwa05 + rwa06 + rwa07 + rwa16 + rwa17 + rwa18 + rwa19 + rwa20 + 
                        #cross-loadings:
                        rwa12 + rwa13 + rwa21
                F3 =~ rwa13 + rwa14 + rwa15 + 
                        #cross-loadings:
                        rwa03 + rwa09 + rwa19
                AUTH =~ F1 + F2 + F3
"

cfa_output_3f_w2 <- lavaan::cfa(model_rwa_3factor, sample.cov = deCMB_shrink_cov_w2, 
                          sample.nobs = nrow(data_deCMB_w2), std.lv=TRUE)
fitmeasures(cfa_output_3f_w2, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                          "rni", "mfi", "gfi")) 
summary(cfa_output_3f_w2, standardized = TRUE, fit.measures = TRUE)
semPaths(cfa_output_3f_w2, what = "paths", whatLabels = "stand")
AICc(cfa_output_3f_w2)
SABIC(cfa_output_3f_w2)



#inspecting subdomains
prediction_3f_w1 <- lavPredict(cfa_output_3f_w1, newdata = data_deCMB_w1) %>% as.data.frame()
prediction_3f_w2 <- lavPredict(cfa_output_3f_w2, newdata = data_deCMB_w2) %>% as.data.frame()

corr.test(prediction_3f_w1)
corr.test(prediction_3f_w2)

cor.test(prediction_3f_w1$F1, prediction_3f_w2$F1)
cor.test(prediction_3f_w1$F2, prediction_3f_w2$F2)
cor.test(prediction_3f_w1$F3, prediction_3f_w2$F3)

cor.test(prediction_3f_w1$F1, prediction_3f_w2$F3)
cor.test(prediction_3f_w1$F2, prediction_3f_w2$F3)
cor.test(prediction_3f_w1$F3, prediction_3f_w2$F2)


# Compute ICC for absolute agreement (ICC(2,1) - test-retest reliability)
icc_result_F1 <- icc(cbind(prediction_3f_w1$F1, prediction_3f_w2$F1), model = "twoway", type = "agreement", unit = "single")
print(icc_result_F1)

icc_result_F2 <- icc(cbind(prediction_3f_w1$F2, prediction_3f_w2$F2), model = "twoway", type = "agreement", unit = "single")
print(icc_result_F2)

icc_result_F3 <- icc(cbind(prediction_3f_w1$F3, prediction_3f_w2$F3), model = "twoway", type = "agreement", unit = "single")
print(icc_result_F3)


icc_result_AUTH <- icc(cbind(prediction_3f_w1$AUTH, prediction_3f_w2$AUTH), model = "twoway", type = "agreement", unit = "single")
print(icc_result_AUTH)

cor.test(prediction_3f_w1$AUTH, prediction_3f_w2$AUTH)




#models derived from CMB-corrected data, fitted to raw data with CMB factor

model_rwa20_1factor_1meth <- "
      AUTH =~ rwa03 + rwa04 + rwa05 + rwa06 + rwa07 + rwa08 + rwa09 + rwa10 + rwa11 + rwa12 + 
              rwa13 + rwa14 + rwa15 + rwa16 + rwa17 + rwa18 + rwa19 + rwa20 + rwa21 + rwa22 
      CMB =~ rwa03 + rwa04 + rwa05 + rwa06 + rwa07 + rwa08 + rwa09 + rwa10 + rwa11 + rwa12 + 
              rwa13 + rwa14 + rwa15 + rwa16 + rwa17 + rwa18 + rwa19 + rwa20 + rwa21 + rwa22
      AUTH ~~ 0*CMB
      AUTH ~~ 1*AUTH
      "

cfa_output_1factor_1meth_w1 <- lavaan::cfa(model_rwa20_1factor_1meth, data = data_w1, 
                                           estimator="MLR")
cfa_output_1factor_1meth_w2 <- lavaan::cfa(model_rwa20_1factor_1meth, data = data_w2, 
                                           estimator="MLR")

fitmeasures(cfa_output_1factor_1meth_w1, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                "rni", "mfi", "gfi")) 
fitmeasures(cfa_output_1factor_1meth_w2, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                           "rni", "mfi", "gfi")) 

SABIC(cfa_output_1factor_1meth_w1)
SABIC(cfa_output_1factor_1meth_w2)

AICc(cfa_output_1factor_1meth_w1)
AICc(cfa_output_1factor_1meth_w2)

summary(cfa_output_1factor_1meth_w1, standardized = TRUE, fit.measures = TRUE)
summary(cfa_output_1factor_1meth_w2, standardized = TRUE, fit.measures = TRUE)

semPaths(cfa_output_1factor_1meth_w1, what = "paths", whatLabels = "stand")
semPaths(cfa_output_1factor_1meth_w2, what = "paths", whatLabels = "stand")



#2-factor bifactor models derived from CMB-corrected data with CMB factor
##wave 1
model_rwa20_2factor_1meth_w1 <- "
      F1 =~ rwa03 + rwa04 + rwa05 + rwa06 + rwa07 + rwa10 + rwa11 + rwa12 + rwa14 + rwa16 + rwa18 +
              rwa20 + rwa21 +
              #cross-loadings:
              rwa13 + rwa22
      F2 =~ rwa08 + rwa09 + rwa13 + rwa15 + rwa17 + rwa19 + rwa22 +
              #cross-loadings:
              rwa06 + rwa21
      CMB =~ rwa03 + rwa04 + rwa05 + rwa06 + rwa07 + rwa08 + rwa09 + rwa10 + rwa11 + rwa12 + 
              rwa13 + rwa14 + rwa15 + rwa16 + rwa17 + rwa18 + rwa19 + rwa20 + rwa21 + rwa22
      F1 ~~ 1*F1
      F2 ~~ 1*F2
      AUTH =~ F1 + F2
      AUTH ~~ 0*CMB
      AUTH ~~ 1*AUTH
              #rwa11 ~~ rwa20
              #rwa03 ~~ rwa14

      "

#wave 2
model_rwa20_2factor_1meth_w2 <- "
      F1 =~ rwa03 + rwa05 + rwa06 + rwa07 + rwa12 + rwa16 + rwa17 + rwa18 + rwa19 + rwa20 + 
              rwa21 + rwa22 + 
              #cross-loadings:
              rwa10 + rwa13
      F2 =~ rwa04 + rwa08 + rwa09 + rwa10 + rwa11 + rwa13 + rwa14 + rwa15 + 
              #cross-loadings:
              rwa12 + rwa21 + rwa22
      CMB =~ rwa03 + rwa04 + rwa05 + rwa06 + rwa07 + rwa08 + rwa09 + rwa10 + rwa11 + rwa12 + 
              rwa13 + rwa14 + rwa15 + rwa16 + rwa17 + rwa18 + rwa19 + rwa20 + rwa21 + rwa22
      F1 ~~ 1*F1
      F2 ~~ 1*F2
      AUTH =~ F1 + F2
      AUTH ~~ 0*CMB
      AUTH ~~ 1*AUTH
#      rwa03 ~~ rwa14
#      rwa12 ~~ rwa16

      "

cfa_output_2factor_1meth_w1 <- lavaan::cfa(model_rwa20_2factor_1meth_w1, data = data_w1, 
                                           estimator="MLR", std.lv=FALSE)
cfa_output_2factor_1meth_w2 <- lavaan::cfa(model_rwa20_2factor_1meth_w2, data = data_w2, 
                                           estimator="MLR", std.lv=FALSE)

fitmeasures(cfa_output_2factor_1meth_w1, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                           "rni", "mfi", "gfi")) 
fitmeasures(cfa_output_2factor_1meth_w2, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                           "rni", "mfi", "gfi")) 

SABIC(cfa_output_2factor_1meth_w1)
SABIC(cfa_output_2factor_1meth_w2)

AICc(cfa_output_2factor_1meth_w1)
AICc(cfa_output_2factor_1meth_w2)


summary(cfa_output_2factor_1meth_w1, standardized = TRUE, fit.measures = TRUE)
summary(cfa_output_2factor_1meth_w2, standardized = TRUE, fit.measures = TRUE)

semPaths(cfa_output_2factor_1meth_w1, what = "paths", whatLabels = "stand")
semPaths(cfa_output_2factor_1meth_w2, what = "paths", whatLabels = "stand")

modindices(cfa_output_2factor_1meth_w1, minimum.value = 5, sort = TRUE)
modindices(cfa_output_2factor_1meth_w2, minimum.value = 5, sort = TRUE)

prediction_20_CMB_methF_2f_w1 <- lavPredict(cfa_output_2factor_1meth_w1)
prediction_20_CMB_methF_2f_w2 <- lavPredict(cfa_output_2factor_1meth_w2)


#3-factor bifactor model derived from CMB-corrected data with CMB factor
#wave 1

model_rwa20_3factor_1meth_w1 <- "
                F1 =~ rwa03 + rwa04 + rwa05 + rwa07 + rwa12 + rwa16 + rwa18 + rwa22 + 
                        #cross-loadings:
                        rwa10 + rwa21
                F2 =~ rwa10 + rwa11 + rwa14 + rwa21 + rwa20 + 
                        #cross-loadings:
                        rwa06 + rwa07 + rwa09 + rwa12 + rwa13 + rwa16 + rwa17
                F3 =~ rwa06 + rwa08 + rwa09 + rwa13 + rwa15 + rwa17 + rwa19 + 
                        #cross-loadings:
                        rwa21 + rwa22
                AUTH =~ F1 + F2 + F3
                CMB =~ rwa03 + rwa04 + rwa05 + rwa06 + rwa07 + rwa08 + rwa09 + rwa10 + rwa11 + rwa12 + 
                        rwa13 + rwa14 + rwa15 + rwa16 + rwa17 + rwa18 + rwa19 + rwa20 + rwa21 + rwa22
              F1 ~~ 1*F1
              F2 ~~ 1*F2
              F3 ~~ 1*F3
              AUTH ~~ 1*AUTH
              AUTH ~~ 0*CMB
             rwa11 ~~ rwa20
             rwa03 ~~ rwa14
              "

cfa_output_3factor_1meth_w1 <- lavaan::cfa(model_rwa20_3factor_1meth_w1, data = data_w1, 
                                           estimator="MLR", std.lv=FALSE)
fitmeasures(cfa_output_3factor_1meth_w1, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                           "rni", "mfi", "gfi")) 
SABIC(cfa_output_3factor_1meth_w1)
AICc(cfa_output_3factor_1meth_w1)

summary(cfa_output_3factor_1meth_w1, standardized = TRUE, fit.measures = TRUE)
semPaths(cfa_output_3factor_1meth_w1, what = "paths", whatLabels = "stand")
modindices(cfa_output_3factor_1meth_w1, minimum.value = 10, sort = TRUE)
inspect(cfa_output_3factor_1meth_w1, "r2")
residuals_wave1_cor <- lavResiduals(cfa_output_3factor_1meth_w1, type = "cor")$cov
print(residuals_wave1_cor)




#3-factor bifactor model derived from CMB-corrected data with CMB factor
#wave 2

model_rwa20_3factor_1meth_w2 <- "
                F1 =~ rwa03 + rwa05 + rwa06 + rwa07 + rwa16 + rwa17 + rwa18 + rwa19 + rwa20 + 
                        #cross-loadings:
                        rwa12 + rwa13 + rwa21
                F2 =~ rwa04 + rwa08 + rwa09 + rwa10 + rwa11 + rwa12 + rwa21 + rwa22 + 
                        #cross-loadings:
                        rwa13
                F3 =~ rwa13 + rwa14 + rwa15 + 
                        #cross-loadings:
                        rwa09 + rwa19
                AUTH =~ F1 + F2 + F3
                CMB =~ rwa03 + rwa04 + rwa05 + rwa06 + rwa07 + rwa08 + rwa09 + rwa10 + rwa11 + rwa12 + 
                        rwa13 + rwa14 + rwa15 + rwa16 + rwa17 + rwa18 + rwa19 + rwa20 + rwa21 + rwa22
              AUTH ~~ 0*CMB
              F1 ~~ 1*F1
              F2 ~~ 1*F2
              F3 ~~ 1*F3
              AUTH ~~ 1*AUTH
              rwa03 ~~ rwa14
              rwa12 ~~ rwa16
                "


cfa_output_3factor_1meth_w2 <- lavaan::cfa(model_rwa20_3factor_1meth_w2, data = data_w2, 
                                           estimator="MLR", std.lv=FALSE)

fitmeasures(cfa_output_3factor_1meth_w2, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                           "rni", "mfi", "gfi")) 
SABIC(cfa_output_3factor_1meth_w2)
AICc(cfa_output_3factor_1meth_w2)
summary(cfa_output_3factor_1meth_w2, standardized = TRUE, fit.measures = TRUE)
semPaths(cfa_output_3factor_1meth_w2, what = "paths", whatLabels = "stand")
modindices(cfa_output_3factor_1meth_w2, minimum.value = 10, sort = TRUE)
inspect(cfa_output_3factor_1meth_w2, "r2")
residuals_wave2_cor <- lavResiduals(cfa_output_3factor_1meth_w2, type = "cor")$cov
print(residuals_wave1_cor)


#extract factor scores for both waves
prediction_20_CMB_w1 <- lavPredict(cfa_output_3factor_1meth_w1, newdata = data_w1) %>% as.data.frame()
prediction_20_CMB_w2 <- lavPredict(cfa_output_3factor_1meth_w2, newdata = data_w2) %>% as.data.frame()

#inspect correlations of factor scores
corr.test(prediction_20_CMB_w1)
corr.test(prediction_20_CMB_w2)

#inspect test-retest reliability of factor scores
cor.test(prediction_20_CMB_w1$F1, prediction_20_CMB_w2$F1)
cor.test(prediction_20_CMB_w1$F2, prediction_20_CMB_w2$F2)
cor.test(prediction_20_CMB_w1$F3, prediction_20_CMB_w2$F3)
cor.test(prediction_20_CMB_w1$AUTH, prediction_20_CMB_w2$AUTH)
cor.test(prediction_20_CMB_w1$CMB, prediction_20_CMB_w2$CMB)


#inspect inter-factor correlations across waves for factor stability testing
cor(prediction_20_CMB_w1$F1, prediction_20_CMB_w2$F1)
cor(prediction_20_CMB_w1$F1, prediction_20_CMB_w2$F2)
cor(prediction_20_CMB_w1$F1, prediction_20_CMB_w2$F3)

cor(prediction_20_CMB_w1$F2, prediction_20_CMB_w2$F2)
cor(prediction_20_CMB_w1$F2, prediction_20_CMB_w2$F1)
cor(prediction_20_CMB_w1$F2, prediction_20_CMB_w2$F3)

cor(prediction_20_CMB_w1$F3, prediction_20_CMB_w2$F3)
cor(prediction_20_CMB_w1$F3, prediction_20_CMB_w2$F2)
cor(prediction_20_CMB_w1$F3, prediction_20_CMB_w2$F1)

cor(prediction_20_CMB_w2$F1, prediction_20_CMB_w1$F1)
cor(prediction_20_CMB_w2$F1, prediction_20_CMB_w1$F2)
cor(prediction_20_CMB_w2$F1, prediction_20_CMB_w1$F3)

cor(prediction_20_CMB_w2$F2, prediction_20_CMB_w1$F2)
cor(prediction_20_CMB_w2$F2, prediction_20_CMB_w1$F1)
cor(prediction_20_CMB_w2$F2, prediction_20_CMB_w1$F2)

cor(prediction_20_CMB_w2$F3, prediction_20_CMB_w1$F3)
cor(prediction_20_CMB_w2$F3, prediction_20_CMB_w1$F2)
cor(prediction_20_CMB_w2$F3, prediction_20_CMB_w1$F1)


#compare CMB score with the original CMB score
cor.test(prediction_20_CMB_w1$CMB, sub_auth_data$rwa_CMB_score_w1)
cor.test(prediction_20_CMB_w2$CMB, sub_auth_data$rwa_CMB_score_w2)
cor.test(sub_auth_data$rwa_CMB_score_w1, sub_auth_data$rwa_CMB_score_w2)



#abridged version of RWA (RWA13)

#correlate RWA13 scale score with RWA20 scale score
#raw data
cor.test(rowMeans(data_w1[all_RWA13_items]), rowMeans(data_w1[all_items_rwa20]))
cor.test(rowMeans(data_w2[all_RWA13_items]), rowMeans(data_w2[all_items_rwa20]))

#ARS-corrected data
cor.test(rowMeans(data_deARS_w1[all_RWA13_items]), rowMeans(data_deARS_w1[all_items_rwa20]))
cor.test(rowMeans(data_deARS_w2[all_RWA13_items]), rowMeans(data_deARS_w2[all_items_rwa20]))

#CMB-corrected data
cor.test(rowMeans(data_deCMB_w1[all_RWA13_items]), rowMeans(data_deCMB_w1[all_items_rwa20]))
cor.test(rowMeans(data_deCMB_w2[all_RWA13_items]), rowMeans(data_deCMB_w2[all_items_rwa20]))



##3-factor model that includes items which consistently load on factors across waves

model_rwa13_3factor <- "
                AGGR =~ rwa07 + rwa03 + rwa05 + rwa16 + rwa18 +
                            #cross-loadings
                            rwa12 + rwa21
                CONV =~ rwa10 + rwa11 + rwa21 +
                            #cross-loadings
                            rwa09 + rwa12 + rwa13
                SUBM =~ rwa15 + rwa13 + 
                            #cross-loadings
                            rwa09 + rwa19
                
                 
                AUTH =~ AGGR + CONV + SUBM

                AUTH ~~ 1*AUTH
                AGGR ~~ 1*AGGR
                CONV ~~ 1*CONV
                SUBM ~~ 1*SUBM
"

#testing the model on CMB-corrected data
cfa_output_13_3f_w1 <- lavaan::cfa(model_rwa13_3factor, sample.cov = deCMB_shrink_cov_w1, 
                                sample.nobs = nrow(data_deCMB_w1), std.lv=FALSE)

cfa_output_13_3f_w2 <- lavaan::cfa(model_rwa13_3factor, sample.cov = deCMB_shrink_cov_w2, 
                                sample.nobs = nrow(data_deCMB_w2), std.lv=FALSE)

fitmeasures(cfa_output_13_3f_w1, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                "rni", "mfi", "gfi")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()
fitmeasures(cfa_output_13_3f_w2, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                "rni", "mfi", "gfi")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()

SABIC(cfa_output_13_3f_w1)
SABIC(cfa_output_13_3f_w2)

AICc(cfa_output_13_3f_w1)
AICc(cfa_output_13_3f_w2)

summary(cfa_output_13_3f_w1, standardized = TRUE, fit.measures = TRUE)
summary(cfa_output_13_3f_w2, standardized = TRUE, fit.measures = TRUE)



#1-factor model for RWA13
model_rwa13_1factor <- "
                AUTH =~ rwa03 + rwa05 + rwa07 + rwa09 + rwa10 + rwa11 + rwa12 + rwa13 +
                rwa15 + rwa16 + rwa18 + rwa19 + rwa21
"

#testing the model on CMB-corrected data
#wave 1
cfa_output_13_1f_w1 <- lavaan::cfa(model_rwa13_1factor, sample.cov = deCMB_shrink_cov_w1, 
                                sample.nobs = nrow(data_deCMB_w1), std.lv=FALSE)

#wave 2
cfa_output_13_1f_w2 <- lavaan::cfa(model_rwa13_1factor, sample.cov = deCMB_shrink_cov_w2, 
                                sample.nobs = nrow(data_deCMB_w2), std.lv=FALSE)

fitmeasures(cfa_output_13_1f_w1, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                "rni", "mfi", "gfi")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()
fitmeasures(cfa_output_13_1f_w2, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                "rni", "mfi", "gfi")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()

SABIC(cfa_output_13_1f_w1)
SABIC(cfa_output_13_1f_w2)

AICc(cfa_output_13_1f_w1)
AICc(cfa_output_13_1f_w2)

summary(cfa_output_13_1f_w1, standardized = TRUE, fit.measures = TRUE)
summary(cfa_output_13_1f_w2, standardized = TRUE, fit.measures = TRUE)


#2-factor model for RWA13
model_rwa13_2factor <- "
                AGGR =~ rwa03 + rwa05 + rwa07 + rwa16 + rwa18 + 
                            #cross-loadings
                            rwa11 + rwa10 + rwa12 + rwa13 + rwa19 + rwa21
                SUBM =~ rwa09 + rwa15 + 
                            #cross-loadings:
                            rwa11 + rwa10 + rwa12 + rwa13 + rwa19 + rwa21
                AUTH =~ AGGR + SUBM
                AUTH ~~ 1*AUTH
                AGGR ~~ 1*AGGR
                SUBM ~~ 1*SUBM
                "

#testing the model on CMB-corrected data
#wave 1
cfa_output_13_2f_w1 <- lavaan::cfa(model_rwa13_2factor, sample.cov = deCMB_shrink_cov_w1, 
                                   sample.nobs = nrow(data_deCMB_w1), std.lv=FALSE)

#wave 2
cfa_output_13_2f_w2 <- lavaan::cfa(model_rwa13_2factor, sample.cov = deCMB_shrink_cov_w2, 
                                   sample.nobs = nrow(data_deCMB_w2), std.lv=FALSE)

fitmeasures(cfa_output_13_2f_w1, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                   "rni", "mfi", "gfi")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()
fitmeasures(cfa_output_13_2f_w2, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                   "rni", "mfi", "gfi")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()

AICc(cfa_output_13_2f_w1)
AICc(cfa_output_13_2f_w2)

SABIC(cfa_output_13_2f_w1)
SABIC(cfa_output_13_2f_w2)

summary(cfa_output_13_2f_w1, standardized = TRUE, fit.measures = TRUE)
summary(cfa_output_13_2f_w2, standardized = TRUE, fit.measures = TRUE)



#testing longitudinal measurement invariance of RWA13 models on CMB-corrected shrinkage-optimized cov matrices
# List of covariance matrices
cov_list <- list(
  wave1 = deCMB_shrink_cov_w1,
  wave2 = deCMB_shrink_cov_w2
)

# Sample sizes for each wave
n_list <- c(wave1 = nrow(data_deCMB_w1), 
            wave2 = nrow(data_deCMB_w2))  

# Compute mean vectors
mean_list <- list(
  wave1 = colMeans(data_deCMB_w1),
  wave2 = colMeans(data_deCMB_w2)
)


# Define the measurement model
model <- model_rwa13_1factor
model <- model_rwa13_2factor
model <- model_rwa13_3factor

# Fit multi-group CFA for configural invariance
configural <- lavaan::cfa(model, 
                          sample.cov = cov_list, 
                          sample.nobs = n_list, 
                          sample.mean = mean_list, 
                          group = "wave", 
                          meanstructure = TRUE
)

# Check model summary
summary(configural, fit.measures = TRUE, standardized = TRUE)
fitmeasures(configural, c("chisq", "df", "pvalue", "srmr","rmsea", "rmsea.ci.lower", "rmsea.ci.upper","tli", "cfi", "gfi", "npar")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()


# Fit the metric invariance model
metric <- lavaan::cfa(model, 
                          sample.cov = cov_list, 
                          sample.nobs = n_list, 
                          sample.mean = mean_list, 
                          group = "wave", 
                          meanstructure = TRUE,
                          group.equal = "loadings"
                      )

# Check model summary
summary(metric, fit.measures = TRUE, standardized = TRUE)
fitmeasures(metric, c("chisq", "df", "pvalue", "srmr","rmsea", "rmsea.ci.lower", "rmsea.ci.upper","tli", "cfi", "gfi", "npar")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()

# Fit the scalar invariance model
scalar <- lavaan::cfa(model, 
                          sample.cov = cov_list, 
                          sample.nobs = n_list, 
                          sample.mean = mean_list, 
                          group = "wave", 
                          meanstructure = TRUE,
                          group.equal = c("loadings", "intercepts"))  # Constrain factor loadings across waves

# Check model summary
summary(scalar, fit.measures = TRUE, standardized = TRUE)
fitmeasures(scalar, c("chisq", "df", "pvalue", "srmr","rmsea", "rmsea.ci.lower", "rmsea.ci.upper","tli", "cfi", "gfi", "npar")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()

AICc(configural)
AICc(metric)
AICc(scalar)
AICc(metric) - AICc(configural)
AICc(scalar) - AICc(metric)


SABIC(configural)
SABIC(metric)
SABIC(scalar)
SABIC(metric) - SABIC(configural)
SABIC(scalar) - SABIC(metric)

#compare model fits across MI levels
fit_comparison <- compareFit(configural, metric, scalar)
summary(fit_comparison)




#testing abridged version of RWA (RWA13) on the raw data by modelling CMB factor

#1-factor bifactor model with CMB factor
model_rwa_1factor_RWA13_methVar <- "
                AUTH =~ rwa03 + rwa05 + rwa07 + rwa09 + rwa10 + rwa11 + rwa12 + rwa13 + 
                        rwa15 + rwa16 + rwa18 + rwa19 + rwa21 #+ rwa22

                CMB =~ rwa03 + rwa05 + rwa07 + rwa09 + rwa10 + rwa11 + rwa12 + rwa13 + 
                        rwa15 + rwa16 + rwa18 + rwa19 + rwa21 #+ rwa22
                
                CMB ~~ CMB_score
                AUTH ~~ 0*CMB
                AUTH ~~ 1*AUTH
#                rwa03 ~~ rwa18
"

#appending CMB score to wave-specific datasets
data_w1_meth <- data_w1 %>% mutate(CMB_score = sub_auth_data$rwa_CMB_score_w1)
data_w2_meth <- data_w2 %>% mutate(CMB_score = sub_auth_data$rwa_CMB_score_w2)

cfa_output_13_w1_CMB_1f <- lavaan::cfa(model_rwa_1factor_RWA13_methVar, data_w1_meth,
                                       estimator="MLR", std.lv=FALSE)
cfa_output_13_w2_CMB_1f <- lavaan::cfa(model_rwa_1factor_RWA13_methVar, data_w2_meth,
                                       estimator="MLR", std.lv=FALSE)

fitmeasures(cfa_output_13_w1_CMB_1f, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                       "rni", "mfi", "gfi")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()
fitmeasures(cfa_output_13_w2_CMB_1f, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                       "rni", "mfi", "gfi")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()

SABIC(cfa_output_13_w1_CMB_1f)
SABIC(cfa_output_13_w2_CMB_1f)

AICc(cfa_output_13_w1_CMB_1f)
AICc(cfa_output_13_w2_CMB_1f)

summary(cfa_output_13_w1_CMB_1f, standardized = TRUE, fit.measures = TRUE)
summary(cfa_output_13_w2_CMB_1f, standardized = TRUE, fit.measures = TRUE)

modindices(cfa_output_13_w1_CMB_1f, minimum.value = 5, sort = TRUE)
modindices(cfa_output_13_w2_CMB_1f, minimum.value = 5, sort = TRUE)
inspect(cfa_output_13_w1_CMB_1f, "r2")
inspect(cfa_output_13_w2_CMB_1f, "r2")
semPaths(cfa_output_13_w1_CMB_1f, what = "paths", whatLabels = "stand")
semPaths(cfa_output_13_w2_CMB_1f, what = "paths", whatLabels = "stand")

lavResiduals(cfa_output_13_w1_CMB_1f, type = "cor")$cov
lavResiduals(cfa_output_13_w2_CMB_1f, type = "cor")$cov


#2-factor bifactor model with CMB factor
model_rwa_2factor_RWA13_methVar <- "
                AGGR =~ rwa03 + rwa05 + rwa07 + rwa16 + rwa18 + 
                            #cross-loadings
                            rwa11 + rwa10 + rwa12 + rwa13 + rwa19 + rwa21
                SUBM =~ rwa09 + rwa15 + 
                            #cross-loadings:
                            rwa11 + rwa10 + rwa12 + rwa13 + rwa19 + rwa21

                CMB =~ rwa03 + rwa05 + rwa07 + rwa09 + rwa10 + rwa11 + rwa12 + rwa13 + 
                            rwa15 + rwa16 + rwa18 + rwa19 + rwa21
                CMB ~~ CMB_score
                AUTH =~ AGGR + SUBM
                AUTH ~~ 0*CMB
                AUTH ~~ 1*AUTH
                AGGR ~~ 1*AGGR
                SUBM ~~ 1*SUBM
#                rwa03 ~~ rwa18
                "

cfa_output_13_CMB_var_2f_w1 <- lavaan::cfa(model_rwa_2factor_RWA13_methVar, data_w1_meth,
                                           estimator="MLR", std.lv=FALSE)
cfa_output_13_CMB_var_2f_w2 <- lavaan::cfa(model_rwa_2factor_RWA13_methVar, data_w2_meth,
                                           estimator="MLR", std.lv=FALSE)

fitmeasures(cfa_output_13_CMB_var_2f_w1, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                           "rni", "mfi", "gfi")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()
fitmeasures(cfa_output_13_CMB_var_2f_w2, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                           "rni", "mfi", "gfi")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()
SABIC(cfa_output_13_CMB_var_2f_w1)
SABIC(cfa_output_13_CMB_var_2f_w2)

AICc(cfa_output_13_CMB_var_2f_w1)
AICc(cfa_output_13_CMB_var_2f_w2)


summary(cfa_output_13_CMB_var_2f_w1, standardized = TRUE, fit.measures = TRUE)
summary(cfa_output_13_CMB_var_2f_w2, standardized = TRUE, fit.measures = TRUE)

modindices(cfa_output_13_CMB_var_2f_w1, minimum.value = 5, sort = TRUE)
modindices(cfa_output_13_CMB_var_2f_w2, minimum.value = 5, sort = TRUE)
inspect(cfa_output_13_CMB_var_2f_w1, "r2")
inspect(cfa_output_13_CMB_var_2f_w2, "r2")
semPaths(cfa_output_13_CMB_var_2f_w1, what = "paths", whatLabels = "stand")
semPaths(cfa_output_13_CMB_var_2f_w2, what = "paths", whatLabels = "stand")

lavResiduals(cfa_output_13_CMB_var_2f_w1, type = "cor")$cov
lavResiduals(cfa_output_13_CMB_var_2f_w2, type = "cor")$cov


#3-factor bifactor model with CMB factor
model_rwa_3factor_RWA13_methVar <- "
                AGGR =~ rwa07 + rwa03 + rwa05 + rwa16 + rwa18 + rwa12 + rwa21
                CONV =~ rwa10 + rwa11 + rwa21 + rwa12 + rwa13
                SUBM =~ rwa15 + rwa13 + rwa09 + rwa19
                CMB =~ rwa03 + rwa05 + rwa07 + rwa09 + rwa10 + rwa11 + rwa12 + rwa13 + 
                        rwa15 + rwa16 + rwa18 + rwa19 + rwa21
                CMB ~~ CMB_score
                AUTH =~ AGGR + CONV + SUBM
                AUTH ~~ 0*CMB
                AUTH ~~ 1*AUTH
                AGGR ~~ 1*AGGR
                CONV ~~ 1*CONV
                SUBM ~~ 1*SUBM

#                rwa03 ~~ rwa18
#                rwa16 ~~ rwa12
                "
cfa_output_13_CMB_var_3f_w1 <- lavaan::cfa(model_rwa_3factor_RWA13_methVar, data_w1_meth,
                                           estimator="MLR", std.lv=FALSE)
cfa_output_13_CMB_var_3f_w2 <- lavaan::cfa(model_rwa_3factor_RWA13_methVar, data_w2_meth,
                                           estimator="MLR", std.lv=FALSE)

fitmeasures(cfa_output_13_CMB_var_3f_w1, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                           "rni", "mfi", "gfi")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()
fitmeasures(cfa_output_13_CMB_var_3f_w2, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                           "rni", "mfi", "gfi")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()
SABIC(cfa_output_13_CMB_var_3f_w1)
SABIC(cfa_output_13_CMB_var_3f_w2)

AICc(cfa_output_13_CMB_var_3f_w1)
AICc(cfa_output_13_CMB_var_3f_w2)

summary(cfa_output_13_CMB_var_3f_w1, standardized = TRUE, fit.measures = TRUE)
summary(cfa_output_13_CMB_var_3f_w2, standardized = TRUE, fit.measures = TRUE)

modindices(cfa_output_13_CMB_var_3f_w1, minimum.value = 5, sort = TRUE)
modindices(cfa_output_13_CMB_var_3f_w2, minimum.value = 5, sort = TRUE)
inspect(cfa_output_13_CMB_var_3f_w1, "r2")
inspect(cfa_output_13_CMB_var_3f_w2, "r2")
semPaths(cfa_output_13_CMB_var_3f_w1, what = "paths", whatLabels = "stand")
semPaths(cfa_output_13_CMB_var_3f_w2, what = "paths", whatLabels = "stand")

lavResiduals(cfa_output_13_CMB_var_3f_w1, type = "cor")$cov
lavResiduals(cfa_output_13_CMB_var_3f_w2, type = "cor")$cov



#inspecting subdomains
# extracting factor scores
prediction_13_CMB_w1 <- lavPredict(cfa_output_13_CMB_var_3f_w1, newdata = data_w1_meth) %>% as.data.frame()
prediction_13_CMB_w2 <- lavPredict(cfa_output_13_CMB_var_3f_w2, newdata = data_w2_meth) %>% as.data.frame()

# inspecting inter-factor correlations
corr.test(prediction_13_CMB_w1)
corr.test(prediction_13_CMB_w2)

#inspecting cross-wave stability of factors
cor.test(prediction_13_CMB_w1$AGGR, prediction_13_CMB_w2$AGGR)
cor.test(prediction_13_CMB_w1$CONV, prediction_13_CMB_w2$CONV)
cor.test(prediction_13_CMB_w1$SUBM, prediction_13_CMB_w2$SUBM)
cor.test(prediction_13_CMB_w1$AUTH, prediction_13_CMB_w2$AUTH)
cor.test(prediction_13_CMB_w1$CMB, prediction_13_CMB_w2$CMB)


cor(prediction_13_CMB_w1$AGGR, prediction_13_CMB_w2$AGGR)
cor(prediction_13_CMB_w1$AGGR, prediction_13_CMB_w2$CONV)
cor(prediction_13_CMB_w1$AGGR, prediction_13_CMB_w2$SUBM)

cor(prediction_13_CMB_w1$CONV, prediction_13_CMB_w2$CONV)
cor(prediction_13_CMB_w1$CONV, prediction_13_CMB_w2$AGGR)
cor(prediction_13_CMB_w1$CONV, prediction_13_CMB_w2$SUBM)

cor(prediction_13_CMB_w1$SUBM, prediction_13_CMB_w2$SUBM)
cor(prediction_13_CMB_w1$SUBM, prediction_13_CMB_w2$CONV)
cor(prediction_13_CMB_w1$SUBM, prediction_13_CMB_w2$AGGR)

cor(prediction_13_CMB_w2$AGGR, prediction_13_CMB_w1$AGGR)
cor(prediction_13_CMB_w2$AGGR, prediction_13_CMB_w1$CONV)
cor(prediction_13_CMB_w2$AGGR, prediction_13_CMB_w1$SUBM)

cor(prediction_13_CMB_w2$CONV, prediction_13_CMB_w1$CONV)
cor(prediction_13_CMB_w2$CONV, prediction_13_CMB_w1$AGGR)
cor(prediction_13_CMB_w2$CONV, prediction_13_CMB_w1$SUBM)

cor(prediction_13_CMB_w2$SUBM, prediction_13_CMB_w1$SUBM)
cor(prediction_13_CMB_w2$SUBM, prediction_13_CMB_w1$CONV)
cor(prediction_13_CMB_w2$SUBM, prediction_13_CMB_w1$AGGR)

#relationships between subdomains within waves
cor(prediction_13_CMB_w1$AGGR, prediction_13_CMB_w1$CONV)
cor(prediction_13_CMB_w2$AGGR, prediction_13_CMB_w2$CONV)

cor(prediction_13_CMB_w1$AGGR, prediction_13_CMB_w1$SUBM)
cor(prediction_13_CMB_w2$AGGR, prediction_13_CMB_w2$SUBM)

cor(prediction_13_CMB_w1$CONV, prediction_13_CMB_w1$SUBM)
cor(prediction_13_CMB_w2$CONV, prediction_13_CMB_w2$SUBM)


#comparing model-extracted bias with the original CMB bias
cor.test(prediction_13_CMB_w1$CMB, sub_auth_data$rwa_CMB_score_w1)
cor.test(prediction_13_CMB_w2$CMB, sub_auth_data$rwa_CMB_score_w2)
cor.test(sub_auth_data$rwa_CMB_score_w2, sub_auth_data$rwa_CMB_score_w2)


# correlate factors with corresponding factors in RWA20
cor.test(prediction_13_CMB_w1$AGGR, prediction_20_CMB_w1$F1)
cor.test(prediction_13_CMB_w1$CONV, prediction_20_CMB_w1$F2)
cor.test(prediction_13_CMB_w1$SUBM, prediction_20_CMB_w1$F3)
cor.test(prediction_13_CMB_w1$AUTH, prediction_20_CMB_w1$AUTH)
cor.test(prediction_13_CMB_w1$CMB, prediction_20_CMB_w1$CMB)




#testing psychometric properties of RWA13
# create vectors of RWA13 item names
all_RWA13_items <- 
  c("rwa03", "rwa05", "rwa07", "rwa09", "rwa10", "rwa11", "rwa12", "rwa13", "rwa15", "rwa16", "rwa18", "rwa19", "rwa21")

all_RWA13_items_w1 <- 
  c("rwa03_w1", "rwa05_w1", "rwa07_w1", "rwa09_w1", "rwa10_w1", "rwa11_w1", 
    "rwa12_w1", "rwa13_w1", "rwa15_w1", "rwa16_w1", "rwa18_w1", "rwa19_w1", "rwa21_w1")

#calculate scale scores for RWA13 (raw data)
RWA13_score_w1 <- rowMeans(data_w1[all_RWA13_items])
RWA13_score_w2 <- rowMeans(data_w2[all_RWA13_items])

# correlate scale scores of RWA13 across waves
cor.test(RWA13_score_w1, RWA13_score_w2)

# compute alpha-relialibity of RWA13
# wave 1
alpha_output <- psych::alpha(data_w1[all_RWA13_items])
#write.table(alpha_output$item.stats %>% round(digits = 2), "RWA13_alpha_RAW_w1.csv", sep = ";")

# wave 2
alpha_output <- psych::alpha(data_w2[all_RWA13_items])
write.table(alpha_output$item.stats %>% round(digits = 2), "RWA13_alpha_RAW_w2.csv", sep = ";")

# calculate omega-reliability of RWA13 on both waves
psych::omega(data_w1[all_RWA13_items], nfactors = 1)
psych::omega(data_w2[all_RWA13_items], nfactors = 1)

# calculate alpha and omega reliability of RWA13 on ARS-corrected data
psych::alpha(data_deARS_w1[all_RWA13_items])
psych::alpha(data_deARS_w2[all_RWA13_items])
psych::omega(data_deARS_w1[all_RWA13_items], nfactors = 1)
psych::omega(data_deARS_w2[all_RWA13_items], nfactors = 1)

# calculate alpha and omega reliability of RWA13 on CMB-corrected data
alpha_output <- psych::alpha(data_deCMB_w1[all_RWA13_items])
#write.table(alpha_output$item.stats %>% round(digits = 2), "RWA13_alpha_deCMB_w1.csv", sep = ";")

alpha_output <- psych::alpha(data_deCMB_w2[all_RWA13_items])
#write.table(alpha_output$item.stats %>% round(digits = 2), "RWA13_alpha_deCMB_w2.csv", sep = ";")

psych::omega(data_deCMB_w1[all_RWA13_items], nfactors = 1)
psych::omega(data_deCMB_w2[all_RWA13_items], nfactors = 1)


# calculate test-retest reliability of RWA13 (ICC)

# Initialize a dataframe to store ICC values
retest_reliability <- matrix(ncol = 4, nrow = 13)  %>% as.data.frame()
colnames(retest_reliability) <- c("item", "raw", "de_acquiescence", "de_cmb")

# Loop through each item and calculate ICC for test-retest reliability
i <- 1
for (item in all_RWA13_items_w1) {
  retest_reliability$item[i] <- item
  
  # Get corresponding wave 2 item name
  item_w2 <- gsub("w1", "w2", item)
  
  # Calculate ICC for raw items
  rwa_raw <- sub_auth_data[, c(item, item_w2)]
  retest_reliability$raw[i] <- icc(rwa_raw, model = "twoway", type = "agreement", unit = "single")$value
  
  # Calculate ICC for ARS-corrected (de-acquiescence) items
  item_residual_w1 <- paste0(item, "_residual")
  item_residual_w2 <- gsub("w1", "w2", item_residual_w1)
  rwa_de_acquiescence <- sub_auth_data[, c(item_residual_w1, item_residual_w2)]
  retest_reliability$de_acquiescence[i] <- icc(rwa_de_acquiescence, model = "twoway", type = "agreement", unit = "single")$value
  
  # Calculate ICC for de-CMB (common method bias corrected) items
  item_decmb_w1 <- paste0(item, "_decmb")
  item_decmb_w2 <- gsub("w1", "w2", item_decmb_w1)
  rwa_de_cmb <- sub_auth_data[, c(item_decmb_w1, item_decmb_w2)]
  retest_reliability$de_cmb[i] <- icc(rwa_de_cmb, model = "twoway", type = "agreement", unit = "single")$value
  
  i <- i + 1
}

# Clean item names for readability
retest_reliability$item <- gsub("_w1", "", retest_reliability$item)

# Round values to 2 decimal places
retest_reliability[,2:4] <- round(retest_reliability[,2:4], digits = 2)

# View final ICC-based retest reliability table
retest_reliability

#write.table(retest_reliability, "RWA13_retest_reliability_ICC.csv", row.names = FALSE, sep=";")





# testing longitudinal measurement invariance for 1-, 2-, and 3-factor bifactor models with CMB factor

#add wave identifiers to the datasets
data_w1_meth$wave <- 1
data_w2_meth$wave <- 2

sub_auth_data_long <- rbind(data_w1_meth, data_w2_meth)


# Define the measurement model
model <- model_rwa_1factor_RWA13_methVar
model <- model_rwa_2factor_RWA13_methVar
model <- model_rwa_3factor_RWA13_methVar

# Configural invariance
      configural <- lavaan::cfa(model, data = sub_auth_data_long, group = "wave")
      fitmeasures(configural, c("chisq", "df", "pvalue", "srmr","rmsea", "rmsea.ci.lower", "rmsea.ci.upper","tli", "cfi", "gfi", "npar")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()
      summary(configural, fit.measures = TRUE, standardized = TRUE)

# Metric invariance (partial)
      metric <- lavaan::cfa(model, data = sub_auth_data_long, group = "wave", group.equal = "loadings",
                      group.partial = c("CMB=~rwa05", "CMB=~rwa18",
                                        "CMB=~rwa11", "CMB=~rwa10",
                                        "CMB=~rwa21", "CMB=~rwa15",
                                        "CMB=~rwa13", "CMB=~rwa09",
                                        "CMB=~rwa07", "CMB=~rwa12",
                                        "CMB=~rwa19", "CMB=~rwa16",
                                        "CMB=~rwa03", "CMB=~rwa22"
                      ))
fitmeasures(metric, c("chisq", "df", "pvalue", "srmr","rmsea", "rmsea.ci.lower", "rmsea.ci.upper","tli", "cfi", "gfi", "npar")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()

summary(metric, fit.measures = TRUE, standardized = TRUE)


# Scalar invariance (partial)
      scalar <- lavaan::cfa(model, data = sub_auth_data_long,
                      group = "wave", 
                      group.equal = c("loadings", "intercepts"),
                      group.partial = c("CMB=~rwa05", "CMB=~rwa18",
                                        "CMB=~rwa11", "CMB=~rwa10",
                                        "CMB=~rwa21", "CMB=~rwa15",
                                        "CMB=~rwa13", "CMB=~rwa09",
                                        "CMB=~rwa07", "CMB=~rwa12",
                                        "CMB=~rwa19", "CMB=~rwa16",
                                        "CMB=~rwa03", "CMB=~rwa22"
                      ))
fitmeasures(scalar, c("chisq", "df", "pvalue", "srmr","rmsea", "rmsea.ci.lower", "rmsea.ci.upper","tli", "cfi", "gfi", "npar")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()
summary(scalar, fit.measures = TRUE, standardized = TRUE)


# compare model fit across MI levels
fit_comparison <- compareFit(configural, metric, scalar)
summary(fit_comparison)

SABIC(configural)
SABIC(metric)
SABIC(scalar)
SABIC(metric) - SABIC(configural)
SABIC(scalar) - SABIC(metric)

AICc(configural)
AICc(metric)
AICc(scalar)
AICc(metric) - AICc(configural)
AICc(scalar) - AICc(metric)




# Testing abridged 6-item version of RWA
# create vectors with item names of RWA6 across data types
rwa6_w1 <- c("rwa04_w1", "rwa10_w1", "rwa12_w1", "rwa13_w1", "rwa16_w1", "rwa21_w1")
rwa6_w2 <- c("rwa04_w2", "rwa10_w2", "rwa12_w2", "rwa13_w2", "rwa16_w2", "rwa21_w2")
rwa6 <- c("rwa04", "rwa10", "rwa12", "rwa13", "rwa16", "rwa21")


rwa6_deARS_w1 <- c("rwa04_w1_residual", "rwa10_w1_residual", 
                   "rwa12_w1_residual", "rwa13_w1_residual", "rwa16_w1_residual", "rwa21_w1_residual")
rwa6_deARS_w2 <- c("rwa04_w_residual","rwa10_w2_residual",
                   "rwa12_w2_residual", "rwa13_w2_residual", "rwa16_w2_residual", "rwa21_w2_residual")
rwa6_deCMB_w1 <- c("rwa04_w1_decmb", "rwa10_w1_decmb", 
                   "rwa12_w1_decmb", "rwa13_w1_decmb", "rwa16_w1_decmb", "rwa21_w1_decmb")
rwa6_deCMB_w2 <- c("rwa04_w2_decmb", "rwa10_w2_decmb",
                   "rwa12_w2_decmb", "rwa13_w2_decmb", "rwa16_w2_decmb", "rwa21_w2_decmb")

# correlate RWA6 scale scores with RWA13 scales scores: raw data
cor.test(rowMeans(data_w1[all_RWA13_items]), rowMeans(data_w1[rwa6]))
cor.test(rowMeans(data_w2[all_RWA13_items]), rowMeans(data_w2[rwa6]))

# correlate RWA6 scale scores with RWA13 scales scores: ARS-corrected data
cor.test(rowMeans(data_deARS_w1[all_RWA13_items]), rowMeans(data_deARS_w1[rwa6]))
cor.test(rowMeans(data_deARS_w2[all_RWA13_items]), rowMeans(data_deARS_w2[rwa6]))

# correlate RWA6 scale scores with RWA13 scales scores: CMB-corrected data
cor.test(rowMeans(data_deCMB_w1[all_RWA13_items]), rowMeans(data_deCMB_w1[rwa6]))
cor.test(rowMeans(data_deCMB_w2[all_RWA13_items]), rowMeans(data_deCMB_w2[rwa6]))



# test 1-factor model of RWA6 
model_rwa6_1factor <- "
      AUTH =~ NA*rwa04  + rwa10 + rwa12 + 
              rwa16 + rwa13 + rwa21
      "

cfa_output_RWA6_w1 <- lavaan::cfa(model_rwa6_1factor, 
                                           data_w1, std.lv=TRUE, estimator="MLR")
cfa_output_RWA6_w2 <- lavaan::cfa(model_rwa6_1factor, 
                                           data_w2, std.lv=TRUE, estimator="MLR")

fitmeasures(cfa_output_RWA6_w1, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                           "rni", "mfi", "gfi")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()

fitmeasures(cfa_output_RWA6_w2, c("chisq", "df", "pvalue", "srmr","rmsea", "tli", "cfi", "nnfi", "ifi", 
                                           "rni", "mfi", "gfi")) #%>% as.data.frame() %>% round(digits = 3) %>% as.vector()
summary(cfa_output_RWA6_w1, standardized = TRUE, fit.measures = TRUE)
summary(cfa_output_RWA6_w2, standardized = TRUE, fit.measures = TRUE)
modindices(cfa_output_RWA6_w1, minimum.value = 5, sort = TRUE)
modindices(cfa_output_RWA6_w2, minimum.value = 5, sort = TRUE)
inspect(cfa_output_RWA6_w1, "r2")
inspect(cfa_output_RWA6_w2, "r2")
semPaths(cfa_output_RWA6_w1, what = "paths", whatLabels = "stand")
semPaths(cfa_output_RWA6_w2, what = "paths", whatLabels = "stand")

lavResiduals(cfa_output_RWA6_w1, type = "cor")$cov
lavResiduals(cfa_output_RWA6_w2, type = "cor")$cov


#validating CMB score
#extract factor scores
prediction_6_CMB_w1 <- lavPredict(cfa_output_RWA6_w1, newdata = data_w1) %>% as.data.frame()
prediction_6_CMB_w2 <- lavPredict(cfa_output_RWA6_w2, newdata = data_w2) %>% as.data.frame()

#test interwave correlation of factor scores
cor.test(prediction_6_CMB_w1$AUTH, prediction_6_CMB_w2$AUTH)

#interwave correlation of raw RWA6 scores
cor.test(rowMeans(data_w1[c("rwa04", "rwa10", "rwa12",  
                            "rwa16", "rwa13", "rwa21")]), 
         rowMeans(data_w2[c("rwa04", "rwa10", "rwa12", "rwa16", "rwa13", "rwa21")]))


#correlations of raw RWA6 scores with factor scores
cor.test(prediction_6_CMB_w1$AUTH, rowMeans(data_w1[c("rwa04", "rwa10", "rwa12",  
                                                      "rwa16", "rwa13", "rwa21")]))

cor.test(prediction_6_CMB_w2$AUTH, rowMeans(data_w2[c("rwa04", "rwa10", "rwa12",  
                                                      "rwa16", "rwa13", "rwa21")]))


# correlations of factor scores with RWA20 scores (CMB-corrected data)
cor.test(prediction_6_CMB_w1$AUTH, sub_auth_data$rwa_decmb_score_w1)
cor.test(prediction_6_CMB_w2$AUTH, sub_auth_data$rwa_decmb_score_w2)




#testing longitudinal measurement invariance of RWA6

# Reshape data to long format
data_w1_LMI <- data_w1 %>% dplyr::mutate(wave = 1)
data_w2_LMI <- data_w2 %>% dplyr::mutate(wave = 2)

sub_auth_data_long <- rbind(data_w1_LMI, data_w2_LMI)

# Define the measurement model
model <- model_rwa6_1factor

# Configural invariance
      configural <- lavaan::cfa(model, data = sub_auth_data_long, group = "wave", estimator="MLR", std.lv=TRUE)

# Metric invariance
      metric <- lavaan::cfa(model, data = sub_auth_data_long, group = "wave", 
                      estimator="MLR", std.lv=TRUE,
                      group.equal = "loadings"
                      )

# Scalar invariance: same loadings and intercepts
      scalar <- lavaan::cfa(model, data = sub_auth_data_long,
                      estimator="MLR", std.lv=TRUE,
                      group = "wave", 
                      group.equal = c("loadings", "intercepts")
                      )

#compare fit across MI levels
fit_comparison <- compareFit(configural, metric, scalar)
summary(fit_comparison)

# alpha-reliability of RWA6 (raw data)
psych::alpha(sub_auth_data[rwa6_w1]) %>% { .$item.stats } %>% round(digits=2) #%>% write.table("RWA_abridged6_reliability_raw_w1.csv", sep=";")
psych::alpha(sub_auth_data[rwa6_w2]) %>% { .$item.stats } %>% round(digits=2) #%>% write.table("RWA_abridged6_reliability_raw_w2.csv", sep=";")

# omega-reliability of RWA6 (raw data)
psych::omega(sub_auth_data[rwa6_w1], nfactors = 1)
psych::omega(sub_auth_data[rwa6_w2], nfactors = 1)

# alpha-reliability of RWA6 (ARS-corrected data)
psych::alpha(sub_auth_data[rwa6_deARS_w1])
psych::alpha(sub_auth_data[rwa6_deARS_w2])

# omega-reliability of RWA6 (ARS-corrected data)
psych::omega(sub_auth_data[rwa6_deARS_w1], nfactors = 1)
psych::omega(sub_auth_data[rwa6_deARS_w2], nfactors = 1)

# alpha-reliability of RWA6 (CMB-corrected data)
psych::alpha(sub_auth_data[rwa6_deCMB_w1]) %>% { .$item.stats } %>% round(digits=2) # %>% write.table("RWA_abridged6_reliability_deCMB_w1.csv", sep=";")
psych::alpha(sub_auth_data[rwa6_deCMB_w2]) %>% { .$item.stats } %>% round(digits=2) # %>% write.table("RWA_abridged6_reliability_deCMB_w2.csv", sep=";")

# omega-reliability of RWA6 (CMB-corrected data)
psych::omega(sub_auth_data[rwa6_deCMB_w1], nfactors = 1)
psych::omega(sub_auth_data[rwa6_deCMB_w2], nfactors = 1)


# test-retest reliability of RWA6 scale score (raw data)
rwa6_scale_score_w1 <- rowMeans(data_w1[rwa6])
rwa6_scale_score_w2 <- rowMeans(data_w2[rwa6])
icc_scale <- icc(cbind(rwa6_scale_score_w1, rwa6_scale_score_w2), model = "twoway", type = "agreement", unit = "single")
print(icc_scale)

#correlation of RWA6 scale scores with RWA20 scale scores (raw data)
cor.test(rwa6_scale_score_w1, sub_auth_data$rwa_score_w1)
cor.test(rwa6_scale_score_w2, sub_auth_data$rwa_score_w2)

# test-retest reliability of RWA6 scale score (ARS-corrected data)
rwa6_scale_score_w1 <- rowMeans(data_deARS_w1[rwa6])
rwa6_scale_score_w2 <- rowMeans(data_deARS_w2[rwa6])
icc_scale <- icc(cbind(rwa6_scale_score_w1, rwa6_scale_score_w2), model = "twoway", type = "agreement", unit = "single")
print(icc_scale)

#correlation of RWA6 scale scores with RWA20 scale scores (ARS-corrected data)
cor.test(rwa6_scale_score_w1, sub_auth_data$rwa_residual_score_w1)
cor.test(rwa6_scale_score_w2, sub_auth_data$rwa_residual_score_w2)

# test-retest reliability of RWA6 scale score (CMB-corrected data)
rwa6_scale_score_w1 <- rowMeans(data_deCMB_w1[rwa6])
rwa6_scale_score_w2 <- rowMeans(data_deCMB_w2[rwa6])
icc_scale <- icc(cbind(rwa6_scale_score_w1, rwa6_scale_score_w2), model = "twoway", type = "agreement", unit = "single")
print(icc_scale)

#correlation of RWA6 scale scores with RWA20 scale scores (CMB-corrected data)
cor.test(rwa6_scale_score_w1, sub_auth_data$rwa_decmb_score_w1)
cor.test(rwa6_scale_score_w2, sub_auth_data$rwa_decmb_score_w2)



#assessing share of CMB variance in the total variance of the raw RWA20 score
mod_cmb_share <- lm(sub_auth_data$rwa_score_w1 ~ sub_auth_data$rwa_CMB_score_w1)
summary(mod_cmb_share)

mod_cmb_share <- lm(sub_auth_data$rwa_score_w2 ~ sub_auth_data$rwa_CMB_score_w2)
summary(mod_cmb_share)


#assessing share of CMB variance in the total variance of the raw RWA13 score
mod_cmb_share <- lm(rowMeans(data_w1[all_RWA13_items]) ~ sub_auth_data$rwa_CMB_score_w1)
summary(mod_cmb_share)

mod_cmb_share <- lm(rowMeans(data_w2[all_RWA13_items]) ~ sub_auth_data$rwa_CMB_score_w2)
summary(mod_cmb_share)

#assessing share of CMB variance in the total variance of the raw RWA6 score
mod_cmb_share <- lm(rowMeans(data_w1[rwa6]) ~ sub_auth_data$rwa_CMB_score_w1)
summary(mod_cmb_share)

mod_cmb_share <- lm(rowMeans(data_w2[rwa6]) ~ sub_auth_data$rwa_CMB_score_w2)
summary(mod_cmb_share)




#pre-processing OAS scale items:
# Define the pro-trait and con-trait items

# create vectors with all, pro-trait and con-trait item names for each wave and data type
# wave 1 raw data:
pro_trait_items_oes_w1 <- c("oes03_w1", "oes06_w1", "oes13_w1", "oes14_w1", "oes16_w1", "oes17_w1",
                            "oes18_w1", "oes19_w1", "oes22_w1")
con_trait_items_oes_w1 <- c("oes01_w1", "oes02_w1", "oes04_w1", "oes07_w1", "oes09_w1", "oes10_w1",
                            "oes20_w1", "oes21_w1", "oes23_w1")
all_items_oes_w1 <- c("oes01_w1", "oes02_w1", "oes03_w1", "oes04_w1", "oes06_w1", "oes07_w1", "oes09_w1",
                      "oes10_w1", "oes13_w1", "oes14_w1", "oes16_w1", "oes17_w1", "oes18_w1", "oes19_w1",
                      "oes20_w1", "oes21_w1", "oes22_w1", "oes23_w1")


# wave 2 raw data"
pro_trait_items_oes_w2 <- c("oes03_w2", "oes06_w2", "oes13_w2", "oes14_w2", "oes16_w2", "oes17_w2",
                            "oes18_w2", "oes19_w2", "oes22_w2")
con_trait_items_oes_w2 <- c("oes01_w2", "oes02_w2", "oes04_w2", "oes07_w2", "oes09_w2", "oes10_w2",
                            "oes20_w2", "oes21_w2", "oes23_w2")
all_items_oes_w2 <- c("oes01_w2", "oes02_w2", "oes03_w2", "oes04_w2", "oes06_w2", "oes07_w2", "oes09_w2",
                      "oes10_w2", "oes13_w2", "oes14_w2", "oes16_w2", "oes17_w2", "oes18_w2", "oes19_w2",
                      "oes20_w2", "oes21_w2", "oes22_w2", "oes23_w2")

# wave 1 ARS-corrected data
pro_trait_items_oes_w1_residual <- c("oes03_w1_residual", "oes06_w1_residual", "oes13_w1_residual", "oes14_w1_residual", "oes16_w1_residual", "oes17_w1_residual",
                                     "oes18_w1_residual", "oes19_w1_residual", "oes22_w1_residual")
con_trait_items_oes_w1_residual <- c("oes01_w1_residual", "oes02_w1_residual", "oes04_w1_residual", "oes07_w1_residual", "oes09_w1_residual", "oes10_w1_residual",
                                     "oes20_w1_residual", "oes21_w1_residual", "oes23_w1_residual")
all_items_oes_w1_residual <- c("oes01_w1_residual", "oes02_w1_residual", "oes03_w1_residual", "oes04_w1_residual", "oes06_w1_residual", "oes07_w1_residual", "oes09_w1_residual",
                               "oes10_w1_residual", "oes13_w1_residual", "oes14_w1_residual", "oes16_w1_residual", "oes17_w1_residual", "oes18_w1_residual", "oes19_w1_residual",
                               "oes20_w1_residual", "oes21_w1_residual", "oes22_w1_residual", "oes23_w1_residual")

# wave 2 ARS-corrected data
pro_trait_items_oes_w2_residual <- c("oes03_w2_residual", "oes06_w2_residual", "oes13_w2_residual", "oes14_w2_residual", "oes16_w2_residual", "oes17_w2_residual",
                                     "oes18_w2_residual", "oes19_w2_residual", "oes22_w2_residual")
con_trait_items_oes_w2_residual <- c("oes01_w2_residual", "oes02_w2_residual", "oes04_w2_residual", "oes07_w2_residual", "oes09_w2_residual", "oes10_w2_residual",
                                     "oes20_w2_residual", "oes21_w2_residual", "oes23_w2_residual")
all_items_oes_w2_residual <- c("oes01_w2_residual", "oes02_w2_residual", "oes03_w2_residual", "oes04_w2_residual", "oes06_w2_residual", "oes07_w2_residual", "oes09_w2_residual",
                               "oes10_w2_residual", "oes13_w2_residual", "oes14_w2_residual", "oes16_w2_residual", "oes17_w2_residual", "oes18_w2_residual", "oes19_w2_residual",
                               "oes20_w2_residual", "oes21_w2_residual", "oes22_w2_residual", "oes23_w2_residual")




###Calculate ARS bias for wave 1
# Step 1: Reverse con-trait items back to their unrecoded form (before recoding)
sub_auth_data[con_trait_items_oes_w1] <- 6 - sub_auth_data[con_trait_items_oes_w1]

# Step 2: Calculate the acquiescence score as the mean of all items (pro-trait and unrecoded con-trait)
sub_auth_data_z <- scale(sub_auth_data[, c(all_items_oes_w1)])                      # Standardize the items to z-scores
sub_auth_data$oes_acquiescence_score_w1 <- rowMeans(sub_auth_data_z, na.rm = TRUE)  # Acquiescence score

# Step 3: Re-reverse the con-trait items to their recoded form (aligned with pro-trait items)
sub_auth_data[con_trait_items_oes_w1] <- 6 - sub_auth_data[con_trait_items_oes_w1]  # Recoding back to align with pro-trait items

# Step 4: Regress each item on the acquiescence score to obtain residuals (bias-corrected scores)
for (item in c(all_items_oes_w1)) {
  sub_auth_data[[paste0(item, "_residual")]] <- residuals(lm(sub_auth_data[[item]] ~ sub_auth_data$oes_acquiescence_score_w1))
}

#calculate OAS raw score and ARS-corrected score for the wave 1
sub_auth_data$oes_score_w1 <- rowMeans(sub_auth_data[c(all_items_oes_w1)])
sub_auth_data$oes_residual_score_w1 <- sub_auth_data %>% 
  dplyr::select(dplyr::contains("oes") & dplyr::contains("residual") & dplyr::contains("w1")) %>% rowMeans()

# correlate raw OAS scale score with raw RWA20 scale scores
cor.test(sub_auth_data$oes_score_w1, sub_auth_data$rwa_score_w1)

# correlate OAS raw score with OAS ARS score
cor.test(sub_auth_data$oes_score_w1, sub_auth_data$oes_acquiescence_score_w1)

# correlate OAS scale ARS-corrected score with OAS scale raw score
cor.test(sub_auth_data$oes_score_w1, sub_auth_data$oes_residual_score_w1)

# correlate OAS ARS score with RWA20 ARS score
cor.test(sub_auth_data$oes_acquiescence_score_w1, sub_auth_data$rwa_acquiescence_score_w1)

# correlate OAS scale ARS-corrected score with RWA20 ARS-corrected scale score
cor.test(sub_auth_data$oes_residual_score_w1, sub_auth_data$rwa_residual_score_w1)



###Calculate ARS bias for wave 2
# Step 1: Reverse con-trait items back to their unrecoded form (before recoding)
sub_auth_data[con_trait_items_oes_w2] <- 6 - sub_auth_data[con_trait_items_oes_w2]

# Step 2: Calculate the acquiescence score as the mean of all items (pro-trait and unrecoded con-trait)
sub_auth_data_z <- scale(sub_auth_data[, c(all_items_oes_w2)])                      # Standardize the items to z-scores
sub_auth_data$oes_acquiescence_score_w2 <- rowMeans(sub_auth_data_z, na.rm = TRUE)  # Acquiescence score

# Step 3: Re-reverse the con-trait items to their recoded form (aligned with pro-trait items)
sub_auth_data[con_trait_items_oes_w2] <- 6 - sub_auth_data[con_trait_items_oes_w2]  # Recoding back to align with pro-trait items

# Step 4: Regress each item on the acquiescence score to obtain residuals (bias-corrected scores)
for (item in c(all_items_oes_w2)) {
  sub_auth_data[[paste0(item, "_residual")]] <- residuals(lm(sub_auth_data[[item]] ~ sub_auth_data$oes_acquiescence_score_w2))
}

#calculate OAS score and bias-corrected score for the wave 2
sub_auth_data$oes_score_w2 <- rowMeans(sub_auth_data[c(all_items_oes_w2)])
sub_auth_data$oes_residual_score_w2 <- sub_auth_data %>% 
  dplyr::select(dplyr::contains("oes") & dplyr::contains("residual") & dplyr::contains("w2")) %>% rowMeans()

# correlate raw OAS scale score with raw RWA20 scale scores
cor.test(sub_auth_data$oes_score_w2, sub_auth_data$rwa_score_w2)

# correlate OAS raw score with OAS ARS score
cor.test(sub_auth_data$oes_score_w2, sub_auth_data$oes_acquiescence_score_w2)

# correlate OAS scale ARS-corrected score with OAS scale raw score
cor.test(sub_auth_data$oes_score_w2, sub_auth_data$oes_residual_score_w2)

# correlate OAS ARS score with RWA20 ARS score
cor.test(sub_auth_data$oes_acquiescence_score_w2, sub_auth_data$rwa_acquiescence_score_w2)

# correlate OAS scale ARS-corrected score with RWA20 ARS-corrected scale score
cor.test(sub_auth_data$oes_residual_score_w2, sub_auth_data$rwa_residual_score_w2)

# correlate OAS ARS scores across waves
cor.test(sub_auth_data$oes_acquiescence_score_w1, sub_auth_data$oes_acquiescence_score_w2)



