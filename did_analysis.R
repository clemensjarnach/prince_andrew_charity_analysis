## ================================================
## Script name: did_analysis.R
## Purpose: Estimate the impact of Prince Andrew’s patronage 
##          withdrawal on UK-registered charities using DiD models
##
## Author: Clemens Jarnach
## Date created: 2025-05-29
## Last modified: 2025-07-01
## ================================================

# This script reproduces the main Difference-in-Differences analysis 
# for the 'Prince Andrew Patronage' project. The dataset includes 
# UK-registered charities for which Prince Andrew was the sole royal 
# patron up to November 2019 (PPA) and non-PPA charities. 
# His public withdrawal affected ~60  charities simultaneously, 
# enabling quasi-experimental estimation of patronage effects.

# ---------------------------------
# Setup
# ---------------------------------

# Check working directory
getwd()  

# Install and load required packages
want <- c("tidyverse", "data.table", "here", "lfe", "readxl", "stargazer", "lubridate", "fixest", "purrr", "mice", "scales", "ggridges" )
have <- want %in% rownames(installed.packages())
if (any(!have)) install.packages(want[!have])
junk <- lapply(want, library, character.only = TRUE)
rm(have, want, junk)


# ---------------------------------
# Load Data
# ---------------------------------

data <- read_csv(file = "did_analysis_data.csv")
df_final <- data
rm(data)

#AND 

df_PAP <- read_csv(file="ppa_list.csv")

# ---------------------------------
# PAP Sample Groups 
# -----------------

# Note: 
# A = Treatment Group (n=35) i.e. all valid PPAs
# B = PPAs which are unis/school
# C = Groups Excluded


table(df_PAP$treatment_index_code)
print(paste("The number of included charities in the Treatment Group is:", nrow(df_PAP)))
print(paste("The number of included charities in the Treatment Group with Index Code A is:", sum(df_PAP$in_analysis_group_A ==1)))
print(paste("The number of included charities in the Treatment Group with Index Code A-B is:", sum(df_PAP$in_analysis_group_A_minus_B ==1)))




# ---------------------------------
# Analysis 
# ---------------------------------


# Difference-in-Differences (DiD) ----------------------------------------------

# Compute averages and standard deviations for 2x2 table
table_means_sd <- df_final %>%
  group_by(treated, post) %>%
  summarize(
    # Calculate the mean of log_income and round it to three decimal places
    mean_log_income = round(mean(log_income), 3),
    # Calculate the standard deviation of log_income and round it to three decimal places
    sd_log_income = round(sd(log_income), 3),
    .groups = "drop"
  ) %>%
  mutate(
    group = paste0(ifelse(treated == 1, "Treated", "Control"), ", ",
                   ifelse(post == 1, "Post", "Pre")),
    # Combine mean and standard deviation into a single string for display
    mean_sd_combined = paste0(mean_log_income, " (", sd_log_income, ")")
  )

# Display the 2x2 table with means and standard deviations
table_2x2_sd <- table_means_sd %>%
  select(group, mean_sd_combined) %>%
  tidyr::spread(key = group, value = mean_sd_combined)

print("Table of Mean log(income) and (Standard Deviation):")
print(table_2x2_sd)


 plot_data <- df_final %>%
  filter(year(yymmdd) >= 2011 & year(yymmdd) <= 2023)
 
# Visualization:
g0 <- ggplot(plot_data,
             aes(
               x = as.factor(post),
               y = log_income,
               color = as.factor(treated),
               group = treated
             )) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean,
               geom = "line",
               linetype = "dashed") +
  labs(
    title = "Difference-in-Differences Visualization",
    x = "Time (0 = Pre, 1 = Post)",
    y = "mean log(income)",
    color = "Group"
  ) +
  scale_color_manual(labels = c("Control", "Treated"),
                     values = c("grey", "red")) +
  theme_classic()
g0

# ggsave("DiD_viz.pdf", g0, width = 4, height = 3, dpi = 300)



## in Steps ------------------------------------
table_data <- plot_data %>%
  group_by(post, treated) %>%
  summarise(
    mean_log_income = mean(log_income, na.rm = TRUE),
    .groups = 'drop'
  )


print(table_data)

ggplot(table_data, aes(x = as.factor(post), y = mean_log_income, color = as.factor(treated), group = treated)) +
  geom_point(size = 3) +
  geom_line(linetype = "solid") +
  labs(
    title = "Average Log Income of PaP and Non-Pap Charities",
    x = "Time (0 = Pre-Event, 1 = Post-Event)",
    y = "Average log(income)",
    color = "Charity Group"
  ) +
  scale_color_manual(labels = c("Control = Non-PAP", "Treated = PAP"),
                     values = c("black", "red")) +
    ylim(9, 13.5) +
  theme_classic()


table_data <- plot_data %>%
  group_by(post, treated) %>%
  summarise(
    mean_log_income = mean(log_income, na.rm = TRUE),
    .groups = 'drop'
  )
table_data

print(table_data)

# Manually provide the counterfactual data: 
table_data <- tibble::tibble(
  post = c(0, 0, 1, 1, 0, 1),
  treated = c(0, 1, 0, 1, 2, 2),
  counterfactual = c(0, 0, 0, 0, 1, 1),
  mean_log_income = c(9.83, 13.0, 10.0, 13.0, 13.0, 13.17)
)

g <- ggplot(table_data, aes(x = as.factor(post), y = mean_log_income, color = as.factor(treated), group = treated, linetype = as.factor(counterfactual))) +
  geom_point(size = 2) +
  geom_line() + # Removed dashed default, now controlled by linetype aesthetic
  labs(
    title = "Average Log Income of Different Charity Groups",
    x = "Time (0 = Pre-Event, 1 = Post-Event)",
    y = "Average log(income)",
    color = "Charity Group",
    linetype = "Counterfactual"
  ) +
  scale_color_manual(
    labels = c("Non-PAP", "PAP", "Counterfactual PAP"),
    values = c("black", "red", "red")
  ) +
  scale_linetype_manual(
    labels = c( "Observed PAP trend", "Counterfactual PAP trend"),
    values = c("solid", "dashed")
  ) +
  ylim(9, 14) +
  theme_classic()

print(g)

# ggsave("plot.svg", plot = g, width = 7, height = 6, units = "in")


# ------------------------------------------------------------------------------

# For the DiD models, it is necessary to restrict the analysis to a defined window of years surrounding the intervention event. Limiting the pre-treatment period to a shorter, more stable baseline helps ensure the plausibility of the parallel trends assumption. Extending the pre-treatment window too far back increases the risk that treated and control groups were already on diverging trajectories due to structural or sector-specific factors.
# Similarly, post-treatment years should be selected based on when an effect from the intervention is most likely to emerge. In this analysis, we therefore focus on the five years preceding the intervention, defined as the discontinuity event in November 2019, when Prince Andrew stepped down from patronage duties, and include all available post-treatment years up to and including 2023.

### Test for parallel trends 
# Estimate means by group and year
df_final %>%
  group_by(treated, year) %>%
  summarise(mean_log_income = mean(log_income, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_log_income, color = as.factor(treated))) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 2019, linetype = "dashed") +
  labs(title = "Parallel Trends Check",
       x = "Year", y = "Mean log(income)",
       color = "Group (0=Control, 1=Treated)") +
    scale_color_manual(labels = c("Control", "Treated"),
                     values = c("grey", "red")) +
  theme_minimal()

# ------------------------------------------------------------------------------

df_final_did <- df_final %>% 
  filter(year(yymmdd) >= 2000) %>%
  mutate(event_time = year - 2019)


# Use -1 as the reference year (baseline)
model_dynamic <- feols(
  log_income ~ i(event_time, treated, ref = -1) | year + Reg_Number,
  data = df_final_did,
  cluster = ~Reg_Number
)

iplot(model_dynamic,
      xlab = "Years relative to treatment",
      main = "Event Study: Parallel Trends and Treatment Effects")



#The event study plot tells the same story. Let's look at a shorter pre-event period. 

df_final_did <- df_final %>% 
  filter(year(yymmdd) >= 2011 & year(yymmdd) <= 2023)%>% 
  mutate(event_time = year - 2019)


# Use -1 as the reference year (baseline)
model_dynamic <- feols(
  log_income ~ i(event_time, treated, ref = -1) | year + Reg_Number,
  data = df_final_did,
  cluster = ~Reg_Number
)

iplot(model_dynamic,
      xlab = "Years relative to treatment",
      main = "Event Study: Parallel Trends and Treatment Effects")

# To hold with the parallel trends assumption we may want to limit the data to:
# Treatment group: Charities that had Prince Andrew as a patron (<2014–2019), i.e. directly affected by his withdrawal.
# Pre-event period: 2011-2019
# Post period: 2020–2023 (after his public withdrawal).
# Control group: A representative sample of all other UK charities.

# ------------------------------------------------------------------------------
## FINAL Paper Version  ------- ------------------------------------------------
# ------------------------------------------------------------------------------

# Restrict the data to 2014-2022 by filtering on the year extracted from yymmdd
# In this analysis, limiting the pre-event period to 2011–2019 and the post-event period to 2020–2022 is a deliberate design choice grounded in both empirical and substantive considerations. First, starting the pre-event period in 2011 allows us to avoid the economic volatility and structural shocks introduced by the 2008 global financial crisis and its immediate aftermath, which could confound baseline trends in charity income. By focusing on the post-crisis stabilisation period, we obtain a more reliable and representative baseline for estimating pre-treatment trends. Extending the pre-period to 2019 maximises statistical power and improves the precision of trend estimates, while our tests suggest the parallel trends assumption holds within this window. For the post-event period, we restrict the window to 2020–2022 to focus specifically on the immediate aftermath of Prince Andrew’s withdrawal from charity patronage. Extending the post-period further risks capturing noise from unrelated developments or long-run institutional adaptation, thereby diluting the causal interpretability of the estimated treatment effect. This balanced design enhances internal validity while preserving temporal relevance.

df_final_did <- df_final %>%
  filter(year(yymmdd) >= 2011 & year(yymmdd) <= 2023)

table(df_final_did$post, useNA = "always") 
table(df_final_did$post, df_final_did$year)


# Compute averages and standard deviations for 2x2 table
table_means_sd <- df_final_did %>%
  group_by(treated, post) %>%
  summarize(
    # Calculate the mean of log_income and round it to three decimal places
    mean_log_income = round(mean(log_income), 3),
    # Calculate the standard deviation of log_income and round it to three decimal places
    sd_log_income = round(sd(log_income), 3),
    .groups = "drop"
  ) %>%
  mutate(
    group = paste0(ifelse(treated == 1, "Treated", "Control"), ", ",
                   ifelse(post == 1, "Post", "Pre")),
    # Combine mean and standard deviation into a single string for display
    mean_sd_combined = paste0(mean_log_income, " (", sd_log_income, ")")
  )

# Display the 2x2 table with means and standard deviations
table_2x2_sd <- table_means_sd %>%
  select(group, mean_sd_combined) %>%
  tidyr::spread(key = group, value = mean_sd_combined)

print("Table of Mean log(income) and (Standard Deviation):")
print(table_2x2_sd)



## REPEAT but with df_final_did_A_minus_B Group ----------------------------
# Step 1: Get the charity ID numbers from df_PAP where treatment_index_code == "B"
reg_charity_number_of_group_B <- df_PAP %>%
  filter(treatment_index_code == "B") %>%
  pull(reg_charity_number)

# Display the charity numbers in group B
print("Charity numbers in Group B:")
print(reg_charity_number_of_group_B)

# Step 2: Filter df_final to exclude those reg_charity_numbers found in group B
df_final_did_A_minus_B <- df_final_did %>%
  filter(!Reg_Number %in% reg_charity_number_of_group_B)

# Display the first few rows of the resulting dataframe
head(df_final_did_A_minus_B)

# You can also check the number of rows to see the subset size
nrow(df_final_did_A_minus_B)

df_final_did_A_minus_B %>%
  filter(treated == 1) %>%
  select(Reg_Number) %>%
  unique()

##  ----------------------------
# Now we have the two main samples: 1) df_final_did and 2) df_final_did_A_minus_B

##  ----------------------------
## This is a list of all the key models. Progressive; in terms of sophistication.

model_did_1 <- lm(log_income~ treated * post,  data = df_final_did)
summary(model_did_1)

model_did_2 <- felm(log_income ~ treated * post |0 | 0 | Reg_Number , data = df_final_did)
summary(model_did_2)

model_did_3 <- lm(log_income ~ treated * post + year, data = df_final_did) # Use df_final_did_subset here
summary(model_did_3)

model_did_4 <-  felm(log_income ~ treated * post | icnpo_group + year, data = df_final_did)
summary(model_did_4)

model_did_5 <- felm(log_income ~ treated * post | Reg_Number + year, data = df_final_did)
summary(model_did_5)

##########################
# List of models and their names

model_list <- list(
  "model_did_1" = "Base DiD",
  "model_did_2" = "DiD + Clustered SEs",
  "model_did_3" = "DiD + Year FE",
  "model_did_4" = "DiD + Year FE + Sector FE",
  "model_did_5" = "DiD + Year FE + Sector FE + Clustered SEs"
)



# Create a list of all key models
models <- list(
  "Base DiD" = model_did_1,
  "DiD + Clustered SEs" = model_did_2,
  "DiD + Year FE" = model_did_3,
  "DiD + Year FE + Sector FE" = model_did_4,
  "DiD + Year FE + Sector FE + Clustered SEs" = model_did_5
  
)

# Custom coefficient names
coefs <- c(
  "treated" = "Treated",
  "post" = "Post",
  "treated:post" = "Treated × Post"
)


extract_model_info <- function(model, model_name) {
  # Get coefficient table safely
  coefs <- tryCatch(
    summary(model)$coefficients,
    error = function(e) return(NULL)
  )
  
  if (is.null(coefs)) {
    return(c(
      Model = model_name,
      Treated = "NA", Post = "NA", `Treated × Post` = "NA"
    ))
  }
  
  # Helper to get term with stars and p-value
  safe_get <- function(term) {
    if (term %in% rownames(coefs)) {
      est <- coefs[term, 1]
      se <- coefs[term, 2]
      pval <- coefs[term, 4]
      
      # Return NA-safe output
      if (is.na(est) | is.na(se) | is.na(pval)) {
        return("NA (NA) [p=NA]")
      }
      
      stars <- if (pval < 0.001) "***"
      else if (pval < 0.01) "**"
      else if (pval < 0.05) "*"
      else if (pval < 0.1) "."
      else ""
      
      return(sprintf("%.3f (%.3f)%s [p=%.3f]", est, se, stars, pval))
    } else {
      return("NA (NA) [p=NA]")
    }
  }
  
  c(
    Model = model_name,
    Treated = safe_get("treated"),
    Post = safe_get("post"),
    `Treated × Post` = safe_get("treated:post")
  )
}

results_table <- do.call(rbind, lapply(names(model_list), function(name) {
  extract_model_info(get(name), model_list[[name]])
}))

## Final Table: 
results_df <- as.data.frame(results_table, stringsAsFactors = FALSE)
print(results_df, row.names = FALSE, right = FALSE)


## Now plot: 

# Define a function to extract results
extract_plot_data <- function(model, model_name) {
  summ <- summary(model)
  coefs <- summ$coefficients
  rownames_coefs <- rownames(coefs)
  
  # Check that "treated:post" exists
  if (!"treated:post" %in% rownames_coefs) {
    return(NULL)
  }
  
  # Extract estimate and clustered SE
  coef_row <- coefs["treated:post", , drop = FALSE]
  estimate <- coef_row[1, "Estimate"]
  
  # Use 'Cluster s.e.' if available, else fallback to standard error
  se_col <- if ("Cluster s.e." %in% colnames(coef_row)) "Cluster s.e." else "Std. Error"
  se <- coef_row[1, se_col]
  
  data.frame(
    model = model_name,
    estimate = estimate,
    conf.low = estimate - 1.96 * se,
    conf.high = estimate + 1.96 * se
  )
}




# Create a list of all key models
model_list <- list(
  "(1) Base DiD" = model_did_1,
  "(2) DiD + Clustered SEs" = model_did_2,
  "(3) DiD + Year FE" = model_did_3,
  "(4) DiD + Year FE + Sector FE" = model_did_4,
  "(5) DiD + Year FE + Sector FE + Clustered SEs" = model_did_5
  
)


# Apply extraction across models
plot_data_list <- lapply(names(model_list), function(name) {
  extract_plot_data(model_list[[name]], name)
})
plot_data_list
# Combine all
plot_data_all <- do.call(rbind, plot_data_list)
plot_data_all

# Specify the desired model order
model_order <- c(
  "(1) Base DiD" ,
  "(2) DiD + Clustered SEs",
  "(3) DiD + Year FE",
  "(4) DiD + Year FE + Sector FE" ,
  "(5) DiD + Year FE + Sector FE + Clustered SEs"
)

# Ensure model factor levels respect the specified order
plot_data_all$model <- factor(plot_data_all$model, levels = rev(model_order))
plot_data_all

plot_data_all_A <- plot_data_all

# Plot
g <- ggplot(plot_data_all, aes(x = model, y = estimate)) +
  geom_point(size = 3, colour = "red") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, colour = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  coord_flip() +
  ylim(-1.5, 1.5) +
  labs(title = "Treatment Effect Estimates from DiD Models",
       x = "Model",
       y = "Estimated Coefficient Income (ln) ± 95% CI") +
  theme_classic(base_size = 13) 
g


#ggsave("DiD_Treatment_Effect_Estimates_Final.pdf", g, width = 8, height = 6, dpi = 300)




##########################
## REPEAT but with df_final_did_A_minus_B Group ----------------------------
##########################


model_did_1_AB <- lm(log_income~ treated * post,  data = df_final_did_A_minus_B)
summary(model_did_1_AB)

model_did_2_AB <- felm(log_income ~ treated * post |0 | 0 | Reg_Number , data = df_final_did_A_minus_B)
summary(model_did_2_AB)

model_did_3_AB <- lm(log_income ~ treated * post + year, data = df_final_did_A_minus_B) # Use df_final_did_subset here
summary(model_did_3_AB)

model_did_4_AB <-  felm(log_income ~ treated * post | icnpo_group + year, data = df_final_did_A_minus_B)
summary(model_did_4_AB)

model_did_5_AB <- felm(log_income ~ treated * post | Reg_Number + year, data = df_final_did_A_minus_B)
summary(model_did_5_AB)


##########################
# List of models and their names

model_list <- list(
  "model_did_1" = "Base DiD",
  "model_did_2" = "DiD + Clustered SEs",
  "model_did_3" = "DiD + Year FE",
  "model_did_4" = "DiD + Year FE + Sector FE",
  "model_did_5" = "DiD + Year FE + Sector FE + Clustered SEs"
)



# Create a list of all key models
models <- list(
  "Base DiD" = model_did_1_AB,
  "DiD + Clustered SEs" = model_did_2_AB,
  "DiD + Year FE" = model_did_3_AB,
  "DiD + Year FE + Sector FE" = model_did_4_AB,
  "DiD + Year FE + Sector FE + Clustered SEs" = model_did_5_AB
  
)

# Custom coefficient names
coefs <- c(
  "treated" = "Treated",
  "post" = "Post",
  "treated:post" = "Treated × Post"
)


extract_model_info <- function(model, model_name) {
  # Get coefficient table safely
  coefs <- tryCatch(
    summary(model)$coefficients,
    error = function(e) return(NULL)
  )
  
  if (is.null(coefs)) {
    return(c(
      Model = model_name,
      Treated = "NA", Post = "NA", `Treated × Post` = "NA"
    ))
  }
  
  # Helper to get term with stars and p-value
  safe_get <- function(term) {
    if (term %in% rownames(coefs)) {
      est <- coefs[term, 1]
      se <- coefs[term, 2]
      pval <- coefs[term, 4]
      
      # Return NA-safe output
      if (is.na(est) | is.na(se) | is.na(pval)) {
        return("NA (NA) [p=NA]")
      }
      
      stars <- if (pval < 0.001) "***"
      else if (pval < 0.01) "**"
      else if (pval < 0.05) "*"
      else if (pval < 0.1) "."
      else ""
      
      return(sprintf("%.3f (%.3f)%s [p=%.3f]", est, se, stars, pval))
    } else {
      return("NA (NA) [p=NA]")
    }
  }
  
  c(
    Model = model_name,
    Treated = safe_get("treated"),
    Post = safe_get("post"),
    `Treated × Post` = safe_get("treated:post")
  )
}

results_table <- do.call(rbind, lapply(names(model_list), function(name) {
  extract_model_info(get(name), model_list[[name]])
}))

## Final Table: 
results_df <- as.data.frame(results_table, stringsAsFactors = FALSE)
print(results_df, row.names = FALSE, right = FALSE)


## Now plot: 

# Define a function to extract results
extract_plot_data <- function(model, model_name) {
  summ <- summary(model)
  coefs <- summ$coefficients
  rownames_coefs <- rownames(coefs)
  
  # Check that "treated:post" exists
  if (!"treated:post" %in% rownames_coefs) {
    return(NULL)
  }
  
  # Extract estimate and clustered SE
  coef_row <- coefs["treated:post", , drop = FALSE]
  estimate <- coef_row[1, "Estimate"]
  
  # Use 'Cluster s.e.' if available, else fallback to standard error
  se_col <- if ("Cluster s.e." %in% colnames(coef_row)) "Cluster s.e." else "Std. Error"
  se <- coef_row[1, se_col]
  
  data.frame(
    model = model_name,
    estimate = estimate,
    conf.low = estimate - 1.96 * se,
    conf.high = estimate + 1.96 * se
  )
}




# Create a list of all key models
model_list <- list(
  "(1) Base DiD" = model_did_1_AB,
  "(2) DiD + Clustered SEs" = model_did_2_AB,
  "(3) DiD + Year FE" = model_did_3_AB,
  "(4) DiD + Year FE + Sector FE" = model_did_4_AB,
  "(5) DiD + Year FE + Sector FE + Clustered SEs" = model_did_5_AB
  
)


# Apply extraction across models
plot_data_list <- lapply(names(model_list), function(name) {
  extract_plot_data(model_list[[name]], name)
})
plot_data_list
# Combine all
plot_data_all <- do.call(rbind, plot_data_list)
plot_data_all

# Specify the desired model order
model_order <- c(
  "(1) Base DiD" ,
  "(2) DiD + Clustered SEs",
  "(3) DiD + Year FE",
  "(4) DiD + Year FE + Sector FE" ,
  "(5) DiD + Year FE + Sector FE + Clustered SEs"
)

# Ensure model factor levels respect the specified order
plot_data_all$model <- factor(plot_data_all$model, levels = rev(model_order))
plot_data_all
plot_data_all_AB <- plot_data_all



# Plot
g <- ggplot(plot_data_all, aes(x = model, y = estimate)) +
  geom_point(size = 3, colour = "blue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, colour = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  coord_flip() +
  ylim(-1.5, 1.5) +
  labs(title = "Treatment Effect Estimates from DiD Models",
       x = "Model",
       y = "Estimated Coefficient Income (ln) ± 95% CI") +
  theme_classic(base_size = 13) 
g





### COMBINE --------
plot_data_all_A
plot_data_all_AB

# Combine data.frames
combined <- bind_rows(
  plot_data_all_A  %>% mutate(group_label = "Group A"),
  plot_data_all_AB %>% mutate(group_label = "Group A-B")
)
combined


# Combined Plot
g_A_AB <- ggplot(combined, aes(x = model, y = estimate, colour = group_label)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2,
                position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_color_manual(values = c("Group A-B" = "blue", "Group A" = "red")) +
  coord_flip() +
  labs(
    title = "Comparison of Treatment Effect Estimates",
    x = "Model",
    y = "Estimated Coefficient ± 95% CI",
    colour = "Data Set",
    shape = "Data Set",
    caption = "Note: 'Group A-B' includes the same charities as 'Group A' but excludes all Group B\ncharities, i.e., PPAs classified as schools or universities."
  ) +
  theme_classic(base_size = 13)+
  theme(    plot.caption = element_text(hjust = 0, size = 9, face = "italic")  # Left-aligned, smaller italic caption
            )

g_A_AB


## Save the plot:

# ggsave("DiD_Treatment_Effect_Estimates_g_A_AB.pdf", g_A_AB, width = 8, height = 6, dpi = 300)




### Lets only Keep models 1,3,4 (i.e. those without clustered SE) -----------
combined_short <- combined %>% 
  slice(c(1, 3, 4, 6, 8, 9)) # Selects rows 1, 3, 4, 5, 8, and 9
combined_short$model

combined_short$model <- combined_short$model %>%
  fct_recode(
    "(2) DiD + Year FE" = "(3) DiD + Year FE",
    "(3) DiD + Year FE + Sector FE" = "(4) DiD + Year FE + Sector FE"
  )

# Now, check the updated levels
levels(combined_short$model)

g_short <- combined_short %>%
  ggplot( aes(x = model, y = estimate, colour = group_label)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2,
                position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_color_manual(values = c("Group A-B" = "blue", "Group A" = "red")) +
  scale_y_continuous(limits = c(-.9, .9)) +
  coord_flip() +
  labs(
    title = "Comparison of Treatment Effect Estimates",
    x = "Model",
    y = "Estimated Coefficient ± 95% CI",
    colour = "Data Set",
    shape = "Data Set",
    caption = "Note: 'Group A-B' includes the same charities as 'Group A' but excludes all \n'Group B' charities, i.e., PPAs classified as schools or universities."
  ) +
  theme_classic(base_size = 13)+
  theme(    plot.caption = element_text(hjust = 0, size = 9, face = "italic")  # Left-aligned, smaller italic caption
  )

g_short

# ggsave(
#   filename = "g_short.svg",
#   plot = g_short,
#   device = "svg",
#   width = 8,      # specify width in inches
#   height = 6,     # specify height in inches
#   dpi = 300       # optional: resolution for rasterized elements
# )
# 


