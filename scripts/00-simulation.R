
# Script to Simulate Data and Analysis

#assign column names

cols <- c("othshelp", "givblood", "givhmlss", "cutahead", "volchrty", "loanitem",
          "careself", "peoptrbl", "selffrst", "helpfrds", 
          "helppoor", "helpnot", "helpsick", "helpblk", "trdunion", 
          "goveqinc", "govjobs", "govunemp", "govlazy", "govcare",
          "partyid","polviews")

#create empty dataframe with 1000 rows

n <- 1000
gss_data <- data.frame(matrix(ncol = 0, nrow = n))

# add simulated values to columns
for (i in cols){
  m <- runif(1, min=1, max=7)
  d <- runif(1, 1, 3)
  gss_data[i] <- abs(as.integer(rnorm(n, mean = m, sd = d)))
}
m <- runif(1, min=0, max=1)
gss_data["binary_orientation"] <- abs(as.integer(rnorm(n, mean = m, sd = 0.3)))

# Calculate Means of the variables


Attribute_Name <- vector(mode = "character", length = ncol(gss_data))
Mean <- vector(mode = "character", length = ncol(gss_data))
Standard_Deviation <- vector(mode = "character", length = ncol(gss_data))

count <- 1

for (i in colnames(gss_data)){
  Mean[count] <- signif(mean(as.numeric(unlist(na.omit(gss_data[i])))), 3)
  Standard_Deviation[count] <- signif(sd(as.numeric(unlist(na.omit(gss_data[i])))),3)
  Attribute_Name[count] <- i
  count <- count + 1
}


mean_sd_table <- data.frame(Attribute_Name, Mean, Standard_Deviation)

knitr::kable(mean_sd_table, caption = "Table I: Means and standard Deviations of Selected Variables") %>%
  kable_styling(latex_options="scale_down")



# Distribution of Variables

counts_table <- data.frame(Attribute = character(ncol(gss_data)), b = character(ncol(gss_data)), c = character(ncol(gss_data)),
                           d = character(ncol(gss_data)), e = character(ncol(gss_data)), f = character(ncol(gss_data)),
                           g = character(ncol(gss_data)), h = character(ncol(gss_data)), i = character(ncol(gss_data)))


for (i in seq(1,ncol(gss_data))){
  atr <- colnames(gss_data)[i]
  if (atr != "coninc"){
    cts <- table(gss_data[,i])
    for (j in seq(1,length(cts))){
      counts_table[i, j+1] <- signif(as.numeric(cts[j])/sum(cts), 5)
    }
    counts_table[i, 1] <- atr
  }
}

colnames(counts_table) <- c("AttributeName", "1", "2", "3", "4", "5", "6", "7", "8")

knitr::kable(counts_table, caption = "Table II: Categorical Variable Frequencies") %>%
  kable_styling(latex_options="scale_down")

# Conduct Chisq test with the Altruistic Variables and Polviews

Altruistic_cols <- c("othshelp", "givblood", "givhmlss", "cutahead", "volchrty", "loanitem", "careself", "peoptrbl", "selffrst", "helpfrds")
Chisq_table <- data.frame(matrix(ncol = length(Altruistic_cols) + 1, nrow = 1))
colnames(Chisq_table) <- c("", Altruistic_cols)

Chisq_table[1,1] <- "P-Val"
for (j in seq(1, length(Altruistic_cols))){
  temp_df <- na.omit(dplyr::select(gss_data, "polviews", Altruistic_cols[j]))
  if (nrow(temp_df) > 500){
    pval <- chisq.test(temp_df[,1], temp_df[,2])$p.value
    if (pval < 0.05){
      Chisq_table[1, j+1] <- signif(pval, 2)
    } else {
      Chisq_table[1, j+1] <- "> 0.05"
    }
  }
}

knitr::kable(Chisq_table, caption = "Table IV: P Value of Chi Square Test Comparing Altruistic Variables and Political Orientation") %>%
  kable_styling(latex_options="scale_down")


# Conduct Chisq test with the Political Opinion Variables and Polviews

Pol_cols <- c("helppoor", "helpnot", "helpsick", "helpblk", "trdunion", "goveqinc", "govjobs", "govunemp", "govlazy", "govcare")
Chisq_table <- data.frame(matrix(ncol = length(Pol_cols) + 1, nrow = 1))
colnames(Chisq_table) <- c("", Pol_cols)

Chisq_table[1,1] <- "P-Val"
for (j in seq(1, length(Pol_cols))){
  temp_df <- na.omit(dplyr::select(gss_data, 'polviews', Pol_cols[j]))
  if (nrow(temp_df) > 500){
    pval <- chisq.test(temp_df[,1], temp_df[,2])$p.value
    if (pval < 0.05){
      Chisq_table[1, j+1] <- signif(pval, 2)
    } else {
      Chisq_table[1, j+1] <- "> 0.05"
    }
  } else{
    Chisq_table[1, j+1] <- NA
  }
  
}


knitr::kable(Chisq_table, caption = "Table IV: P Value of Chi Square Test Comparing Political Stance Variables and Political Orientation") %>%
  kable_styling(latex_options="scale_down")


# Logistic Regression Model Altruistism Variables and Polviews

Log_reg_table <- data.frame(matrix(ncol = length(Altruistic_cols) + 1, 1))
colnames(Log_reg_table) <- c("",Altruistic_cols)

Log_reg_table[1,1] <- "Coefficient"
for (j in seq(1, length(Altruistic_cols))){
  temp_df <- na.omit(dplyr::select(gss_data, "polviews", Altruistic_cols[j]))
  colnames(temp_df) <- c("a", "b")
  if (nrow(temp_df) > 500){
    pval <- chisq.test(temp_df[,1], temp_df[,2])$p.value
    if (pval < 0.05){
      temp_model <- polr(as.factor(a) ~ b, data = temp_df)
      Log_reg_table[1, j+1] <- signif(as.numeric(temp_model$coefficients), 3)
    } else {
      Log_reg_table[1, j+1] <- "P > 0.05"
    }
  }
}

knitr::kable(Log_reg_table, caption = "Table V: Coefficient of Logistic Regression of Altruistic Variables and Polviews") %>%
  kable_styling(latex_options="scale_down")

# Logistic Regression Model Political Opinion Variables and Binary Orientation

Log_reg_table <- data.frame(matrix(ncol = 7, nrow = 1))

Pol_cols <- c("helppoor", "helpnot", "helpsick", "helpblk", "trdunion", "goveqinc")

pol_df <- na.omit(dplyr::select(gss_data, "binary_orientation", Pol_cols))
mdl <- glm(binary_orientation ~ helppoor + helpnot + helpsick + helpblk + trdunion + goveqinc, family = binomial(link = "logit"), data = pol_df)

colnames(Log_reg_table) <- c("", Pol_cols)

Log_reg_table[1, 1] <- "P-Val"

for (i in seq(1, length(Pol_cols))){
  Log_reg_table[1, i+1] <- as.numeric(coef(summary(mdl))[,4][Pol_cols[i]])
}

knitr::kable(Log_reg_table, caption = "Table IV: P Value of Logistic Regression Model of Political Stance Variables and Voted Candidate") %>%
  kable_styling(latex_options="scale_down")



