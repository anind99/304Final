
# Script to Simulate Data and Analysis

#assign column names

cols <- c("Urban", "Rural", "Mountain" , "Hill","Terai",  "Eastern" , "Central", "Western" ,"Mid-western" ,"Far.western", "Primary", "Secondary+","No Education", 
          "No antenatal or delivery care" , "Either antenatal or delivery care","Both antenatal and delivery care" )

#create empty dataframe with 1000 rows

n <- 1000
df <- data.frame(matrix(ncol = 0, nrow = n))

# add simulated values to columns
for (i in cols){
  m <- runif(1, min=0, max=1)
  df[i] <- abs(rnorm(n, mean = m, sd = 0.05))
}

# Calculate Means of the variables

mean_selected <- df
MeanMortality <- vector(mode = "character", length = ncol(mean_selected))
SocioEconomicGroup <- vector(mode = "character", length = ncol(mean_selected))
count <- 1
for (i in colnames(mean_selected)){
  MeanMortality[count] <- signif(mean(as.numeric(unlist(na.omit(mean_selected[i])))), 3)
  SocioEconomicGroup[count] <- i
  count <- count + 1
}
mean_table <- data.frame(SocioEconomicGroup, MeanMortality)

knitr::kable(mean_table, caption = "Mean Mortality Rates of Selected Socio-economic Groupings") %>%
  kable_styling(latex_options="scale_down")

# Plot Bar Charts of the Variables

selected_df <- select(df, Urban, Rural, InfantAgeGroup)
df3 <- melt(selected_df, id.vars='InfantAgeGroup')
ggplot(df3, aes(x=InfantAgeGroup, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_x_discrete(guide = guide_axis(n.dodge=3)) + ggtitle("Plot 1: Mortality Rates of children by Residence Type") + guides(fill=guide_legend(title="Residence")) + ylab("Mortality Rate (Death per 1000)") 

selected_df <- select(df, "Primary", "Secondary+", "No Education", "InfantAgeGroup")
df3 <- melt(selected_df, id.vars='InfantAgeGroup')
ggplot(df3, aes(x=InfantAgeGroup, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_x_discrete(guide = guide_axis(n.dodge=3)) + guides(fill=guide_legend(title="Education")) + ylab("Mortality Rate (Death per 1000)") + ggtitle("Plot 2: Mortality Rates of children by Education Level")

selected_df <- select(df,  "Mountain" ,"Hill", "Terai", "InfantAgeGroup")
df3 <- melt(selected_df, id.vars='InfantAgeGroup')
ggplot(df3, aes(x=InfantAgeGroup, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_x_discrete(guide = guide_axis(n.dodge=3)) + guides(fill=guide_legend(title="Ecological Region")) + ylab("Mortality Rate (Death per 1000)") + ggtitle("Plot 3: Mortality Rates of children by Ecologicial Region")

selected_df <- select(df, "Eastern","Central","Western","Mid-western","Far.western","InfantAgeGroup")
df3 <- melt(selected_df, id.vars='InfantAgeGroup')
ggplot(df3, aes(x=InfantAgeGroup, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_x_discrete(guide = guide_axis(n.dodge=3)) + guides(fill=guide_legend(title="Geographical Region")) + ylab("Mortality Rate (Death per 1000)") + ggtitle("Plot 4: Mortality Rates of children by Geographical Region")


# Conduct T test with the Variables

ttest_df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(ttest_df) <- c("X1", "X2", "Type", "P-Value")
# T tests 

ttest1 <- t.test(as.numeric(df$Urban), as.numeric(df$Rural), alternative='less')
ttest_df[1, ] <- c("Urban", "Rural", "Less", signif(ttest1$p.value, 3))

ttest2 <- t.test(as.numeric(df$Mountain), as.numeric(df$Hill), alternative='greater')
ttest_df[2, ] <- c("Mountain", "Hill", "Greater", signif(ttest2$p.value, 3))

ttest3 <- t.test(as.numeric(df$Mountain), as.numeric(df$Terai), alternative='greater')
ttest_df[3, ] <- c("Mountain", "Terai", "Greater", signif(ttest3$p.value, 3))

ttest4 <- t.test(as.numeric(df$Hill), as.numeric(df$Terai), alternative='less')
ttest_df[4, ] <- c("Hill", "Terai", "Less", signif(ttest4$p.value, 3))

ttest5 <- t.test(as.numeric(df$Primary), as.numeric(df$`No Education`), alternative='less')
ttest_df[5, ] <- c("Primary", "No Education", "Less", signif(ttest5$p.value, 3))

ttest6 <- t.test(as.numeric(df$`Secondary+`), as.numeric(df$`No Education`), alternative='less')
ttest_df[6, ] <- c("Secondary +", "No Education", "Less", signif(ttest6$p.value, 3))

ttest7 <- t.test(as.numeric(df$`Secondary+`), as.numeric(df$Primary), alternative='less')
ttest_df[7, ] <- c("Secondary +", "Primary", "Less", signif(ttest7$p.value, 3))

ttest8 <- t.test(as.numeric(df$Eastern), as.numeric(df$Western), alternative='less')
ttest_df[8, ] <- c("Eastern", "Western", "Less", signif(ttest8$p.value, 3))

ttest9 <- t.test(as.numeric(df$Eastern), as.numeric(df$Central), alternative='less')
ttest_df[9, ] <- c("Eastern", "Central", "Less", signif(ttest9$p.value, 3))

ttest10 <- t.test(as.numeric(df$Eastern), as.numeric(df$`Mid-western`), alternative='less')
ttest_df[10, ] <- c("Eastern", "Mid Western", "Less", signif(ttest10$p.value, 3))

ttest11 <- t.test(as.numeric(df$Eastern), as.numeric(df$Far.western), alternative='less')
ttest_df[11, ] <- c("Eastern", "Far Western", "Less", signif(ttest11$p.value, 3))

ttest12 <- t.test(as.numeric(df$Western), as.numeric(df$Central), alternative='less')
ttest_df[12, ] <- c("Western", "Central", "Less", signif(ttest12$p.value, 3))

ttest13 <- t.test(as.numeric(df$Western), as.numeric(df$`Mid-western`), alternative='less')
ttest_df[13, ] <- c("Western", "Mid Western", "Less", signif(ttest13$p.value, 3))

ttest14 <- t.test(as.numeric(df$Western), as.numeric(df$Far.western), alternative='less')
ttest_df[14, ] <- c("Western", "Far Western", "Less", signif(ttest14$p.value, 3))

ttest15 <- t.test(as.numeric(df$Central), as.numeric(df$`Mid-western`), alternative='less')
ttest_df[15, ] <- c("Central", "Mid Western", "Less", signif(ttest15$p.value, 3))

ttest16 <- t.test(as.numeric(df$Central), as.numeric(df$Far.western), alternative='less')
ttest_df[16, ] <- c("Central", "Far Western", "Less", signif(ttest16$p.value, 3))

ttest17 <- t.test(as.numeric(df$`Mid-western`), as.numeric(df$Far.western), alternative='less')
ttest_df[17, ] <- c("Mid Western", "Far Western", "Less", signif(ttest17$p.value, 3))

knitr::kable(ttest_df, caption = "T-Test of Mortality Rates by Socioeconomic Factors") %>%
  kable_styling(latex_options="scale_down")

