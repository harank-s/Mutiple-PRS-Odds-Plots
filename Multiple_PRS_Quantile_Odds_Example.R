# load the required packages
if (!require("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!require("ggplot2", quietly = TRUE)) install.packages("dplyr")

library(dplyr)
library(ggplot2)

# Here is the stimulated data for the example
set.seed(2025)
bd_prs1 <- rnorm(400, mean = 0, sd = 1)
bd_prs2 <- rnorm(400, mean = 0.2, sd = 1)
bd_prs <- c(bd_prs1, bd_prs2)

scz_prs1 <- rnorm(400, mean = 0, sd = 1)
scz_prs2 <- rnorm(400, mean = 0.15, sd = 1)
scz_prs <- c(scz_prs1, scz_prs2)

adhd_prs1 <- rnorm(400, mean = 0, sd = 1)
adhd_prs2 <- rnorm(400, mean = 0.3, sd = 1)
adhd_prs <- c(adhd_prs1, adhd_prs2)

mdd_prs1 <- rnorm(400, mean = 0, sd = 1)
mdd_prs2 <- rnorm(400, mean = 0.11, sd = 1)
mdd_prs <- c(mdd_prs1, mdd_prs2)

#Simulate phenotypic data
ace <- c(rep("NO", 400), rep("YES", 400))
sex <- sample(c("Male", "Female"), 800, replace = TRUE)
age <- sample(19:80, 800, replace = TRUE)
smoking <- sample(c("Yes", "No"), 800, replace = TRUE)
pc1 <- rnorm(800, mean = 0.1, sd = 1)
pc2 <- rnorm(800, mean = -0.1, sd = 1)
pc3 <- rnorm(800, mean = 0.15, sd = 1)

Em <- data.frame(ADHD_PRS=adhd_prs, BD_PRS=bd_prs, MDD_PRS=mdd_prs, SCZ_PRS=scz_prs,  ACE=ace, Sex=sex, Age=age, Smoking=smoking, PC1=pc1, PC2=pc2, PC3=pc3)

Em$ACE <- factor(Em$ACE, levels = c("NO", "YES"))
Em$ADHD_PRS_4q <- as.factor(ntile(Em$ADHD_PRS, 4))
Em$BD_PRS_4q <- as.factor(ntile(Em$BD_PRS, 4))
Em$MDD_PRS_4q <- as.factor(ntile(Em$MDD_PRS, 4))
Em$SCZ_PRS_4q <- as.factor(ntile(Em$SCZ_PRS, 4))

Em[1:5,]

ADHD_m <- glm(ACE ~ ADHD_PRS_4q+Sex+Age+Smoking+PC1+PC2+PC3, data = Em, family = binomial)
BD_m <- glm(ACE ~ BD_PRS_4q+Sex+Age+Smoking+PC1+PC2+PC3, data = Em, family = binomial)
MDD_m <- glm(ACE ~ MDD_PRS_4q+Sex+Age+Smoking+PC1+PC2+PC3, data = Em, family = binomial)
SCZ_m <- glm(ACE ~ SCZ_PRS_4q+Sex+Age+Smoking+PC1+PC2+PC3, data = Em, family = binomial)

summary(ADHD_m)
summary(BD_m)
summary(MDD_m)
summary(SCZ_m)

prepare_table <- function(regression_model, dataframe_name) {
  dataframe_name <- cbind(exp(coef(regression_model)), exp(confint(regression_model)))
  dataframe_name <- as.data.frame(dataframe_name)
  dataframe_name$p <- summary(regression_model)$coefficients[, "Pr(>|z|)"]
  dataframe_name <- dataframe_name[c(2:4),]
  dataframe_name <- rbind(c(1,1,1,1), dataframe_name)
  dataframe_name$quartile <- c("Q1", "Q2", "Q3", "Q4" )
  colnames(dataframe_name) <- c('Odds','LL','UL',"p",  "quartile")
  return(dataframe_name)
}

ADHD_mt <- prepare_table(ADHD_m)
BD_mt <- prepare_table(BD_m)
MDD_mt <- prepare_table(MDD_m)
SCZ_mt <- prepare_table(SCZ_m)

ADHD_mt$Group <- "ADHD"
BD_mt$Group <- "BD"
MDD_mt$Group <- "MDD"
SCZ_mt$Group <- "SCZ"

C_mt <- rbind(ADHD_mt, BD_mt, MDD_mt, SCZ_mt)

#Mutiple testing correction with fdr
C_mt$p.adj <- p.adjust(C_mt$p, method = "fdr")
C_mt$signif <- ifelse(C_mt$p.adj < 0.001, "***",
                      ifelse(C_mt$p.adj < 0.01, "**",
                             ifelse(C_mt$p.adj < 0.05, "*", "")))
C_mt$y_pos <- C_mt$UL + 0.1 # Set y-position slightly above the error bar upper limit

#Plot the figure
CV1p <- ggplot(C_mt, aes(quartile, Odds)) +
  geom_errorbar(aes(ymin = LL, ymax = UL, color = Group), 
                position = position_dodge(0.9), width = 0.2) +
  geom_point(aes(color = Group), position = position_dodge(0.9), size = 3) +
  
  # Add odds values for Q2-Q4 only
  geom_text(aes(label = ifelse(quartile == "Q1", NA, round(Odds, 2)),group = Group), 
  position = position_dodge(0.9), 
  vjust = -1, size = 3.5, color = "black") +
  
  geom_text(aes(y = y_pos, label = signif, group = Group), 
            position = position_dodge(0.9), size = 5, vjust = 0) +
  scale_x_discrete(limits = c("Q1", "Q2", "Q3", "Q4")) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")) +
  
  labs(
    title = "Multiple Pscyhiatric PRS Quantile Odds Plot",
    x = "PRS Quartiles",
    y = "Odds (95% CI)",
    caption = "Note. p.adj < 0.001:***; p.adj < 0.01,**; $p.adj < 0.05:*") + 
  geom_hline(yintercept = 1, color = 'black', linetype = 'dashed', alpha = 0.5) +
  theme_minimal() + 
  theme(plot.caption = element_text(hjust = 0, size = 10)) 

CV1p










