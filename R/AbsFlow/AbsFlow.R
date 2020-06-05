# Title     : Absolute Flow database analyse
# Objective : Analyze data from absolute flow database at 04-06-2020
# Created by: Jo
# Created on: 04-6-2020
library(tidyverse)
library(readxl)
library(gridExtra) #to combine plots like the one from missing value's
setwd("~/R/Git/Rprojects/R/AbsFlow") #set directory
AF <- read_xlsx("~/R/Git/Rprojects/R/AbsFlow/AllPatients0620clean.xlsx") 

# missing value's
sum(is.na(AF))
as_tibble(AF)
summary(AF) # summary of data

#plots for percentage of missing values
missing.values <- AF %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

levels <- (missing.values  %>% filter(isna == T) %>%
             arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)),
               y = pct, fill=isna),
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "",
                    values = c('steelblue', 'tomato3'),
                    labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values",
       x = 'Variable', y = "% of missing values")
# percentage.plot # plot the percentage missing values

row.plot <- AF %>%
  mutate(id = row_number()) %>%
  gather(-id, key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  ggplot(aes(key, id, fill = isna)) +
  geom_raster(alpha=0.8) +
  scale_fill_manual(name = "",
                    values = c('steelblue', 'tomato3'),
                    labels = c("Present", "Missing")) +
  scale_x_discrete(limits = levels) +
  labs(x = "Variable",
       y = "Row Number", title = "Missing values in rows") +
  coord_flip()
# row.plot #plot the missing values per row
grid.arrange(percentage.plot, row.plot, ncol = 2) #combine the two plots

# associations between data objects
#correlograms from Correlogam in R towardsdatascience.com
library(corrplot)
library(lares)
#modify corrplot function
dat = AF
corrplot2 <- function(data,
                      method = "pearson",
                      sig.level = 0.05,
                      order = "original",
                      diag = FALSE,
                      type = "upper",
                      tl.srt = 90,
                      number.font = 1,
                      number.cex = 1,
                      mar = c(0, 0, 0, 0)) {
  library(corrplot)
  data_incomplete <- data
  data <- data[complete.cases(data), ]
  mat <- cor(data, method = method)
  cor.mtest <- function(mat, method) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], method = method)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  p.mat <- cor.mtest(data, method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(mat,
           method = "color", col = col(200), number.font = number.font,
           mar = mar, number.cex = number.cex,
           type = type, order = order,
           addCoef.col = "black", # add correlation coefficient
           tl.col = "black", tl.srt = tl.srt, # rotation of text labels
           # combine with significance level
           p.mat = p.mat, sig.level = sig.level, insig = "blank",
           # hide correlation coefficiens on the diagonal
           diag = diag
  )
}

corrplot2(data = dat, method = "pearson", sig.level = 0.05, order = "original", type = "upper", tl.srt = 75)
corr_cross(dat, max_pvalue = 0.05, top = 100)

corr_var(dat, # name of dataset
         RLAD_1, # name of variable to focus on
         top = 50 # display top 10 correlations
)

corr_var(dat, # name of dataset
         RRCA_1, # name of variable to focus on
         top = 50 # display top 10 correlations
)

corr_var(dat, # name of dataset
         R_CX_1, # name of variable to focus on
         top = 50 # display top 10 correlations
)