#
# Microarray Challenges
#
# There are 33 slides, which are really 3 replications of
# 11 sets of slide layouts (see column `set`).
#
# Each slide has 340 wells for 340 genes (with each
# well given measurements across 7 rows)
# which we aggregated in our first exercise.
# Thus, there are 340 genes * 11 sets = 3740 genes
# that can be studied here.
#
#
# EVERYONE: Learn something about grid graphics!  This is
# a straight visualization challenge.  My recommendation
# would be to produce a single graphic summarizing the
# normalized measurements of a single slide.

library(ggplot2)
library(grid)
library(gridExtra)
y <- read.csv("microarray_data_cleaned.csv", as.is = TRUE)
# Choose 1st slide to make graphics for
y.1 <- y[y$slide == 1, ]
# Do PCA to reduce dimension for box plot
pca <- prcomp(y.1[, -(131:138)])
summary(pca)
# Plot PC1 and PC2 and types as colours
pca.plot <- ggplot(data.frame(pca$x), aes(x = PC1, y = PC2, colour = y.1$type)) +
  geom_point() +
  scale_colour_discrete(name = "Gene Types") +
  theme_bw() +
  xlab("PC1 (46.12% of variation)") +
  ylab("PC2 (21.21% of variation)")
# Box plot
box <- ggplot(y.1, aes(x = factor(type), y = pca$x[,1])) +
  geom_boxplot() + 
  theme_bw() +
  xlab("Gene Types") + 
  ylab("First PC of all 130 variables")
# Bar Plot
bar <- ggplot(y.1, aes(x = factor(type))) +
  geom_bar() + 
  theme_bw() +
  xlab("Gene Types") +
  ylab("Number of samples")
# Layout matrix
lay <- rbind(c(1,1,2,2),
             c(1,1,2,2),
             c(1,1,3,3),
             c(1,1,3,3))
grid.arrange(grobs = list(pca.plot, bar, box), layout_matrix = lay)

# MORE INTERESTED IN BIOLOGY THAN BASKETBALL?
#
# One interesting question would be to identify the
# genes that are "most different" from the negative
# controls (type == "rf").  Another (and not necessarily
# similar) would be to look for genes that are 
# "most similar to" the positive controls.  Some
# dimensionality reduction may be necessary.

# Do PCA for data first to reduce dimensionality
pca.all <- prcomp(y[, -(131:137)])
# Combine the first PC from the PCA and the gene types into a new data frame for ANOVA
a <- data.frame(pc1 = pca.all$x[, 1], type = na.omit(y)$type, stringsAsFactors = FALSE)
# Create factors and make gene type "rf" the baseline
a$type <- factor(a$type)
a$type <- relevel(a$type, "rf")
lm.a <- lm(pc1 ~ type, data = a)
summary(lm.a)
