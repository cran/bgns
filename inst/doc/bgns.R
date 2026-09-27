## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", error = FALSE)

## ----installation, eval=FALSE-------------------------------------------------
# install.packages("remotes")
# remotes::install_github("metaddict/bgns")

## ----measurements-------------------------------------------------------------
library(bgns)

set.seed(11)
x <- matrix(rnorm(80 * 6), nrow = 80, ncol = 6)
colnames(x) <- paste0("item_", seq_len(ncol(x)))
x[, 2] <- 0.8 * x[, 1] + 0.2 * rnorm(nrow(x))
x[, 3] <- -0.7 * x[, 1] + 0.3 * rnorm(nrow(x))
x[1:6, 4] <- NA_real_

cm <- bicor(x, min_overlap = 3)
round(cm, 3)

## ----correlation-edges--------------------------------------------------------
edges <- bicor(x, tidy = TRUE, threshold = 0.5)
edges

## ----nearest-neighbors--------------------------------------------------------
kn <- bicor_knn(x, knn = 2, threshold = 0)
head(kn)

## ----sparse-measurements------------------------------------------------------
set.seed(12)
x_zero <- x
index <- sample.int(length(x_zero), floor(0.2 * length(x_zero)))
x_zero[index] <- 0
xs <- Matrix::Matrix(x_zero, sparse = TRUE)

edges_sparse <- bicor(xs, tidy = TRUE, threshold = 0.5)
kn_sparse <- bicor_knn(xs, knn = 2, threshold = 0)
head(edges_sparse)
head(kn_sparse)

## ----cross-matrix-neighbors---------------------------------------------------
set.seed(13)
y <- cbind(
  target_A = x[, 1] + rnorm(nrow(x), sd = 0.2),
  target_B = x[, 3] + rnorm(nrow(x), sd = 0.2)
)
kn_xy <- bicor_knn(
  xs, y, knn = 2, threshold = 0.5,
  bipartite_levels = "separate"
)
kn_xy

## ----overlap-normalization----------------------------------------------------
x_missing <- x[, 1:2]
x_missing[1:20, 2] <- NA_real_

r_column <- bicor(x_missing, use_intersection_denominator = FALSE)[1, 2]
r_shared <- bicor(x_missing, use_intersection_denominator = TRUE)[1, 2]
c(column_denominator = r_column, shared_denominator = r_shared)

## ----overlap-eligibility------------------------------------------------------
n_shared <- sum(is.finite(x_missing[, 1]) & is.finite(x_missing[, 2]))
n_shared
bicor(x_missing, min_overlap = n_shared + 1)[1, 2]
bicor(x_missing, pairwise.complete.obs = FALSE)[1, 2]

## ----degenerate-columns-------------------------------------------------------
z <- cbind(
  variable = x[, 1],
  constant = rep(1, nrow(x)),
  zero_heavy = c(rep(0, 60), seq_len(20)),
  all_missing = rep(NA_real_, nrow(x))
)
diag(bicor(z))

## ----runtime-settings, eval=FALSE---------------------------------------------
# Sys.setenv(BGNS_MEM_MB = "256", BGNS_NUM_THREADS = "2")

## ----session-info-------------------------------------------------------------
sessionInfo()

