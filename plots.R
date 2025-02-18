library(tidyverse)
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
allcvd_1 = read_csv("Data/allcvd_22_medians.csv")
allcvd_3 = readxl::read_xlsx("Data/allcvd_20_22.xlsx")

names(allcvd_1)
names(allcvd_3)
namefix = tolower(names(allcvd_3)[-1])
namefix = t(simplify2array(str_split(namefix, "_")))
t(simplify2array(str_split(names(allcvd_1)[-1], "_")))
racekey = c("all" = "all", "aia" = "aian", "asn" = "asian", "nhp" = "nhopi", "mor" = "multi", "blk" = "black", "his" = "hispanic", "wht" = "white")
genkey = c("f" = "f", "m" = "m", "a" = "all")
agekey = c("35up" = "35up", "3564" = "35-64", "65up" = "65up", "all" = "all")

unique(namefix[, 3])

unique(t(simplify2array(str_split(names(allcvd_1)[-1], "_")))[, 1])
namefix = namefix[, c(3, 1, 2)]
namefix[, 1] = agekey[namefix[, 1]]
namefix[, 2] = racekey[namefix[, 2]]
namefix[, 3] = genkey[namefix[, 3]]
namefix = apply(namefix, 1, paste, collapse = "_")
namefix = c("stcty_fips", namefix)
colnames(allcvd_3) = namefix

ctys = intersect(rownames(allcvd_1), rownames(allcvd_3))
grps = intersect(names(allcvd_1), names(allcvd_3))

allcvd_1 = allcvd_1[ctys, grps] %>% arrange(stcty_fips)
allcvd_3 = allcvd_3[ctys, grps] %>% arrange(stcty_fips)
allcvd_3[allcvd_3 < 0] = NA

bw_groups = c("35up_asian_f", "35-64_white_all", "65up_black_m")
hist_groups = c("all_white_f", "35up_aian_m", "all_nhopi_m")
plot_groups = c("all_black_f", "35-64_hispanic_m", "65up_white_m")

par(mfrow = c(4, 6))
for (cat in names(allcvd_1)[-1]) {
  if (sum(is.na(allcvd_1[, cat])) < 2700 & sum(is.na(allcvd_3[, cat])) < 2700) {
    maxlim = max(simplify2array(c(allcvd_1[, cat], allcvd_3[, cat])), na.rm = TRUE)
    plot(
      as_vector(allcvd_1[, cat]),
      as_vector(allcvd_3[, cat]), 
      xlim = c(0, maxlim), 
      ylim = c(0, maxlim),
      main = cat,
      xlab = "single_year",
      ylab = "three_year"
    )
    abline(0, 1, col = "blue")
  }
}

par(mfrow = c(4, 6))
for (cat in names(allcvd_1)[-1]) {
  if (sum(is.na(allcvd_1[, cat])) < 2700 & sum(is.na(allcvd_3[, cat])) < 2700) {
    est_13 = simplify2array(c(allcvd_1[, cat], allcvd_3[, cat]))
    lims = c(min(est_13, na.rm = TRUE), max(est_13, na.rm = TRUE))
    breaks = seq(lims[1], lims[2], length.out = 21)
    hist(as_vector(allcvd_3[, cat]), xlim = lims, main = cat, breaks = breaks, xlab = "Estimated rates")
    hist(as_vector(allcvd_1[, cat]), col = "#00990044", breaks = breaks, add = TRUE)
  }
}

par(mfrow = c(4, 6))
for (cat in names(allcvd_1)[-1]) {
  if (sum(is.na(allcvd_1[, cat])) < 2700 & sum(is.na(allcvd_3[, cat])) < 2700) {
    cons = data.frame(
      single_year = as_vector(allcvd_1[, cat]),
      three_year = as_vector(allcvd_3[, cat])
    )
    boxplot(cons, main = cat)
  }
}

cvsum = read_csv("Data/Table Comparison/cv_summary.csv")
cvsum
par(mfrow = c(1, 1))
lims = c(0, max(cvsum$`NA Count1Y`, cvsum$`NA Count3Y`))
plot(
  3134 - cvsum$`NA Count1Y`,
  3226 - cvsum$`NA Count3Y`, 
  xlim = lims,
  ylim = lims,
  xlab = "single_year",
  ylab = "three_year",
  main = "Number of Non-Suppressed Counties"
)
abline(0, 1, col = "blue")
