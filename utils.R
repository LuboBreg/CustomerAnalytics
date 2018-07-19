# Santa's little helpers

# Export plot in PDF and PNG (optionally EPS)
ExportPlot <- function(gplot, filename, width = 2, height = 1.5) {
  # Notice that A4: width=11.69, height=8.27
  ggsave(paste(filename, '.pdf', sep = ""), gplot, width = width, height = height)
  #postscript(file = paste(filename, '.eps', sep = ""), width = width, height = height)
  print(gplot)
  dev.off()
  png(file = paste(filename, '_.png', sep = ""), width = width * 100, height = height * 100)
  print(gplot)
  dev.off()
}

## define a helper function for replacing blanks with NAs
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}

# function that returns majority class per column and respective percentage
majority_percent <- function (x) {
  t <- prop.table(table(x))[which.max(prop.table(table(x)))]
  res <- c(names(t), round(t, 3))
  return(res)
}

## function for getting number of levels per variable
num.cats <- function(col){
  t <- table(col)
  l <- length(t)
  return(l)
}

## function for exluding variables that are correlated
## from df of pairs of varibles and their correlation we sleect only one variable per pair
## from each pair we drop the variable with lower information value
drop_vars <- function(row, IV_df){
  row <- row[c(1,2)]
  res <- IV_df %>%
    filter(Variable %in% row) %>%
    arrange(IV) %>%
    slice(1) %>%
    select(Variable) %>%
    pull() %>%
    as.character()
  return(res)
}

# Function recognizes categorical vars with categories that cover only negligible number of observations
poor_levels <- function(dtf, nms = colnames(dtf), trim = 0.01 * nrow(dtf)) {
  stopifnot(is.null(nms) | (is.character(nms) & length(nms) > 0))
  stopifnot(is.data.frame(dtf))
  
  level_table <-
    lapply(dtf[, which(lapply(dtf[, nms], class) %in% c("character", "factor"))], table)
  lapply(level_table, function(tbl) {
    names(which(tbl < trim))
  })
}

cap_outliers <- function(x, threshold){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- threshold * IQR(x, na.rm = T)
  x <- ifelse(x < (qnt[1] - H) , caps[1], x)
  x <- ifelse(x > (qnt[2] + H) , caps[2], x)
  return(x)
}