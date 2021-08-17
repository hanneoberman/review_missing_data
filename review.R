# lit review
library(tidyverse)
dat <- readxl::read_excel("./asreview_result_clinical-applications-of-ai.xlsx", sheet = "Export", na = "0")

# analyze data
analyze <- function(data) {
  # exclude unused methods and paper characteristics (columns 1-4)
  excl <- colMeans(data, na.rm=TRUE)=="NaN"
  excl[1:4] <- TRUE
  
  # labels of used methods
  methods <- names(data)[!excl]
  
  results <- purrr::map_dfr(1:length(methods), function(x) {
    purrr::map_dfr(methods, function(y) {
      sum(data[, methods[x]] < data[, y], na.rm = TRUE) %>%
        c(., sum(data[, methods[x]] > 0 &
                   data[, y] > 0, na.rm = TRUE)) %>%
        setNames(., c("count", "total")) #name per method: c(methods[x],paste0(methods[x], "_sum"))
    }) %>%
      cbind(method = methods[x], versus = methods, .)
  }) %>%
    mutate(
      method = factor(method, levels = rev(methods)),
      versus = factor(versus, levels = methods),
      label = paste0(count, "/", total)
    )
  is.na(results$label) <- results[, "label"] == "0/0"
  results$diag <- results[, "method"] == results[, "versus"]
  results[results$diag, "label"] <- methods
  results[results$diag, "count"] <- NA
  return(results)
}

# plot results
plot_results <- function(results, title_text){
  ggplot(results, aes(x=versus, y=method, fill=100*count/total, label = label, fontface = ifelse(diag, 2,1)))+
    geom_tile() +
    geom_text() +
    labs(x = "", y="", title = title_text) +
    scale_fill_distiller(palette = "RdYlBu", name = "Performance \n(% studies \nin which \nmethod in \nrow > column)", na.value = "white") + #, low="white", high="darkgrey", na.value = "white") +
    theme_classic()
}

# for all studies combined
results_full <-analyze(dat)
plot_results(results_full, "Comparison of missing data methods, MAR, MNAR and 'real' missingness")

# # longitudinal only
# results_long <- dat %>% 
#   filter(longitudinal==1) %>% 
#   analyze()
# plot_results(results_long, "Comparison of missing data methods, all longitudinal data")

# # cross-sectional only
# results_cros <- dat %>% 
#   filter(is.na(longitudinal)) %>% 
#   analyze()
# plot_results(results_cros, "Comparison of missing data methods, all cross-sectional data")

# MAR, cross-sectional only
results_MAR_cros <- dat %>% filter(MAR==1) %>%
  filter(is.na(longitudinal)) %>%
  analyze() %>% 
  plot_results("Comparison of missing data methods, MAR missingness, cross-sectional data only")
results_MAR_cros

# MNAR, cross-sectional only
results_MNAR_cros <- dat %>% filter(MNAR==1) %>% 
  filter(is.na(longitudinal)) %>% 
  analyze() %>% 
  plot_results("Comparison of missing data methods, MNAR missingness, cross-sectional data only")
results_MNAR_cros

# empirical cross-sectional only
results_emp_cros <- dat %>% filter(empirical==1) %>% 
  filter(is.na(longitudinal)) %>% 
  analyze() %>% 
  plot_results("Comparison of missing data methods, empirical missingness, cross-sectional data only")
results_emp_cros

# MAR longitudinal only
results_MAR_long <- dat %>% filter(MAR==1) %>% 
  filter(longitudinal==1) %>% 
  analyze() %>% 
  plot_results("Comparison of missing data methods, MAR missingness, longitudinal data only")
results_MAR_long

# MNAR longitudinal only
results_MNAR_long <- dat %>% filter(MNAR==1) %>% 
  filter(longitudinal==1) %>% 
  analyze() %>% 
  plot_results("Comparison of missing data methods, MNAR missingness, longitudinal data only")
results_MNAR_long

# empirical longitudinal only
results_emp_long <- dat %>% filter(empirical==1) %>% 
  filter(longitudinal==1) %>% 
  analyze() %>% 
  plot_results("Comparison of missing data methods, empirical missingness, longitudinal data only")
results_emp_long


# combine plots
library(patchwork)
excl_cros <- apply(dat[is.na(dat$longitudinal),-c(1:4)], 2, function(x){all(is.na(x))}) %>% c(rep(TRUE, 4),.)
excl_long <- apply(dat[dat$longitudinal==1,-c(1:4)], 2, function(x){all(is.na(x))}) %>% c(rep(TRUE,4),.) #dat %>% filter(longitudinal==1) %>% colMeans(., na.rm=TRUE) == "NaN"

# cross-sectional
(results_MAR_cros + scale_y_discrete(limits = rev(names(dat)[!excl_cros]))) +
  (results_MNAR_cros + scale_y_discrete(limits = rev(names(dat)[!excl_cros]))) +
  (results_emp_cros + scale_y_discrete(limits = rev(names(dat)[!excl_cros]))) +
  plot_layout(guides = 'collect')

# longitudinal
(results_MAR_long + scale_y_discrete(limits = rev(names(dat)[!excl_long]))) +
(results_MNAR_long + scale_y_discrete(limits = rev(names(dat)[!excl_long]))) +
(results_emp_long + scale_y_discrete(limits = rev(names(dat)[!excl_long]))) +
  plot_layout(guides = 'collect')
# OLD CODE
# results <- purrr::map_dfr(1:15, function(x){
#   purrr::map_dfr(methods, function(y){
#     sum(dat[, methods[x]]<dat[,y], na.rm=TRUE) %>% 
#       c(., sum(dat[, methods[x]]> 0 & dat[,y] > 0, na.rm = TRUE)) %>% 
#       setNames(., c("count", "total")) #name per method: c(methods[x],paste0(methods[x], "_sum"))
#     }) %>% 
#     cbind(method = methods[x], versus = methods, .)}) %>% 
#   mutate(method = factor(method, levels=rev(methods)),
#          versus = factor(versus, levels=methods),
#          label = paste0(count,"/",total)) 
# is.na(results$label) <- results[, "label"]=="0/0"
# results[results[,"method"]==results[,"versus"], "label"] <- methods


# MNAR_results <- purrr::map_dfr(1:19, function(x){
#   purrr::map_dfr(methods, function(y){
#     sum(MNAR_dat[, methods[x]]< MNAR_dat[,y], na.rm=TRUE) %>% 
#       c(., sum(MNAR_dat[, methods[x]]> 0 & MNAR_dat[,y] > 0, na.rm = TRUE)) %>% 
#       setNames(., c("count", "total")) #name per method: c(methods[x],paste0(methods[x], "_sum"))
#   }) %>% 
#     cbind(method = methods[x], versus = methods, .)}) %>% 
#   mutate(method = factor(method, levels=rev(methods)),
#          versus = factor(versus, levels=methods),
#          label = paste0(count,"/",total)) 
# is.na(MNAR_results$label) <- MNAR_results[, "label"]=="0/0"
# MNAR_results[results[,"method"]==MNAR_results[,"versus"], "label"] <- methods

# # check which methods are not used at all
# MNAR_results <- MNAR_results %>% 
#   filter(method != "RNN") %>% filter(method != "kernel") %>% 
#   filter(versus != "RNN") %>% filter(versus != "kernel")

# ggplot(results_MNAR, aes(x=versus, y=method, fill=count/total, label = label))+
#   geom_tile() +
#   geom_text() +
#   labs(x = "", y="", title = "Comparison of missing data methods, MNAR mmissingness only") +
#   scale_fill_gradient(name = "Performance \n(row > column)", low="white", high="darkgrey", na.value = "white") +
#   theme_bw()
