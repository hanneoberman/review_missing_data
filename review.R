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
  ggplot(results, aes(x=versus, y=method, fill=100*count/total, label = label))+
    geom_tile() +
    geom_text() +
    labs(x = "", y="", title = title_text) +
    scale_fill_distiller(palette = "RdYlBu", name = "Performance \n(% studies \nrow > column)", na.value = "white") + #, low="white", high="darkgrey", na.value = "white") +
    theme_bw()
}

# for all studies combined
results_full <-analyze(dat)
plot_results(results_full, "Comparison of missing data methods, MAR, MNAR and 'real' missingness")

# MAR only
results_MAR <- dat %>% filter(MAR==1) %>% filter(is.na(longitudinal)) %>% analyze()
plot_results(results_MAR, "Comparison of missing data methods, MAR missingness, cross-sectional data only")

# MNAR only
results_MNAR <- dat %>% filter(MNAR==1) %>% filter(is.na(longitudinal)) %>% analyze()
plot_results(results_MNAR, "Comparison of missing data methods, MNAR missingness, cross-sectional data only")

# empirical only
results_emp <- dat %>% filter(empirical==1) %>% 
  filter(is.na(longitudinal)) %>% 
  analyze()
plot_results(results_emp, "Comparison of missing data methods, empirical missingness only")

# longitudinal only
results_long <- dat %>% filter(longitudinal==1) %>% analyze()
plot_results(results_long, "Comparison of missing data methods, longitudinal data only")


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
