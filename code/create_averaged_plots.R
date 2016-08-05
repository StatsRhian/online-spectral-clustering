# Take all of the results from STORM
# Average over the number or runs
# Plot performance *Purity and V-measure*

rm(list = ls())
library(tidyr)
library(dplyr)
library(ggplot2)

all_results_list <- list.files(path = "results/", full.names = T)

dataset_list <- c("staticNormal", "jumpNormal", "hyperSphere") 
for(dataset in dataset_list){
results_list <- all_results_list[grep(dataset, all_results_list)]

windowed_list <- results_list[grep("window", results_list)]
weighted_list <- results_list[grep("_weighted", results_list)]
unweighted_list <- results_list[grep("_unweighted", results_list)]

#Quick check
length(windowed_list)== length(weighted_list) && length(weighted_list) ==length(unweighted_list)

windowed_sum <- 0
for (i in 1:length(windowed_list)){
  new <- read.csv(windowed_list[i])
  windowed_sum <- windowed_sum + new
}
results_window <- windowed_sum/length(windowed_list)

weighted_sum <- 0
for (i in 1:length(weighted_list)){
  new <- read.csv(weighted_list[i])
  weighted_sum <- weighted_sum + new
}
results_weighted <- weighted_sum/length(weighted_list)

unweighted_sum <- 0
for (i in 1:length(unweighted_list)){
  new <- read.csv(unweighted_list[i])
  unweighted_sum <- unweighted_sum + new
}
results_unweighted <- unweighted_sum/length(unweighted_list)


######Create the plots

results_weighted <- cbind(results_weighted, algorithm ="weighted")
results_unweighted <- cbind(results_unweighted, algorithm ="unweighted")
results_window <- cbind(results_window, algorithm ="window")

results_all <- rbind(results_weighted, results_unweighted ,results_window )

purity_plot <- 
  results_all %>%
  ggplot(aes(batch_number,purity, col = algorithm, lty = algorithm)) +
  geom_line(size = 1.2) +
  scale_y_continuous(limits = c(0, 1)) + 
  xlab("Batch number")+
  ylab("Performance") +
  ggtitle(sprintf("Purity for dataset %s", dataset)) +
  theme(legend.title = element_text(size=18, face="bold")) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.justification=c(1,0), legend.position=c(1,0))+
  theme(plot.title = element_text(size=20))

vmeasure_plot <- 
  results_all %>%
  ggplot(aes(batch_number,vmeasure, col = algorithm, lty = algorithm)) +
  geom_line(size = 1.2) +
  scale_y_continuous(limits = c(0, 1)) + 
  xlab("Batch number")+
  ylab("Performance") +
  ggtitle(sprintf("V measure for dataset %s", dataset)) +
  theme(legend.title = element_text(size=18, face="bold")) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.justification=c(1,0), legend.position=c(1,0))+
  theme(plot.title = element_text(size=20))


ggsave(purity_plot, filename = sprintf("figures/%s_purity.png",dataset))
ggsave(vmeasure_plot, filename = sprintf("figures/%s_vmeasure.png",dataset))
}