#Create purity plot for dataset "norm"
rm(list = ls())
library(tidyr)
library(dplyr)
library(ggplot2)

dataset_list <- read.table("../data/sim_data/dataset_list.txt")
dataset_list <- dataset_list[[1]] 
for(dataset in dataset_list){
  
setwd("E:/Dropbox/current_sims/results/")
  
files <- list.files()
selected_files <- files[grep(dataset, files)]

results_weighted <- as.data.frame(read.csv(selected_files[grep("_weighted", selected_files)]))
results_unweighted <-  as.data.frame(read.csv(selected_files[grep("_unweighted", selected_files)]))
results_window <- as.data.frame(read.csv(selected_files[grep("_window", selected_files)]))

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


ggsave(purity_plot, filename = sprintf("../figures/%s_purity.png",dataset))
ggsave(vmeasure_plot, filename = sprintf("../figures/%s_vmeasure.png",dataset))
}