list <- read.table("sim_data/dataset_list.txt", stringsAsFactors = F, header = F, col.names = F)
list <- list[[1]]
                  
my_list <- list[c(1:5, 14,15)]

for (dataset in my_list){

source("create_test_train_sims_files.R")

train <- cbind(trainData[[1]], trainData[[2]])
test <- cbind(testData[[1]], testData[[2]])

write.table(x = train, file = sprintf("sim_data/train/%s.csv", dataset), sep = ",", col.names = F, row.names = F)
write.table(x = test, file = sprintf("sim_data/test/%s.csv", dataset), sep = ",", col.names = F, row.names = F)
}