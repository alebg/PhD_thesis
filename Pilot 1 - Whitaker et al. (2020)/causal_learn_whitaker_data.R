library(dagitty)

data <- read.table("dataset_whitaker.csv", header = TRUE, sep = ",")
data$OUTCOME <- NULL  # Keep only the variables used by Whitaker et al.
data$X <- NULL
data$PHC <- NULL  # bnstruct doesn't like constants

install.packages("bnstruct")
library(bnstruct)

# bnstruct allows to infer DAGs from data

# Will use bnstruct::learn.network() to infer a DAG from data

# Preparation:
data_asnumbers <- as.data.frame(lapply(data, as.numeric))
# stupid library doesn't like when starting from 0
data_asnumbers <- data_asnumbers + 1

bnstruct_data <- BNDataset(data = data_asnumbers,
  discreteness = rep(TRUE, ncol(data_asnumbers)),
  variables = names(data_asnumbers),
  node.sizes = c(2, 3, rep(2, ncol(data_asnumbers) - 2))
)

# Use the learn.network mmhc algorithm
bnstruct_mmhc_out <- learn.network(bnstruct_data, algo = "mmhc")

install.packages("qgraph")
library(qgraph)
# JESUS CHRIST R PEOPLE

png("bnstruct_mmhc.png", width = 1000, height = 1000, res = 300)
plot(bnstruct_mmhc_out, method = "qgraph")
dev.off()

# Use the learn.network sem algorithm
bnstruct_sem_out <- learn.network(bnstruct_data, algo = "sem")

png("bnstruct_sem.png", width = 1000, height = 1000, res = 300)
plot(bnstruct_sem_out, method = "qgraph")
dev.off()


# Use the hc algorithm

install.packages("bnlearn")
library(bnlearn)

bnlearn_hc_out <- hc(data_asnumbers)
png("bnlearn_hc.png", width = 1000, height = 1000, res = 300)
plot(bnlearn_hc_out)
dev.off()
