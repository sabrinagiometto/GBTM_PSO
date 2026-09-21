# load data 
data <- fread(paste0(diroutput, "/cohort_final.csv"))

### Group membership probability distributions
#View(data[, (ncol(data)-5):ncol(data)])
summary(data[`_traj_Group` == 1, `_traj_ProbG1`])
summary(data[`_traj_Group` == 2, `_traj_ProbG2`])
summary(data[`_traj_Group` == 3, `_traj_ProbG3`])
summary(data[`_traj_Group` == 4, `_traj_ProbG4`])




