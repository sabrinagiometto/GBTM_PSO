# cut off parameter
cut.off <- 0.8

# loading  data 
data <- fread(paste0(dirinput, "adherence.csv"))
cohort <- fread(paste0(dirinput, "cohort.csv"))

# trasform adherence in binary, using cut.off
data.bin <- data[, lapply(.SD, function(x) ifelse(x >= cut.off, 1, 0)), 
                 .SDcols = 3:14]

# add ID
data.bin <- cbind(ID = data$ID, data.bin)
names(data.bin) <- c("ID", paste0("AD_", 1:12))

# time matrix (1:12, same for each individual)
DT_time <- as.data.table(matrix(1:12, nrow(data.bin), 12, byrow = T))
names(DT_time) <- paste0("t_", 1:12)
Data.clean <- cbind(data.bin, DT_time)

# covariate
Data.clean <- merge(Data.clean, 
                    cohort, 
                    by = "ID", 
                    all.x = T)

# rename index drug
Data.clean[COD_ATC5 == "L04AB01", Etanercept := 1][is.na(Etanercept), Etanercept :=0]
Data.clean[COD_ATC5 == "L04AB04", Adalimumab := 1][is.na(Adalimumab), Adalimumab:=0]
Data.clean[COD_ATC5 == "L04AB02", Infliximab := 1][is.na(Infliximab), Infliximab:=0]
Data.clean[COD_ATC5 == "L04AC05", Ustekinumab := 1][is.na(Ustekinumab), Ustekinumab:=0]
Data.clean[COD_ATC5 == "L04AC10", Secukinumab := 1][is.na(Secukinumab), Secukinumab:=0]

# reclassify age
Data.clean[ETA < 20, young := 1][is.na(young), young := 0]
Data.clean[ETA >= 20 & ETA <40, young_adult := 1][is.na(young_adult), young_adult := 0]
Data.clean[ETA >= 40 & ETA < 65, adult := 1][is.na(adult), adult := 0]
Data.clean[ETA >= 65, senior := 1][is.na(senior), senior := 0]

# rename non biologic drug covariate
setnames(Data.clean, "SMALL_MOL_PSO", "non_bio_drugs")

fwrite(Data.clean, paste0(dirinput, "Data_covs.csv"))
