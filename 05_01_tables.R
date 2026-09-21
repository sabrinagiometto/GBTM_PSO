# load data 
data <- fread(paste0(diroutput, "/cohort_final.csv"))

# change group names
setnames(data, "_traj_Group", "group")

data[cod_atc5 == "L04AB01", cod_atc5 := "Etanercept"]
data[cod_atc5 == "L04AB04", cod_atc5 := "Adalimumab"]
data[cod_atc5 == "L04AB02", cod_atc5 := "Infliximab"]
data[cod_atc5 == "L04AC05", cod_atc5 := "Ustekinumab"]
data[cod_atc5 == "L04AC10", cod_atc5 := "Secukinumab"]


## Vector of variables to summarize
myVars <- tolower(c("ETA", "VISITE"))

## Vector of categorical variables that need transformation
catVars <- tolower(c("SESSO", 
             "ETA_c", 
             "COD_ATC5", 
             "LUNG", 
             "INFAR",
             "STROKE",
             "HYPERT",
             "OTHER_CV",
             "DIABETES",
             "FRACTURE",
             "DEPRESSION",
             "GASTRO",
             "OTHER_GASTRO",
             "SJOGREN",
             "REUMA_NOD",
             "REUMA_LUNG",
             "MYOPAT",
             "POLYNEURO",
             "cancer",
             "SMALL_MOL_PSO_v2",
             "ACITRETIN",
             "ANTIPSO_TOPIC",
             "APREMILAST",
             "CYCLOSPORIN",
             "METHOTREXATE_merge",
             "PSORALENS_SYST",
             "PSORALENS_TOPIC",
             "RETINOIDS",
             "DRUG_PSO_SISTEMICI",
             #"NO_FARMACO_PSO",
             "GLUCORTICOIDI",
             "ANTIF_NON_STEROIDEI",
             "ANALGESICI_OPPIOIDI"))
             #"NO_FARMACO_ALL"


# create table 1
tab <- CreateTableOne(vars = c(myVars, catVars, "group"),  factorVars = catVars, data = data, strata = "group" )

# flextable format
tab_flex <- as.data.frame(print(tab, showAllLevels = TRUE, formatOptions = list(big.mark = ",")))
tab_flex$var = rownames(tab_flex)
tab_flex <- flextable(tab_flex)

# saving in .docx format
doc <- read_docx() %>% 
  body_add_flextable(tab_flex)

print(doc, target = paste0(diroutput, "tab_output.docx"))

 