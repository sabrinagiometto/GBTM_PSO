rm(list = ls())

library(nnet)
library(forestplot)
library(dplyr)
library(ggplot2)
library(data.table)


# load data 
DT <- fread(paste0(diroutput, "cohort_final.csv"))
# DT <- fread("C:/Users/giorg/GitHub/GBTM_PSO-repo_DATA/output/cohort_final.csv")
setnames(DT, "_traj_Group", "group" )


#----------------------------
# Define categorical variable
#----------------------------

# group
DT$group <- as.factor(DT$group) 
DT$group <- relevel(DT$group, ref = "3") 

#drug
DT[ustekinumab == 1, drug := "Ustekinumab"]
DT[adalimumab  == 1, drug := "Adalimumab"]
DT[infliximab  == 1, drug := "Infliximab"]
DT[secukinumab  == 1, drug := "Secukinumab"]
DT[etanercept  == 1, drug := "Etanercept"]

DT$drug <- as.factor(DT$drug)
DT$drug <- relevel(DT$drug, ref = "Etanercept")

DT <- DT[!is.na(drug)]

#visit
DT[visite >= 4, Visits:= 1]
DT[visite < 4, Visits:= 0]

# age
DT[eta_c %in% c("71-80", "81-100"), eta_c:= "71-100"]


#-----------
# Regression
#-----------

Formula <- "group ~ sesso + eta_c + drug + Visits + acitretin + antipso_topic + cyclosporin + retinoids + glucorticoidi + antif_non_steroidei"

model <- multinom(formula = Formula, data = DT)

conf_int <- confint(model) 
odds_ratios <- exp(coef(model))

#-----------------
# data preparation
#-----------------

response_levels <- levels(DT$group)[-1]

DT_plt <- data.table(
  
  Group = rep(response_levels, each = ncol(odds_ratios)), 
  
  Variable = rep(colnames(odds_ratios), times = length(response_levels)),
  
  OR = c(odds_ratios[1, ], odds_ratios[2, ], odds_ratios[3, ]),
  
  Lower = c(exp(as.vector(conf_int[, , 1][, 1])), 
            exp(as.vector(conf_int[, , 2][, 1])), 
            exp(as.vector(conf_int[, , 3][, 1]))), 
  
  Upper = c(exp(as.vector(conf_int[, , 1][, 2])), 
            exp(as.vector(conf_int[, , 2][, 2])), 
            exp(as.vector(conf_int[, , 3][, 2])))
)


DT_plt$Label <- paste0(round(DT_plt$OR, 2), " (", round(DT_plt$Lower, 2), "-", round(DT_plt$Upper, 2), ")")

# renaming vars
DT_plt[Group == 1, g := "High vs Early Decline"]
DT_plt[Group == 2, g := "High vs Late Decline"]
DT_plt[Group == 4, g := "High vs Moderate"]

DT_plt[Variable == "sesso" , v := "Gender"]
DT_plt[Variable == "eta" , v := "Age"]

# DT_plt[Variable == "adalimumab" , v := "Adalimumab"]
# DT_plt[Variable == "infliximab" , v := "Infliximab"]
# DT_plt[Variable == "ustekinumab" , v := "Ustekinumab"]
# DT_plt[Variable == "secukinumab" , v := "Secukinumab"]
# DT_plt[Variable == "visite" , v := "Dermatological Visit"]
# DT_plt[Variable == "acitretin" , v := "Acitretin"]
# DT_plt[Variable == "antipso_topic" , v := "Anti-psoriatic topic"]
# DT_plt[Variable == "cyclosporin" , v := "cyclosporin"]
# DT_plt[Variable == "retinoids" , v := "Retinoids"]
# DT_plt[Variable == "glucorticoidi" , v := "Glucorticoidi"]
# DT_plt[Variable == "antif_non_steroidei" , v := "NSAID"]



DT_plt[Variable == "(Intercept)",         v := "Intercept"]
DT_plt[Variable == "Gender",              v := "Gender: Male vs Female"]
DT_plt[Variable == "eta_c21-40",          v := "Age: 21-40 vs 0-20"]
DT_plt[Variable == "eta_c41-50",          v := "Age: 41-50 vs 0-20"]
DT_plt[Variable == "eta_c51-60",          v := "Age: 51-60 vs 0-20"]
DT_plt[Variable == "eta_c61-70",          v := "Age: 61-70 vs 0-20"]
DT_plt[Variable == "eta_c71-100",         v := "Age: 71-100 vs 0-20"]
DT_plt[Variable == "drugAdalimumab",      v := "Drug: Adalimumab vs Etanercept"]
DT_plt[Variable == "drugInfliximab",      v := "Drug: Infliximab vs Etanercept"]
DT_plt[Variable == "drugSecukinumab",     v := "Drug: Secukinumab vs Etanercept"]
DT_plt[Variable == "drugUstekinumab",     v := "Drug: Ustekinumab vs Etanercept"]
DT_plt[Variable == "Visits",              v := "Visits: more than 4 vs less than 4"]
DT_plt[Variable == "acitretin",           v := "Acitretin: use vs no use"]
DT_plt[Variable == "antipso_topic",       v := "Topic anti-psoriatic: use vs no use"]
DT_plt[Variable == "cyclosporin",         v := "Cyclosporin: use vs no use"]
DT_plt[Variable == "retinoids",           v := "Retinoids: use vs no use"]
DT_plt[Variable == "glucorticoidi",       v := "Glucorticoidi: use vs no use"]
DT_plt[Variable == "antif_non_steroidei", v := "NSAID: use vs no use"]
 

DT_plt$v <- factor(DT_plt$v, levels = c(
  "NSAID: use vs no use",
  "Glucorticoidi: use vs no use",
  "Retinoids: use vs no use",
  "Cyclosporin: use vs no use",
  "Topic anti-psoriatic: use vs no use",
  "Acitretin: use vs no use",
  "Visits: more than 4 vs less than 4",
  "Drug: Ustekinumab vs Etanercept",
  "Drug: Secukinumab vs Etanercept",
  "Drug: Infliximab vs Etanercept",
  "Drug: Adalimumab vs Etanercept",
  "Age: 71-100 vs 0-20",
  "Age: 61-70 vs 0-20",
  "Age: 51-60 vs 0-20",
  "Age: 41-50 vs 0-20",
  "Age: 21-40 vs 0-20",
  "Gender: Male vs Female",
  "Intercept"
))

DT_plt[, v_od := paste0(v, " - ", round(odds_ratios, 2))]




# size 

DT_plt$weight <-  1 / ((log(DT_plt$Upper) - log(DT_plt$Lower)) / 3.92)^2

#-----
# Plot
#-----
# ggplot(DT_plt[Variable != "(Intercept)"], aes(x = v, y = 1/OR, ymin = 1/Upper, ymax = 1/Lower, color = g)) +
#   geom_pointrange(position = position_dodge(width = 0.6), size = 0.6) + 
#   geom_errorbar(aes(ymin = 1/Upper, ymax = 1/Lower), width = 0.3, position = position_dodge(width = 0.6)) + 
#   geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.5) + 
#   geom_point(position = position_dodge(width = 0.6), size = 1) + 
#   #geom_point(aes(size = weight), position = position_dodge(width = 0.6)) + 
#   geom_text(aes(y = 0.2, label = round(1/OR, 2),  group = g), #paste(round(1/OR, 2), " (", round(1/Lower, 2), ";", round(1/Upper, 2) , ")")
#             position = position_dodge(width = 0.6), 
#             hjust = 1.2, 
#             size = 2.5, 
#             color = "grey40") +
#   coord_flip() + 
#   scale_y_continuous(trans = "log", 
#                      breaks = c( 0.5, 1, 2),
#                      labels = c( "0.5", "1", "2")) + 
#   labs(title = "", x = "", y = "", legend = "") +
#   #scale_color_brewer(palette = "Set2") +
#   scale_color_manual(values = c('#1A85FF', '#D5165D', '#FFD400')) +
#   theme_minimal(base_size = 14) + 
#   theme(legend.position = "bottom",
#         panel.grid.major.y = element_blank(), 
#         panel.grid.minor = element_blank(),
#         #axis.text.y = element_text(face = "bold"), 
#         legend.title = element_blank()) 
# 
# 
# ggsave("forest_plot_v2.png",  width = 10, height = 12, dpi = 300)
# 
# 
# ## labels
# 
#  ggplot(DT_plt[Variable != "(Intercept)"], aes(x = v, y = 1/OR, ymin = 1/Upper, ymax = 1/Lower, color = g)) +
#   geom_pointrange(position = position_dodge(width = 0.6), size = 0.6) + 
#   geom_errorbar(aes(ymin = 1/Upper, ymax = 1/Lower), width = 0.3, position = position_dodge(width = 0.6)) + 
#   geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.5) + 
#   geom_point(position = position_dodge(width = 0.6), size = 1) + 
#   geom_text(aes(y = -1.5, label = paste0(round(1/OR, 2), " (", 
#                                         round(1/Upper, 2), "-" ,
#                                         round(1/Lower, 2), ")"), 
#                 group = g), #paste(round(1/OR, 2), " (", round(1/Lower, 2), ";", round(1/Upper, 2) , ")")
#             position = position_dodge(width = 0.6), 
#             hjust = 0.5, 
#             size = 4.9, 
#             color = "grey40") +
#   coord_flip() + 
#   scale_y_continuous( limits = c(-2, 2), 
#                       breaks = 0) + 
#   labs(title = "", x = "", y = "", legend = "") +
#   #scale_color_brewer(palette = "Set2") +
#   scale_color_manual(values = c('#1A85FF', '#D5165D', '#FFD400')) +
#   theme_minimal(base_size = 14) + 
#   theme(legend.position = "bottom",
#         panel.grid.major.y = element_blank(), 
#         panel.grid.minor = element_blank(),
#         #axis.text.y = element_text(face = "bold"), 
#         legend.title = element_blank()) 
# 
# 
# 
#  
# ggsave("label_forest_plot.png",  width = 10, height = 12, dpi = 300)

#################

# Latest forestplot

library(ggplot2)
library(patchwork)

y_min <- 0.15
y_max <- 4.5

DT_plt[, inv_or    := 1 / OR]
DT_plt[, inv_lower := 1 / Upper]
DT_plt[, inv_upper := 1 / Lower]
DT_plt[, ci_label  := paste0(round(inv_or, 2), " (", round(inv_lower, 2), "\u2013", round(inv_upper, 2), ")")]

dt <- DT_plt[Variable != "(Intercept)"]

dt[, inv_or_clip    := pmax(y_min, pmin(y_max, inv_or))]
dt[, inv_lower_clip := pmax(y_min, pmin(y_max, inv_lower))]
dt[, inv_upper_clip := pmax(y_min, pmin(y_max, inv_upper))]

p_main <- ggplot(
  dt,
  # aes(x = v, y = inv_or, ymin = inv_lower, ymax = inv_upper, color = g)
  aes(x = v, y = inv_or_clip, ymin = inv_lower_clip, ymax = inv_upper_clip, color = g)
) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray70", linewidth = 0.4) +
  geom_errorbar(
    position = position_dodge(width = 0.6),
    width = 0.25, linewidth = 0.5
  ) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 2
  ) +
  coord_flip(ylim = c(y_min, y_max)) +
  scale_y_continuous(
    trans  = "log",
    breaks = c(0.25, 0.5, 1, 2, 4),
    labels = c("0.25", "0.5", "1", "2", "4")
  ) +
  scale_color_manual(values = c('#1A85FF', '#D5165D', '#FFD400'),
                     guide = guide_legend(override.aes = list(shape = 16, linetype = 0))) +
  labs(x = "", y = "OR", color = "") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.title       = element_blank(),
    axis.text.y        = element_text(size = 9),
    axis.text.x        = element_text(size = 11, color = "gray40"),
    plot.margin        = margin(5, 2, 5, 5)
  )



# p_label <- ggplot(
#   dt,
#   aes(x = v, y = 0, label = ci_label, color = g)
# ) +
#   geom_text(
#     position = position_dodge(width = 0.6),
#     hjust = 0, size = 2.8
#   ) +
#   coord_flip() +
#   scale_x_discrete(limits = levels(factor(dt$v))) +
#   scale_color_manual(values = c('black', 'black', 'black'), guide = "none") +
#   theme_void() +
#   theme(
#     legend.position = "none",
#     plot.margin     = margin(5, 5, 5, 0)
#   )

library(cowplot)


p_main <- p_main +
  geom_text(
    aes(y = y_max * 1.8, label = ci_label, group = g),
    position = position_dodge(width = 0.6),
    hjust = 0, size = 2.8, color = "gray30"
  ) +
  coord_flip(ylim = c(y_min, y_max), clip = "off") +
  theme(plot.margin = margin(5, 120, 5, 5))

png("forest_plot_v4.png", width = 18, height = 23, res = 300, units = "cm")

# p_main + p_label + plot_layout(widths = c(1.3, 1), guides = "collect") &
#   theme(legend.position = "bottom",
#         legend.justification = c(0.2, 1))




# legend <- get_legend(p_main + theme(legend.position = "bottom",
#                                     legend.justification = "center"))
# 
# p_nolegg <- p_main + theme(legend.position = "none")
# 
# plot_grid(
#   plot_grid(p_nolegg, p_label, nrow = 1, rel_widths = c(2.5, 1)),
#   legend,
#   ncol = 1,
#   rel_heights = c(1, 0.08)
# )


print(p_main)


dev.off()

