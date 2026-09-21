#-----------------------------------------------------------------
# Import results: all poly combination (1 to 4) for 4 gruops model
#-----------------------------------------------------------------
list_of_models <- vector(mode="list")

for (a in 1:4) {
  for (b in 1:4) {
    for (c in 1:4) {
      for (d in 1:4) {
        cat(a,b,c,d, "\n")
        mod <- fread(paste0(dirintermediate, 
                            "modelli_4_gruppi/results_",
                            a, "_", b, "_", c, "_", d, 
                            ".txt"))
        
        mod <- mod[!(V2 %in% c("(1)", "y1", "", "." ))][V1 != ""]
        
        mod[, V3:= ifelse(gregexpr("\\*", V2)[[1]][1] == -1, 
                          0, 
                          lengths(gregexpr("\\*", V2))), 
            by =seq_len(nrow(mod))]
        
        mod[,V2:=gsub("\\*", "", V2)]
        
        names(mod) <- c("parametro", "valore", "signifi")
        
        sum <- dcast(mod, . ~ parametro, value.var = c("valore", "signifi")) 
        sum[, model := paste0(a,b,c,d)]
        
        list_of_models[[paste0(a,b,c,d)]] <- sum
        
      }
    }
  }
}

summary_modelli<-rbindlist(list_of_models,use.names=T,fill=T)

summary<-summary_modelli[,.(model,
                            valore_AIC, 
                            valore_BIC_data,
                            valore_BIC_subject,
                            valore_LogLikelihood,
                            
                            valore_interc1, signifi_interc1, 
                            valore_interc2, signifi_interc2, 
                            valore_interc3, signifi_interc3, 
                            valore_interc4, signifi_interc4,
                            
                            valore_linear1, signifi_linear1,
                            valore_linear2, signifi_linear2, 
                            valore_linear3, signifi_linear3, 
                            valore_linear4, signifi_linear4, 
                            
                            valore_quadra1, signifi_quadra1,
                            valore_quadra2, signifi_quadra2,
                            valore_quadra3, signifi_quadra3,
                            valore_quadra4, signifi_quadra4,
                            
                            valore_cubic1, signifi_cubic1,
                            valore_cubic2, signifi_cubic2,
                            valore_cubic3, signifi_cubic3,
                            valore_cubic4, signifi_cubic4,
                            
                            valore_quarti1, signifi_quarti1,
                            valore_quarti2, signifi_quarti2,
                            valore_quarti3, signifi_quarti3,
                            valore_quarti4, signifi_quarti4,
                            
                            valore_theta2, signifi_theta2,
                            valore_theta3, signifi_theta3,
                            valore_theta4, signifi_theta4)]

summary_clean <-summary_modelli[,.(model,
                                   valore_AIC, 
                                   valore_BIC_data,
                                   valore_BIC_subject,
                                   valore_LogLikelihood,
                                   
                                   signifi_interc1,
                                   signifi_linear1,
                                   signifi_quadra1,
                                   signifi_cubic1,
                                   signifi_quarti1,
                                   
                                   signifi_interc2,
                                   signifi_linear2,
                                   signifi_quadra2,
                                   signifi_cubic2,
                                   signifi_quarti2,
                                   
                                   signifi_interc3,
                                   signifi_linear3,
                                   signifi_quadra3,
                                   signifi_cubic3,
                                   signifi_quarti3,
                                   
                                   signifi_interc4,
                                   signifi_linear4,
                                   signifi_quadra4,
                                   signifi_cubic4,
                                   signifi_quarti4)]


fwrite(summary_clean, paste0(dirintermediate, "summary_4_gruppi.csv"))


#----------------------
# Polynomials selection
#----------------------

summary_clean <- fread(paste0(dirintermediate, "summary_4_gruppi.csv"))

Models <- summary_clean[, model := as.character(model)]

for (gruppo in 1:4) {
  
  Models[unlist(strsplit(as.character(model), NULL))[[gruppo]] == 1 & 
           get(paste0("signifi_interc", gruppo)) < 3, 
         grado_max_non_sign := 1]
  
  Models[unlist(strsplit(as.character(model), NULL))[[gruppo]] == 2 & 
           get(paste0("signifi_quadra", gruppo)) < 3, 
         grado_max_non_sign := 1]
  
  Models[unlist(strsplit(as.character(model), NULL))[[gruppo]] == 3 & 
           get(paste0("signifi_cubic", gruppo)) < 3, 
         grado_max_non_sign := 1]
  
  Models[unlist(strsplit(as.character(model), NULL))[[gruppo]] == 4 & 
           get(paste0("signifi_quarti", gruppo)) < 3, 
         grado_max_non_sign := 1]
  
}

Models[is.na(grado_max_non_sign), grado_max_non_sign := 0]

### Excluding models with higher degree that is non significant
model_selection <- Models[grado_max_non_sign == 0]
### Sort: higher BIC first
model_selection <- model_selection[order(-valore_BIC_data)]

### graphical representation
model_selection[, x:= seq_along(1:nrow(model_selection))]

plt <- ggplot(model_selection[x %in% c(1:6)], aes(x,valore_BIC_data ))+
  geom_point()+
  geom_label(aes(label = model),  
             color = "black", fill = "lightgray", size = 4,
             , vjust = -0.5, hjust = 0.5)+
  #xlab("Model (sorted by BIC)")+
  #ylab("BIC")+
  scale_x_continuous(labels = NULL)+
  labs(x = "Polynomials orders", y = "BIC")+
  theme_bw()+
  theme(legend.position = "none") 

png(filename = paste0(dirintermediate, "Poly_selection_v3.png"), units="in", width=6, height=4, res = 300)
plt
dev.off()

# ggsave(paste0(dirintermediate, "Poly_selection.png"), width = 4.5, height = 2, dpi = 300)

# plty <- ggplotly(plt)
# plty


fwrite(model_selection, paste0(dirintermediate, "BICs_poly_4_gruops.csv"))








### 5 groups models
# 
# for (a in 1:4) {
#   for (b in 1:4) {
#     for (c in 1:4) {
#       for (d in 1:4) {
#         for (e in 1:4) {
#           cat(a,b,c,d,e, "\n")
#           mod <- fread( paste0("/home/statistica/Scrivania/modelli/results_", a, "_", b, "_", c, "_", d, "_", e, ".txt"))
#           
#           mod <- mod[!(V2 %in% c("(1)", "y1", "", "." ))][V1 != ""]
#           
#           mod[, V3:= ifelse(gregexpr("\\*", V2)[[1]][1] == -1, 
#                             0, 
#                             lengths(gregexpr("\\*", V2))), 
#               by =seq_len(nrow(mod))]
#           
#           mod[,V2:=gsub("\\*", "", V2)]
#           
#           names(mod) <- c("parametro", "valore", "signifi")
#           
#           sum <- dcast(mod, . ~ parametro, value.var = c("valore", "signifi")) 
#           sum[, model := paste0(a,b,c,d,e)]
#           
#           list_of_models[[paste0(a,b,c,d,e)]] <- sum
#         }
#       }
#     }
#   }
# }
# 
# summary_modelli<-rbindlist(list_of_models,use.names=T,fill=T)
# 
# summary<-summary_modelli[,.(model,
#                             valore_AIC, 
#                             valore_BIC_data,
#                             valore_BIC_subject,
#                             valore_LogLikelihood,
#                             
#                             valore_interc1, signifi_interc1, 
#                             valore_interc2, signifi_interc2, 
#                             valore_interc3, signifi_interc3, 
#                             valore_interc4, signifi_interc4,
#                             valore_interc5, signifi_interc5,
#                             
#                             valore_linear1, signifi_linear1,
#                             valore_linear2, signifi_linear2, 
#                             valore_linear3, signifi_linear3, 
#                             valore_linear4, signifi_linear4, 
#                             valore_linear5, signifi_linear5,
#                             
#                             valore_quadra1, signifi_quadra1,
#                             valore_quadra2, signifi_quadra2,
#                             valore_quadra3, signifi_quadra3,
#                             valore_quadra4, signifi_quadra4,
#                             valore_quadra5, signifi_quadra5,
#                             
#                             valore_cubic1, signifi_cubic1,
#                             valore_cubic2, signifi_cubic2,
#                             valore_cubic3, signifi_cubic3,
#                             valore_cubic4, signifi_cubic4,
#                             valore_cubic5, signifi_cubic5,
#                             
#                             valore_quarti1, signifi_quarti1,
#                             valore_quarti2, signifi_quarti2,
#                             valore_quarti3, signifi_quarti3,
#                             valore_quarti4, signifi_quarti4,
#                             valore_quarti5, signifi_quarti5,
#                             
#                             valore_theta2, signifi_theta2,
#                             valore_theta3, signifi_theta3,
#                             valore_theta4, signifi_theta4,
#                             valore_theta5, signifi_theta5)]
# 
# summary_clean <-summary_modelli[,.(model,
#                                    valore_AIC, 
#                                    valore_BIC_data,
#                                    valore_BIC_subject,
#                                    valore_LogLikelihood,
#                                    
#                                    signifi_interc1,
#                                    signifi_linear1,
#                                    signifi_quadra1,
#                                    signifi_cubic1,
#                                    signifi_quarti1,
#                                    
#                                    signifi_interc2,
#                                    signifi_linear2,
#                                    signifi_quadra2,
#                                    signifi_cubic2,
#                                    signifi_quarti2,
#                                    
#                                    signifi_interc3,
#                                    signifi_linear3,
#                                    signifi_quadra3,
#                                    signifi_cubic3,
#                                    signifi_quarti3,
#                                    
#                                    signifi_interc4,
#                                    signifi_linear4,
#                                    signifi_quadra4,
#                                    signifi_cubic4,
#                                    signifi_quarti4,
#                                    
#                                    signifi_interc5,
#                                    signifi_linear5,
#                                    signifi_quadra5,
#                                    signifi_cubic5,
#                                    signifi_quarti5,
#                                    
#                                    signifi_theta2,
#                                    signifi_theta3,
#                                    signifi_theta4,
#                                    signifi_theta5)]
# 
# fwrite(summary, "/home/statistica/Scrivania/modelli/summary.csv")
