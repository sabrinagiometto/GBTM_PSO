# Load Stata results
summo <- fread(paste0(dirintermediate, "summary_models_poly_2.csv"))

# Preprocessing for plot
summo <- summo[2:6]
names(summo) <- c("Method", "2", "3", "4", "5", "6", "7")

summo <- data.table(summo)

summo[, Method:=ifelse(Method=="BIC(N=21683)","BIC (N=21,683)",
              ifelse(Method=="BIC(N=1838)","BIC (N=1,838)",Method))]

summo_melted <- melt(summo, id.vars = "Method")

# plot: graphical representation of AIC & BIC
plt <- ggplot(summo_melted[Method %in% c("AIC", "BIC (N=21,683)", "BIC (N=1,838)" )], aes(x = variable, y = value, group = Method, col = Method))+
  geom_point(size = 3)+
  geom_line(linewidth = 1.0)+
  labs(x = "Number of groups", y = "Value")+
  theme_bw()+
  theme(legend.position = "right") 


png(filename = paste0(dirintermediate, "N_group_selection_v2.png"), units="in", width=10, height=5, res = 300)

plt

dev.off()
# ggsave(paste0(dirintermediate, "N_group_selection_v2.png"), width = 3, height = 2, dpi = 300)


plty <- ggplotly(plt)

plty
