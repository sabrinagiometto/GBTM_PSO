import delimited "C:\Users\giorg\GitHub\GBTM_PSO-repo_DATA\input\data_covs.csv"

traj, var(ad_1-ad_12) indep (t_1-t_12) model(logit) order (2 3 2 3)  risk(sesso young young_adult senior etanercept infliximab secukinumab ustekinumab non_bio_drugs glucorticoidi) refgroup(3)

trajplot, xtitle(3 months' period) ytitle(Adherence) ci


export delimited using C:\Users\giorg\GitHub\GBTM_PSO-repo_DATA\output\cohort_final.csv
*file C:\Users\giorg\GitHub\GBTM_PSO-repo_DATA\output\cohort_final.csv save d
