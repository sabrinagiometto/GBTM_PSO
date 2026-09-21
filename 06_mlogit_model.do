import delimited "C:\Users\giorg\GitHub\GBTM_PSO-repo_DATA\output\cohort_final.csv"

mlogit _traj_group sesso  eta  adalimumab infliximab ustekinumab secukinumab visite acitretin antipso_topic cyclosporin retinoids glucorticoidi antif_non_steroidei

mlogit, rrr