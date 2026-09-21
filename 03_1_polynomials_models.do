import delimited DIR
*replace DIR with the output directory

 *4 groups 
forvalues a = 1/4 {
	forvalues b = 1/4 {
		forvalues c = 1/4 {
			forvalues d = 1/4 {
    traj, var(ad_1-ad_12) indep (t_1-t_12) model(logit) order (`a' `b' `c' `d')

    outreg2 using DIR_`a'_`b'_`c'_`d'.txt, replace ctitle(Model for `var')
	*replace DIR with the output directory
    }
   }
  }
 }
}









