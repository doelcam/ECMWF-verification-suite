# ECMWF-verification-suite

This gitHub repository can be found at:
https://github.com/doelcam/ECMWF-verification-suite.git

Its purpose is to collate the verification scripts used for automatic seasonal hindcast verification used at ECMWF, specifically the version worked on me (Dave MacLeod) at Oxford University 2013-2016. Changes from the versions used by other users at the time are mainly so that the suite works for external users of the ECMWF computing systems.

Other changes involve:
- Land surface fields (soil moisture and soil temperature) use a research version of ERA-Land reanalysis for their reference, rather than ERA-Interim.
- A new probabilistic score, Ignorance, has been added to cal_rel_reg and cal_rel_map.

I have tried to comment everything as much as possible, my email is at the bottom if (when) it doesn't work for you - but n.b., these scripts come with absolutely no guarantee.

Contents of the gitHub respository

documentation
-------------
- ECMWF_User_guide.pdf : start here if you've never used the ECMWF computing systems
- Using_the_ECMWF_seasonal_verification_suite.pdf : instructions on how to use the suite
	
	
ecmwfVerificationSuite - containing all the code for running the suite, plus some extras
-------------
scores : directory containing all the suite source code
scores_example.def : an example suite definition file
subSuite.sh/resubSuite.sh : helper bash scripts for submitting definition files
copyData_ECFS-remote.sh : a helper bash script to transfer verification data from ECFS where it is archived to a remote location
scores_old.tar : an archive containing the original suite code given to me in 2013 before I made any changes - this is just so I have archived it somewhere, safely ignore this!

remotePlottingScripts - containing all the NCL scripts I have written which make nice plots based on the verification suite output
-------------
untar_r.sh : helper bash script which unpacks the archived verification suite data when it is copied to a location remote from ECMWF
plotScores : directory containing all the plotting scripts
	functions: helper functions I've written to do miscellaneous tasks related to plotting the verification suite
	plottingScripts: NCL plotting scripts. Set up setupInfo.ncl for your specific verification exercise, then you plot with the scripts Plot*.ncl. Hopefully these just work(!) and you only need to modify the loop iterators.
	
	Alternatively feel free to write your own plotting if you don't like NCL / my plots - the output of the suite is not incredibly complex, e.g. for maps it is just grib files with a single lat/lon field in each. But some scores (e.g. reliability diagrams) are a bit more complicated and perhaps the work I've already done here can help. If these don't work as expected or you get some errors (you will probably get errors) then maybe I can help, feel free to contact me.
	
	Dave MacLeod, University of Oxford 24th March 2016
	macleod at atm.ox.ac.uk
	dave.a.macleod at gmail.com
