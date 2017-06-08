# How This Works
* This dashboard was built using data from the [Kentucky Registry for Election Finance](http://www.kref.state.ky.us/krefsearch/) database.  
* A copy of this data is available at [data.world](https://data.world/rkahne/kref-contributions-2014-may-2017)
	* Please see the notes at the above data.world link for how I processed the data.
* In this app, I've filtered out any donations made from people including the name `PRIMARY`
	* This elminiates transfers between primary election accounts to general election accounts.
	* I don't think this is the case, but this filtering might have caused some oddness in the data.
	* If you notice something weird, let me know: `rkahne [at] gmail.com`.
* Candidates with >1000 contributors do not have treemaps.  They bog down the loading process too much, and aren't that informative at that level anyway.
