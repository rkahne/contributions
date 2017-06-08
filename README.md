# Contributions Shiny App
## Visualizing Kentucky Campaign Contributions

* `App.R` creates a dashboard was built using data from the [Kentucky Registry for Election Finance](http://www.kref.state.ky.us/krefsearch/) database.  
* A copy of the data driving this project is available at [data.world](https://data.world/rkahne/kref-contributions-2014-may-2017)
	* Please see the notes at the above data.world link for how I processed the data.
* In this app, I've filtered out any donations made from people including the name `PRIMARY`
	* This elminiates transfers between primary election accounts to general election accounts.
	* I don't think this is the case, but this filtering might have caused some oddness in the data.
	* If you notice something weird, let me know: `rkahne [at] gmail.com`.
* Candidates with >1000 contributors do not have treemaps.  They bog down the loading process too much, and aren't that informative at that level anyway.
* `data_setup.R` is the script used to pull all the data together for use in the app.  The data was access in six month chunks from the KREF website.  That is the amount of data that could reliably be retreived from that site.
