# Analysis of NICS gun purchase background checks

Statistical analysis of monthly background checks of gun purchases for the New York Times story [What Drives Gun Sales: Terrorism,
Obama and Calls for Restrictions](http://www.nytimes.com/interactive/2015/12/10/us/gun-sales-terrorism-obama-restrictions.html?).

### Pre-requisites

This script uses the R package [seasonal](https://cran.r-project.org/web/packages/seasonal/vignettes/seas.pdf) for the seasonal adjustments, which itself uses a program called [X-13ARIMA-SEATS](). 
Windows, OS X and Linux binaries for this program are installed by the R package [x13binary](https://github.com/x13org/x13binary).

### Running the script

Run the script from the command-line:

```sh
$ Rscript main.R
```

or step-by-step in your favorite R environment as eg
[RStudio](http://www.rstudio.com) or [ESS](http://ess.r-project.org).

The script creates the [PDF plots](https://github.com/NYTimes/gun-sales/blob/master/out/plots.pdf) and [CSV](https://github.com/NYTimes/gun-sales/blob/master/out/final.csv) [files](https://github.com/NYTimes/gun-sales/blob/master/out/gun-sales-by-year.csv) in the `out/` folder.

For more explanation of what the script is doing, please read through the comments in [main.R](https://github.com/NYTimes/gun-sales/blob/master/main.R).

### Data issues

The source data comes from the [FBI's National Instant Criminal Background Check System](https://www.fbi.gov/about-us/cjis/nics), and was converted from the original [PDF format](https://www.fbi.gov/about-us/cjis/nics/reports/nics_firearm_checks_-_month_year_by_state_type.pdf) to CSV using [Tabula](http://tabula.technology/).

BuzzFeed also released the same dataset on [Github](https://github.com/BuzzFeedNews/nics-firearm-background-checks/) last week.

#### Getting gun sales estimates from background checks

To convert background checks into estimated sales, we relied on a method suggested in the [Small Arms Survey](http://www.smallarmssurvey.org/fileadmin/docs/F-Working-papers/SAS-WP14-US-Firearms-Industry.pdf) by Jurgen Brauer, a professor at Georgia Regents University. Each long gun and handgun check was counted as 1.1 sales. Each multiple-gun check was counted as two sales. Permit checks and other types of checks were omitted. The multiplier is an estimate based on Mr. Brauer's interviews with gun shop owners.

Note: In our [computation](https://github.com/NYTimes/gun-sales/blob/master/main.R#L20), we excluded background checks for the "multiple" category in California because they followed an unusual pattern that did not match California gun sales data.
