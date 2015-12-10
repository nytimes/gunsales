# Analysis of NCIS gun purchase background checks

Statistical analysis of monthly background checks of gun purchases for the New York Times story [What Drives Gun Sales: Terrorism,
Obama and Calls for Restrictions](http://www.nytimes.com/interactive/2015/12/10/us/gun-sales-terrorism-obama-restrictions.html?).

### Pre-requisites

This script uses the R package [seasonal](https://cran.r-project.org/web/packages/seasonal/vignettes/seas.pdf) for the seasonal adjustments, which itself uses a program called [X-13ARIMA-SEATS](). Binaries for this program are available for Windows only, but there is a very helpful [compilation guide for Mac OS X](https://github.com/christophsax/seasonal/wiki/Compiling-X-13ARIMA-SEATS-from-Source-for-OS-X). If that doesn't work, try downloading the program [from here](https://gist.github.com/gka/3b200d57b0db14d058f3).

### Running the script

You can either open `main.R`  in RStudio and run it line by line or run the entire script as a whole on the command-line:

```sh
$ R --no-save < main.R
```

The script creates the [PDF plots](https://github.com/NYTimes/gun-sales/blob/master/out/plots.pdf) and [CSV](https://github.com/NYTimes/gun-sales/blob/master/out/final.csv) [files](https://github.com/NYTimes/gun-sales/blob/master/out/gun-sales-by-year.csv) in the `out/` folder.

For more explanation of what the script is doing, please read through the comments in [main.R](https://github.com/NYTimes/gun-sales/blob/master/main.R).

### Data issues

The source data comes from the [FBI's National Instant Criminal Background Check System](https://www.fbi.gov/about-us/cjis/nics), and was converted from the original [PDF format](https://www.fbi.gov/about-us/cjis/nics/reports/nics_firearm_checks_-_month_year_by_state_type.pdf) to CSV using [Tabula](http://tabula.technology/).

BuzzFeed also released the same dataset on [Github](https://github.com/BuzzFeedNews/nics-firearm-background-checks/) last week.

#### Getting gun sales estimates from background checks

We used a method suggested by [research published](http://www.smallarmssurvey.org/fileadmin/docs/F-Working-papers/SAS-WP14-US-Firearms-Industry.pdf) in the Small Arms Survey, which uses an estimate of the number of sales for each background check. Each long gun and hand gun check was counted for 1.1 sales. Each multiple gun check was counted for 2 sales. Permit checks and other types of checks were omitted. The multiplier is only an estimate, but which was described by other experts as reasonable.
