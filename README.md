# Analysis of NICS gun purchase background checks

[![Build Status](https://travis-ci.org/NYTimes/gunsales.svg)](https://travis-ci.org/NYTimes/gunsales) 
[![License](http://img.shields.io/badge/license-Apache%20%28=%202%29-brightgreen.svg?style=flat)](http://www.apache.org/licenses/LICENSE-2.0) 
[![CRAN](http://www.r-pkg.org/badges/version/gunsales)](https://cran.r-project.org/package=gunsales) 
[![Downloads](http://cranlogs.r-pkg.org/badges/gunsales?color=brightgreen)](http://www.r-pkg.org/pkg/gunsales)

Statistical analysis of monthly background checks of gun purchases for the New York Times story [What Drives Gun Sales: Terrorism,
Obama and Calls for Restrictions](http://www.nytimes.com/interactive/2015/12/10/us/gun-sales-terrorism-obama-restrictions.html?).

### Pre-requisites

This package depends on the R package [seasonal](https://cran.r-project.org/package=seasonal/vignettes/seas.pdf) for the seasonal adjustments, which itself uses a program called [X-13ARIMA-SEATS](https://www.census.gov/srd/www/x13as/).
Windows, OS X and Linux binaries for this program are installed by the R package [x13binary](https://github.com/x13org/x13binary).

Both packages are now on CRAN and can be installed along with the other dependencies via

```r
> install.packages("gunsales")
```


### Running the main function

Once the package has loaded, run this in an R shell:

```r
> library(gunsales)
> df <- analysis()
```

to create a single dataframe containing the results. The dataframe can be
visualized via

```r
> plot_gunsales(df)    
> ggplot_gunsales(df)
```

to create, respectively, plots via R base or
[ggplot2](https://github.com/hadley/ggplot2). Options to save the output in the `out/` folder exist. The resulting [ggplot2](https://github.com/hadley/ggplot2) charts are shown below:

![Total Estimated Gun Sales](https://raw.githubusercontent.com/NYTimes/gunsales/master/out/ggplot_total.png)

![Total Estimated Gun Sales, Seasonally Adjusted](https://raw.githubusercontent.com/NYTimes/gunsales/master/out/ggplot_total_seasadj.png)

![Total Estimated Gun Sales, Population-Growth Adjusted](https://raw.githubusercontent.com/NYTimes/gunsales/master/out/ggplot_total_popadj.png)

![Handguns vs Longguns](https://raw.githubusercontent.com/NYTimes/gunsales/master/out/ggplot_hand_vs_long_guns.png)

![Six States](https://raw.githubusercontent.com/NYTimes/gunsales/master/out/ggplot_six_states.png)

![DC](https://raw.githubusercontent.com/NYTimes/gunsales/master/out/ggplot_dc.png)


### Data issues

The source data comes from the [FBI's National Instant Criminal Background Check System](https://www.fbi.gov/about-us/cjis/nics), and was converted from the original [PDF format](https://www.fbi.gov/file-repository/nics_firearm_checks_-_month_year_by_state_type.pdf) to CSV using [Tabula](http://tabula.technology/).

BuzzFeed also released the same dataset on [Github](https://github.com/BuzzFeedNews/nics-firearm-background-checks/) last week.

#### Getting gun sales estimates from background checks

To convert background checks into estimated sales, we relied on a method suggested in the [Small Arms Survey](http://www.smallarmssurvey.org/fileadmin/docs/F-Working-papers/SAS-WP14-US-Firearms-Industry.pdf) by Jurgen Brauer, a professor at Georgia Regents University. Each long gun and handgun check was counted as 1.1 sales. Each multiple-gun check was counted as two sales. Permit checks and other types of checks were omitted. The multiplier is an estimate based on Mr. Brauer's interviews with gun shop owners.

Note: In our computation, we excluded background checks for the "multiple" category in California because they followed an unusual pattern that did not match California gun sales data.
