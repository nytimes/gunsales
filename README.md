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

The script creates the PDF plots and CSV files in the `out/` folder.

For more explanation of what the script is doing, please read through the comments in [main.R](https://github.com/NYTimes/gun-sales/blob/master/main.R).
