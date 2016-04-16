
## This file is not included in the package and just kept in the repository for reference


## load Josh Katz' needs() function
##source('needs.R')
## or not, as we don't believe in reinventing the packaging system for R

## load our helper functions
##source('functions.R')

## this script uses the `seasonal` package, which
## itself depends on `X13`
##Sys.setenv(X13_PATH = ".")
## the dev version of seasonal uses the dev version of x13binary to install and auto-detect this

## load packages we need
## needs(readr, dplyr, seasonal, stringr)

## this is hack -- but with the dplyr scoping these 'appear' to be global
## so we silence 'R CMD check' by actually creating variables
#handgun <- longgun <- multiple_corrected <- year <- month <- res_pop <- NULL
#guns_total <- value <- guns_total_per_1000 <- other <- multiple <- NULL
#handgun_share <- longgun_share <- NULL
#
#total.seas <- guns_total_seas <- total <- state <- month.num <- NULL

.old.analysis <- function(savePlots=FALSE, saveData=FALSE) {

    if (interactive()) {
        op <- par(ask=TRUE)
        on.exit(par(op))
    }
    
    ## read source data
    #all <- read_csv('data/ncis_bystate_bymonth_bytype.csv', na = '#N/A')
    ## will 'LazyData: yes' and the data/ directory, 'all' is known
    
    data("alldata", envir=environment())              # load package datasets
    data("poptotal", envir=environment())

    ## estimate gun sales using formula by Jurgen Brauer, published here
    ## http://www.smallarmssurvey.org/fileadmin/docs/F-Working-papers/SAS-WP14-US-Firearms-Industry.pdf
    ##
    ## note: the column `multiple_corrected` is a copy of `multiple` in which
    ## we set the checks in the "multiple" category to 0 for California
    alldata <- alldata %>% mutate(guns_sold=(handgun + longgun) * 1.1 + multiple_corrected * 2)
    ##all <- mutate(all, guns_sold=(handgun + longgun) * 1.1 + multiple_corrected * 2)

    ## let's look at the total numbers
    total <- alldata %>% state_ts('Totals', 'guns_sold')
    ##total <- state_ts(all, 'Totals', 'guns_sold')

    ## save all plots as PDF
    if (savePlots) pdf("out/plots.pdf", width=9, height=4)

    ## plot total guns sold
    plot(total / 1e6, main='Total estimated gun sales', ylab='in million', xlab='')

    ## compute seasonally adjusted gun sales
    total.seas <- total %>% seas %>% final
    ##total.seas <- final(seas(total))

    ## plot seasonally adjusted gun sales
    plot(total.seas / 1e6, main='Total estimated gun sales',
         ylab='in million', xlab='seasonal adjused')

    ## read population data
    ## pop.total <- read_csv('data/population.csv') %>%
    ## will 'LazyData: yes' and the data/ directory, 'all' is known

    poptotal <- poptotal %>%
        filter(year >= 2000) %>%
        #filter(year < 2015 | month <= 11) %>%
        with(ts(res_pop, start=c(2000,1), frequency = 12))
   
    ## normalize gun sales by population
    total.seas.pop <- total.seas / poptotal * 1000

    ## plot gun sales normalized to population
    plot(total.seas / 280726, main='Estimated gun sales per 1000',
         xlab='red = adjusted for population growth', ylab='')
    ## and add the not normalized version for comparison
    lines(total.seas.pop, col='red')

    ## create a new data frame that eventually stores all the
    ## data we need in the final piece
    out_data <- ts_to_dataframe(total, 'guns_total') %>% 
        mutate(guns_total=round(guns_total, 3))

    out_data <- out_data %>% left_join(
                                 ts_to_dataframe( total.seas.pop ) %>%
                                 mutate(guns_total_per_1000=round(value,3)) %>%
                                 select(year, month, guns_total_per_1000),
                                 by=c("year", "month")
                             )

    ## join in seasonal adjusted numbers
    temp <- ts_to_dataframe( total.seas ) %>%
        mutate(guns_total_seas=round(value,3)) %>%
        select(year, month, guns_total_seas)

    out_data <- out_data %>%
        left_join(temp, c('year', 'month')) %>%
        filter(year >= 2000)

    ## join in population adjusted numbers
    temp <- ts_to_dataframe( total.seas.pop ) %>%
        mutate(guns_total_per_1000=round(value,3)) %>%
        select(year, month, guns_total_per_1000)

    out_data <- out_data %>%
        left_join(temp, c('year', 'month'))
    
    ## create a temporary data frame for computing the
    ## handgun_share and longgun_share columns

    tmp.handguns <- alldata %>% state_ts('Totals', 'handgun') %>%
        seas %>% final %>% ts_to_dataframe('handgun')
    tmp.longguns <- alldata %>% state_ts('Totals', 'longgun') %>%
        seas %>% final %>% ts_to_dataframe('longgun')
    tmp.other <- alldata %>% state_ts('Totals', 'other') %>%
        seas %>% final %>% ts_to_dataframe('other')
    tmp.multiple <- alldata %>% state_ts('Totals', 'multiple_corrected') %>%
        seas %>% final %>% ts_to_dataframe('multiple')

    ## merge above dataframes into one
    temp <- tmp.handguns %>%
        left_join(tmp.longguns, c('year', 'month')) %>%
        left_join(tmp.other, c('year', 'month')) %>%
        left_join(tmp.multiple, c('year', 'month'))

    ## convert NAs to 0 in column other
    temp$other[is.na(temp$other)] <- 0

    ## compute the handgun/longgun share
    temp <- temp %>%
        mutate(handgun_share=round(handgun / (handgun + longgun + other + multiple * 0.5), 4)) %>%
        mutate(longgun_share=round(longgun / (handgun + longgun + other + multiple * 0.5), 4)) %>% 
        select(year, month, handgun_share, longgun_share)

    ## join into master data frame
    out_data <- out_data %>%
        left_join(temp, c('year', 'month')) %>%
        filter(year >= 2000)

    ## plot handgun/longgun 
    temp$time <- temp$year + (temp$month-1) / 12

    temp %>% with(plot(time, longgun_share, type='l', col='blue',
                       ylim=c(0.2,0.8), main='Long guns vs handguns',
                       ylab='', xlab='red = handguns, blue = long guns'))
    temp %>% with(lines(time, handgun_share, col='red'))

    ## plot percent of national for selected states 
    show_states <- c('New Jersey', 'Maryland', 'Georgia',
                     'Louisiana', 'Mississippi', 'Missouri')

    for (s in show_states) {
        s.ts <- state_data(alldata, s, total, total.seas)
    
        ## plot staate data
        plot(s.ts, main=paste(s), xlab='pct of national gun sales')
    
        ## merge with out_data
        temp <- ts_to_dataframe(s.ts) %>% mutate(value=round(value,3))
        colnames(temp) <- c('year', 'month', str_replace_all(str_to_lower(s), ' ', '_'))
        out_data <- out_data %>% left_join(temp, c('year', 'month'))
    }

    head(out_data)

    ## compute handgun sales for DC: handung * 1.1 + multiple

    dc.handgun_checks <- state_ts(alldata, 'District of Columbia', 'handgun', outer_zeros_to_na=F)
    dc.multiple <- state_ts(alldata, 'District of Columbia', 'multiple', outer_zeros_to_na=F)
    dc.handgun <- (dc.handgun_checks * 1.1 + dc.multiple + 1) %>% seas %>% final - 1
    total.handgun <- (state_ts(alldata, 'Totals', 'handgun') * 1.1 +
                      state_ts(alldata, 'Totals', 'multiple')) %>% seas %>% final
    dc.handgun.pct <- dc.handgun / total.handgun * 100000

    ## plot DC chart
    plot(dc.handgun.pct, main='Washington D.C.', xlab='sales per 100,000 national handguns')

    ## merge with out_data
    temp <- ts_to_dataframe(round(dc.handgun.pct, 1), 'dc_handguns_per_100k_national_sales')
    out_data <- out_data %>% left_join(temp, c('year', 'month'))

    ## write data file that we used in the graphics
    if (saveData) {
        write.csv(out_data, 'out/final.csv', row.names = F, quote = F)

        ## check yearly sums for overall trends
        ts_to_dataframe(total) %>%
        group_by(year) %>%
            summarize(guns_sold=sum(value)) %>% tempdat
        write.csv(tempdat, file='out/gun-sales-by-year.csv')
    }
    
    ## estimate how much more guns are sold missouri after law change
    missouri <- state_data(alldata, 'Missouri', normalize = F, adj_seasonal = F)
    missouri.avg_pre_2007 <- mean(missouri[73:84])
    missouri.avg_post_2008 <- mean(missouri[97:108])
    print(paste('Increase in monthly gun sales in Missouri =', missouri.avg_post_2008 - missouri.avg_pre_2007))

    ## save plots
    if (savePlots) dev.off()

    invisible(NULL)
}


## this is hack -- but with the dplyr scoping this 'appears' to be global
## so we silence 'R CMD check'
handgun <- longgun <- multiple_corrected <- year <- month <- res_pop <- NULL
guns_total <- value <- guns_total_per_1000 <- other <- multiple <- handgun_share <- longgun_share <- NULL
#
total.seas <- guns_total_seas <- total <- state <- month.num <- NULL
Date <- Sales <- variable <- Value <- h <- NULL

guns_total_per_1000_scaled <- dc_handguns_per_100k_national_sales <- NULL

# ' Run Statistical Analysis of Monthly Background Checks of Gun Purchase
# '
# ' @param savePlots Optional boolean switch to indicate whether plots are saved, default is \sQuote{FALSE}
# ' @param saveData Optional boolean switch to indicate whether csv files are saved, default is \sQuote{FALSE}
# '
# ' @return \code{NULL} is returned invisibly
# '                                        
# ' @author Gregor Aisch and Josh Keller wrote the R code; Dirk Eddelbuettel created and maintains the package.
# ' @seealso The NY Times article presenting this analsysi undertaken by this package is
# ' at \url{http://www.nytimes.com/interactive/2015/12/10/us/gun-sales-terrorism-obama-restrictions.html?}
# '
# ' @examples
# ' gganalysis()
.gganalysis <- function(savePlots=FALSE, saveData=FALSE) {

    if (interactive()) {
        op <- par(ask=TRUE)
        on.exit(par(op))
    }

    data("alldata", envir=environment())              # load package datasets
    data("poptotal", envir=environment())
    
    ## estimate gun sales using formula by Jurgen Brauer, published here
    ## http://www.smallarmssurvey.org/fileadmin/docs/F-Working-papers/SAS-WP14-US-Firearms-Industry.pdf
    ##
    ## note: the column `multiple_corrected` is a copy of `multiple` in which
    ## we set the checks in the "multiple" category to 0 for California
    alldata <- alldata %>% mutate(guns_sold=(handgun + longgun) * 1.1 + multiple_corrected * 2)
    ##all <- mutate(all, guns_sold=(handgun + longgun) * 1.1 + multiple_corrected * 2)

    ## let's look at the total numbers
    total <- alldata %>% state_ts('Totals', 'guns_sold')
    ##total <- state_ts(all, 'Totals', 'guns_sold')

    ## save all plots as PDF
    if (savePlots) pdf("out/ggplots.pdf", width=9, height=4)

    ## plot total guns sold
    #plot(total / 1e6, main='Total estimated gun sales', ylab='in million', xlab='')

    ztotal <- zoo(total)
    zdf <- data.frame(Date=as.Date(as.yearmon(index(ztotal))), 
                      Sales=coredata(ztotal/1e6))
    print(ggplot(data=zdf, aes(x=Date, y=Sales)) + geom_line() + scale_x_date() + 
          ggtitle("Total estimated gun sales") + ylab("in million") + xlab("") + 
              theme_bw(base_size=11))
    
    
    ## compute seasonally adjusted gun sales
    total.seas <- total %>% seas %>% final
    ##total.seas <- final(seas(total))

    ## plot seasonally adjusted gun sales
    ztotseas <- zoo(total.seas)
    ztsdf <- data.frame(Date=as.Date(as.yearmon(index(ztotseas))), 
                        Sales=coredata(ztotseas/1e6))
    
    #plot(total.seas / 1e6, main='Total estimated gun sales', ylab='in million', xlab='seasonally adjused')
    print(ggplot(data=ztsdf, aes(x=Date, y=Sales)) + geom_line() + scale_x_date() +
        ggtitle("Total estimated gun sales") + ylab("in million") + 
            xlab("seasonally adjusted") + theme_bw(base_size=11))
    ## read population data
    ##pop.total <- read_csv('data/population.csv') %>%
    ## will 'LazyData: yes' and the data/ directory, 'all' is known

    poptotal <- poptotal %>%
        filter(year >= 2000) %>%
        #filter(year < 2015 | month <= 11) %>%
        with(ts(res_pop, start=c(2000,1), frequency = 12))
   
    ## normalize gun sales by population
    total.seas.pop <- total.seas / poptotal * 1000

    ## plot gun sales normalized to population
    #plot(total.seas / 280726, main='Estimated gun sales per 1000',
    #     xlab='red = adjusted for population growth', ylab='')
    ## and add the not normalized version for comparison
    #lines(total.seas.pop, col='red')

    ztotseaspop <- zoo(total.seas.pop)
    ztspdf <- data.frame(Date=as.Date(as.yearmon(index(ztotseaspop))),
                         Sales=coredata(ztotseas/280726),
                         SalesAdjusted=coredata(ztotseaspop))
    dt <- data.table::data.table(ztspdf)
    ndt <- data.table::melt(dt, measure.vars=c("Sales", "SalesAdjusted"), id.var="Date")
    print(ggplot(data=ndt, aes(x=Date, y=value)) + geom_line(aes(colour=variable)) + scale_x_date() + 
        ggtitle("Estimated gun sales per 1000") + xlab("red = adjusted for population growth") +
        ylab("") + scale_colour_manual(values=c("black", "red")) + 
            theme_bw(base_size=11) +
            theme(legend.title=element_blank(), legend.position="bottom")) 

    ## create a new data frame that eventually stores all the
    ## data we need in the final piece
    out_data <- ts_to_dataframe(total, 'guns_total') %>% 
        mutate(guns_total=round(guns_total, 3))

    out_data <- out_data %>% left_join(
                                 ts_to_dataframe( total.seas.pop ) %>%
                                 mutate(guns_total_per_1000=round(value,3)) %>%
                                 select(year, month, guns_total_per_1000),
                                 by=c("year", "month")
                             )

    ## join in seasonal adjusted numbers
    temp <- ts_to_dataframe( total.seas ) %>%
        mutate(guns_total_seas=round(value,3)) %>%
        select(year, month, guns_total_seas)

    out_data <- out_data %>%
        left_join(temp, c('year', 'month')) %>%
        filter(year >= 2000)

    ## join in population adjusted numbers
    temp <- ts_to_dataframe( total.seas.pop ) %>%
        mutate(guns_total_per_1000=round(value,3)) %>%
        select(year, month, guns_total_per_1000)

    out_data <- out_data %>%
        left_join(temp, c('year', 'month'))
    
    ## create a temporary data frame for computing the
    ## handgun_share and longgun_share columns

    tmp.handguns <- alldata %>% state_ts('Totals', 'handgun') %>%
        seas %>% final %>% ts_to_dataframe('handgun')
    tmp.longguns <- alldata %>% state_ts('Totals', 'longgun') %>%
        seas %>% final %>% ts_to_dataframe('longgun')
    tmp.other <- alldata %>% state_ts('Totals', 'other') %>%
        seas %>% final %>% ts_to_dataframe('other')
    tmp.multiple <- alldata %>% state_ts('Totals', 'multiple_corrected') %>%
        seas %>% final %>% ts_to_dataframe('multiple')

    ## merge above dataframes into one
    temp <- tmp.handguns %>%
        left_join(tmp.longguns, c('year', 'month')) %>%
        left_join(tmp.other, c('year', 'month')) %>%
        left_join(tmp.multiple, c('year', 'month'))

    ## convert NAs to 0 in column other
    temp$other[is.na(temp$other)] <- 0

    ## compute the handgun/longgun share
    temp <- temp %>%
        mutate(handgun_share=round(handgun / (handgun + longgun + other + multiple * 0.5), 4)) %>%
        mutate(longgun_share=round(longgun / (handgun + longgun + other + multiple * 0.5), 4)) %>% 
        select(year, month, handgun_share, longgun_share)

    ## join into master data frame
    out_data <- out_data %>%
        left_join(temp, c('year', 'month')) %>%
        filter(year >= 2000)

    ## plot handgun/longgun 
    temp$time <- temp$year + (temp$month-1) / 12

    #temp %>% with(plot(time, longgun_share, type='l', col='blue',
    #                   ylim=c(0.2,0.8), main='Long guns vs handguns',
    #                   ylab='', xlab='red = handguns, blue = long guns'))
    #temp %>% with(lines(time, handgun_share, col='red'))

    tt <- data.table::data.table(temp)
    ntt <- data.table::melt(tt, measure.vars=c("handgun_share", "longgun_share"), id.var="time")
    ntt[,time := as.Date(as.yearmon(time))]
    print(ggplot(data=ntt, aes(x=time, y=value)) + geom_line(aes(colour=variable)) + scale_x_date() +
          ggtitle("Long guns vs handguns") +
            xlab("red = handguns, blue = long guns") + ylab("") +
            theme_bw(base_size=11) +
            theme(legend.position="none") + #theme(legend.title=element_blank()) +
            scale_colour_manual(values=c("blue", "red")))

    
    ## plot percent of national for selected states 
    show_states <- c('New Jersey', 'Maryland', 'Georgia',
                     'Louisiana', 'Mississippi', 'Missouri')

    for (s in show_states) {
        s.ts <- state_data(alldata, s, total, total.seas)
    
        ## plot staate data
        #plot(s.ts, main=paste(s), xlab='pct of national gun sales')
    
        ## merge with out_data
        temp <- ts_to_dataframe(s.ts) %>% mutate(value=round(value,3))
        colnames(temp) <- c('year', 'month', str_replace_all(str_to_lower(s), ' ', '_'))
        out_data <- out_data %>% left_join(temp, c('year', 'month'))
    }

    head(out_data)

    ## do it again, but less crazy
    rl <- lapply(show_states, function(s) {
        s.ts <- as.zoo(state_data(alldata, s, total, total.seas))
        #print(head(s.ts))
        zts <- zoo(coredata(s.ts), order.by=as.Date(as.yearmon(index(s.ts))))
        #nd <- cbind(nd, zts)
    })
    nd <- do.call(merge, rl)
    colnames(nd) <- show_states
    ndf <- data.table::data.table(Date=index(nd), coredata(nd))
    pdf <- data.table::melt(ndf, id.vars="Date")
    
    print(ggplot(data=pdf, aes(x=Date, y=value)) + geom_line() + 
        facet_wrap( ~ variable) + 
        theme_bw(base_size=11) +
        xlab("Percentage of National Sales") + ylab(""))
    
    ## compute handgun sales for DC: handung * 1.1 + multiple

    dc.handgun_checks <- state_ts(alldata, 'District of Columbia', 'handgun', outer_zeros_to_na=F)
    dc.multiple <- state_ts(alldata, 'District of Columbia', 'multiple', outer_zeros_to_na=F)
    dc.handgun <- (dc.handgun_checks * 1.1 + dc.multiple + 1) %>% seas %>% final - 1
    total.handgun <- (state_ts(alldata, 'Totals', 'handgun') * 1.1 +
                      state_ts(alldata, 'Totals', 'multiple')) %>% seas %>% final
    dc.handgun.pct <- dc.handgun / total.handgun * 100000

    ## plot DC chart
    #plot(dc.handgun.pct, main='Washington D.C.', xlab='sales per 100,000 national handguns')

    dcz <- as.zoo(dc.handgun.pct)
    dcdf <- data.frame(Date=as.Date(as.yearmon(index(dcz))), Value=coredata(dcz))
    print(ggplot(data=dcdf, aes(x=Date, y=Value)) + geom_line() +
                 theme_bw(base_size=11) +
                 ggtitle("Washington D.C.") + xlab("Sales per 100,000 national handguns"))
    
    
    ## merge with out_data
    temp <- ts_to_dataframe(round(dc.handgun.pct, 1), 'dc_handguns_per_100k_national_sales')
    out_data <- out_data %>% left_join(temp, c('year', 'month'))

    ## write data file that we used in the graphics
    if (saveData) {
        write.csv(out_data, 'out/final.csv', row.names = F, quote = F)

        ## check yearly sums for overall trends
        ts_to_dataframe(total) %>%
        group_by(year) %>%
            summarize(guns_sold=sum(value)) %>%
            write_csv('out/gun-sales-by-year.csv')
    }
    
    ## estimate how much more guns are sold missouri after law change
    missouri <- state_data(alldata, 'Missouri', normalize = F, adj_seasonal = F)
    missouri.avg_pre_2007 <- mean(missouri[73:84])
    missouri.avg_post_2008 <- mean(missouri[97:108])
    #print(paste('Increase in monthly gun sales in Missouri =', missouri.avg_post_2008 - missouri.avg_pre_2007))

    ## save plots
    if (savePlots) dev.off()

    invisible(NULL)
}
