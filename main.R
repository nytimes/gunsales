
# load Josh Katz' needs() function
source('needs.R')

# load our helper functions
source('functions.R')

# this script uses the `seasonal` package, which
# itself depends on `X13`
Sys.setenv(X13_PATH = ".")

# load packages we need
needs(readr, dplyr, seasonal, stringr)

# read source data
all <- read_csv('data/ncis_bystate_bymonth_bytype.csv', na = '#N/A')

# estimate gun sales using formula by Jurgen Brauer, published here
# http://www.smallarmssurvey.org/fileadmin/docs/F-Working-papers/SAS-WP14-US-Firearms-Industry.pdf
#
# note: the column `multiple_corrected` is a copy of `multiple` in which
# we set the checks in the "multiple" category to 0 for California
all <- all %>% mutate(guns_sold=(handgun + longgun) * 1.1 + multiple_corrected * 2)

# let's look at the total numbers
total <- all %>% state_ts('Totals', 'guns_sold')

# save all plots as PDF
pdf('out/plots.pdf', width=9, height=4)

# plot total guns sold
plot(total / 1e6, main='Total estimated gun sales', ylab='in million', xlab='')

# compute seasonally adjusted gun sales
total.seas <- total %>% seas %>% final

# plot seasonally adjusted gun sales
plot(total.seas / 1e6, main='Total estimated gun sales', ylab='in million', xlab='seasonal adjused')

# read population data
pop.total <- read_csv('data/population.csv') %>%
    filter(year >= 2000) %>%
    filter(year < 2015 | month <= 11) %>%
    with(ts(res_pop, start=c(2000,1), frequency = 12))

# normalize gun sales by population
total.seas.pop <- total.seas / pop.total * 1000

# plot gun sales normalized to population
plot(total.seas / 280726, main='Estimated gun sales per 1000', xlab='red = adjusted for population growth', ylab='')
# and add the not normalized version for comparison
lines(total.seas.pop, col='red')

# create a new data frame that eventually stores all the
# data we need in the final piece
out_data <- ts_to_dataframe(total, 'guns_total') %>% 
    mutate(guns_total=round(guns_total, 3))

out_data <- out_data %>% left_join(
    ts_to_dataframe( total.seas.pop ) %>%
        mutate(guns_total_per_1000=round(value,3)) %>%
        select(year, month, guns_total_per_1000)
    )

# create a temporary data frame for computing the
# handgun_share and longgun_share columns

tmp.handguns <- all %>% state_ts('Totals', 'handgun') %>%
    seas %>% final %>% ts_to_dataframe('handgun')
tmp.longguns <- all %>% state_ts('Totals', 'longgun') %>%
    seas %>% final %>% ts_to_dataframe('longgun')
tmp.other <- all %>% state_ts('Totals', 'other') %>%
    seas %>% final %>% ts_to_dataframe('other')
tmp.multiple <- all %>% state_ts('Totals', 'multiple_corrected') %>%
    seas %>% final %>% ts_to_dataframe('multiple')

# merge above dataframes into one
temp <- tmp.handguns %>%
    left_join(tmp.longguns, c('year', 'month')) %>%
    left_join(tmp.other, c('year', 'month')) %>%
    left_join(tmp.multiple, c('year', 'month'))

# convert NAs to 0 in column other
temp$other[is.na(temp$other)] <- 0

# compute the handgun/longgun share
temp <- temp %>%
    mutate(handgun_share=round(handgun / (handgun + longgun + other + multiple * 0.5), 4)) %>%
    mutate(longgun_share=round(longgun / (handgun + longgun + other + multiple * 0.5), 4)) %>% 
    select(year, month, handgun_share, longgun_share)

# join into master data frame
out_data <- out_data %>%
    left_join(temp, c('year', 'month')) %>%
    filter(year >= 2000)

# plot handgun/longgun 
temp$time <- temp$year + (temp$month-1) / 12

temp %>% with(plot(time, longgun_share, type='l', col='blue',
                   ylim=c(0.2,0.8), main='Long guns vs handguns',
                   ylab='', xlab='red = handguns, blue = long guns'))
temp %>% with(lines(time, handgun_share, col='red'))

# plot percent of national for selected states 
show_states <- c('New Jersey', 'Maryland', 'Georgia',
                 'Louisiana', 'Mississippi', 'Missouri')

for (s in show_states) {
    s.ts <- state_data(s)
    
    # plot staate data
    plot(s.ts, main=paste(s), xlab='pct of national gun sales')
    
    # merge with out_data
    temp <- ts_to_dataframe(s.ts) %>% mutate(value=round(value,3))
    colnames(temp) <- c('year', 'month', str_replace_all(str_to_lower(s), ' ', '_'))
    out_data <- out_data %>% left_join(temp, c('year', 'month'))
}

head(out_data)

# compute handgun sales for DC: handung * 1.1 + multiple

dc.handgun_checks <- state_ts(all, 'District of Columbia', 'handgun', outer_zeros_to_na=F)
dc.multiple <- state_ts(all, 'District of Columbia', 'multiple', outer_zeros_to_na=F)
dc.handgun <- (dc.handgun_checks * 1.1 + dc.multiple + 1) %>% seas %>% final - 1
total.handgun <- (state_ts(all, 'Totals', 'handgun') * 1.1 + state_ts(all, 'Totals', 'multiple')) %>% seas %>% final
dc.handgun.pct <- dc.handgun / total.handgun * 100000

# plot DC chart
plot(dc.handgun.pct, main='Washington D.C.', xlab='sales per 100,000 national handguns')

# merge with out_data
temp <- ts_to_dataframe(round(dc.handgun.pct, 1), 'dc_handguns_per_100k_national_sales')
out_data <- out_data %>% left_join(temp, c('year', 'month'))

# write data file that we used in the graphics
write.csv(out_data, 'out/final.csv', row.names = F, quote = F)

# check yearly sums for overall trends
ts_to_dataframe(total) %>%
    group_by(year) %>%
    summarize(guns_sold=sum(value)) %>%
    write_csv('out/gun-sales-by-year.csv')

# estimate how much more guns are sold missouri after law change
missouri <- state_data('Missouri', normalize = F, adj_seasonal = F)
missouri.avg_pre_2007 <- mean(missouri[73:84])
missouri.avg_post_2008 <- mean(missouri[97:108])
print(paste('Increase in monthly gun sales in Missouri =', missouri.avg_post_2008 - missouri.avg_pre_2007))

# save plots
dev.off()

