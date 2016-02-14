state_ts <- function(data, state_ts, column='guns_sold', outer_zeros_to_na=TRUE) {
   d <- data %>%
        filter(state == state_ts & (year >= 2000)) %>%
        arrange(year, month.num) %>%
        select_('year', month='month.num', value=column)
   # d$value[d$value == 0] <- NA
   series <- ts(d$value, start=c(d$year[1],d$month[1]), end=c(last(d$year), last(d$month)), frequency = 12)
   if (outer_zeros_to_na) series <- replace_outer_zeros(series)
   series
}

ts_to_dataframe <- function(t, value.name='value') {
    df <- data.frame(year=as.numeric(floor(time(t))),
               month=as.numeric(round(1+(time(t) - floor(time(t))) * 12)),
               value=as.matrix(t))
    colnames(df) <- c('year', 'month', value.name)
    df
}

state_data <- function(all, state_, total, total.seas,
                       normalize=T, adj_seasonal=T, column='guns_sold') {
    state <- state_ts(all, state_, column)
    if (adj_seasonal) {
        pct <- seas(state) %>% final()
        if (normalize) pct <- pct / total.seas * 100
    } else {
        if (normalize) pct <- state / total * 100
        else pct <- state
    }
    pct
}


replace_outer_zeros <- function(x) {
    for(i in 1:length(x)){        
        if(x[i] != 0) break
        if(x[i] == 0) x[i] <- NA
    }
    for(i in length(x):1){        
        if(x[i] != 0) break
        if(x[i] == 0) x[i] <- NA
    }
    x
}

# d <- state_ts(all, 'Louisiana', 'permit')

df2ts <- function(df, col) {
    
    stopifnot(inherits(df, "data.frame"),
              "year" %in% colnames(df),
              "month" %in% colnames(df),
              col %in% colnames(df))
    
    ts(df[, col], start=c(df[1,"year"], df[1,"month"]), frequency=12)
}
