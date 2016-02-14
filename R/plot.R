plot_gunsales <- function(df, savePlots=FALSE, saveData=FALSE) {

    if (interactive()) {
        op <- par(ask=TRUE)
        on.exit(par(op))
    }
    
    ## save all plots as PDF
    if (savePlots) pdf("out/plots.pdf", width=9, height=4)

    ## plot total guns sold
    plot(df2ts(df, "guns_total")/1e6, main="Total estimated gun sales",
               ylab="in million", xlab="")

    ## plot seasonally adjusted gun sales
    plot(df2ts(df, "guns_total_seas")/1e6, main="Total estimated gun sales",
         ylab="in million", xlab="seasonal adjused")

    ## plot gun sales normalized to population
    plot(df2ts(df, "guns_total_per_1000_scaled"), main="Estimated gun sales per 1000",
         xlab="red = adjusted for population growth", ylab="")
    ## and add the not normalized version for comparison
    lines(df2ts(df, "guns_total_per_1000"), col="red")

    ## plot handgun/longgun 
    plot(df2ts(df, "longgun_share"), col="blue",
         ylim=c(0.2,0.8), main="Long guns vs handguns",
         ylab="", xlab="red = handguns, blue = long guns")
    lines(df2ts(df, "handgun_share"), col="red")


    ## plot percent of national for selected states 
    show_states <- c('New Jersey', 'Maryland', 'Georgia',
                     'Louisiana', 'Mississippi', 'Missouri')
    selected <- gsub(" ", "_", tolower(show_states))
    ## plot staate data
    for (s in seq_along(show_states)) {
        plot(df2ts(df, selected[s]), main=paste(show_states[s]),
             xlab="pct of national gun sales", ylab="")
    }

    ## plot DC chart
    plot(df2ts(df, "dc_handguns_per_100k_national_sales"),
         main="Washington D.C.", xlab="sales per 100,000 national handguns", ylab="")

    ## save plots
    if (savePlots) dev.off()

    invisible(NULL)
    
}
