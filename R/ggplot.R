
## variant of R/main.R analysis() using ggplot


##' This function creates (ggplot2) plots for gunsales analysis.
##'
##' In interactive mode, plot display is paused and the user has to
##' advance by pressing the Return key.
##' @title ggplot2 plots for gunsales analysis
##' @param df A \code{data.frame} as prepared by the
##' \code{\link{analysis}} functions.
##' @param savePlots A boolean toggle to indicate if the plots are to
##' be saved in the \code{out/} directory as a single pdf file,
##' with a default of \code{FALSE}.
##' @param savePNG A boolean toggle to indicate if the plots are to
##' be saved in the \code{out/} directory as individual png files,
##' with a default of \code{FALSE}.
##' @return \code{NULL} is returned invisibly.
##' @author Gregor Aisch and Josh Keller wrote the R code; Dirk
##' Eddelbuettel created and maintains the package.
##' @seealso The NY Times article presenting this analsysi undertaken
##' by this package is at
##' \url{http://www.nytimes.com/interactive/2015/12/10/us/gun-sales-terrorism-obama-restrictions.html?}
##' @examples
##' \dontrun{
##'   gs <- analysis()
##'   ggplot_gunsales(gs)
##' }
ggplot_gunsales <- function(df, savePlots=FALSE, savePNG=FALSE) {

    if (interactive()) {
        op <- par(ask=TRUE)
        on.exit(par(op))
    }

    ## create a Date object suitable for plotting; as.yearmon from zoo
    df$Date <- as.Date(as.yearmon(df$year + (df$month-1)/12))
    
    ## save all plots as PDF
    if (savePlots) pdf("out/ggplots.pdf", width=9, height=4)

    theme_set(theme_bw(base_size=11))

    ## plot total guns sold
    if (savePNG) png("out/ggplot_total.png", 640, 480)
    print(ggplot(data=df, aes(x=Date, y=guns_total/1e6)) + geom_line() + scale_x_date() + 
          ggtitle("Total estimated gun sales") + ylab("in million") + xlab("")
          )
    if (savePNG) dev.off()
    
    if (savePNG) png("out/ggplot_total_seasadj.png", 640, 480)
    print(ggplot(data=df, aes(x=Date, y=guns_total_seas/1e6)) + geom_line() + scale_x_date() +
          ggtitle("Total estimated gun sales") + ylab("in million") + 
          xlab("seasonally adjusted")
          )
    if (savePNG) dev.off()
    
    ## plot gun sales normalized to population
    if (savePNG) png("out/ggplot_total_popadj.png", 640, 480)
    print(ggplot(data=df, aes(x=Date)) +
          geom_line(aes(y=guns_total_per_1000_scaled, colour="black")) +
          geom_line(aes(y=guns_total_per_1000, colour="red")) +
          scale_x_date() + 
          ggtitle("Estimated gun sales per 1000") +
          xlab("red = adjusted for population growth") +
          ylab("") +
          theme(legend.position="none") +
          scale_colour_manual(labels=c("Raw", "Adjusted"),values=c("black", "red"))
          ) 
    if (savePNG) dev.off()
    
    ## plot handgun/longgun 
    if (savePNG) png("out/ggplot_hand_vs_long_guns.png", 640, 480)
    print(ggplot(data=df, aes(x=Date)) +
          geom_line(aes(y=handgun_share, colour="red")) +
          geom_line(aes(y=longgun_share, colour="blue")) +
          scale_x_date() +
          ggtitle("Long guns vs handguns") +
          xlab("red = handguns, blue = long guns") + ylab("") +
          theme(legend.position="none") +
          scale_colour_manual(labels=c("longgun", "handgun"), values=c("blue", "red"))
          )
    if (savePNG) dev.off()
  
    ## plot percent of national for selected states 
    if (savePNG) png("out/ggplot_six_states.png", 640, 480)
    show_states <- c('New Jersey', 'Maryland', 'Georgia',
                     'Louisiana', 'Mississippi', 'Missouri')
    selected <- gsub(" ", "_", tolower(show_states))
    ndf <- data.table::data.table(Date=df[,"Date"], df[,selected])
    ldf <- data.table::melt(ndf, id.vars="Date")
    print(ggplot(data=ldf, aes(x=Date, y=value)) + geom_line() + 
          facet_wrap( ~ variable) + 
          xlab("Percentage of National Sales") + ylab(""))
    if (savePNG) dev.off()
    
    ## compute handgun sales for DC: handung * 1.1 + multiple
    if (savePNG) png("out/ggplot_dc.png", 640, 480)
    print(ggplot(data=df, aes(x=Date, y=dc_handguns_per_100k_national_sales)) + geom_line() +
                 ggtitle("Washington D.C.") + xlab("Sales per 100,000 national handguns"))
    if (savePNG) dev.off()
    
    ## save plots
    if (savePlots) dev.off()

    invisible(NULL)
}

