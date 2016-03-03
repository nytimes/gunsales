
## we are having issues on solaris and with old windows releases
## the supportedPlatform() check covers Solaris, but we need to explicitly check for old R

.goodOS <- function() {
    rversion <- paste(R.Version()$major, R.Version()$minor, sep = ".")
    isoldrel <- .Platform$OS.type == "windows" && (compareVersion(rversion, "3.1.3") < 1)
    x13binary::supportedPlatform() && !isoldrel
}
