.onAttach <- function(libname, pkgname) {
    txt <- paste("Package 'gunsales' -- use 'vignette(\"gunsales\")' to see a short summary\n",
                 "and 'edit(vignette(\"gunsales\"))' to examine the code.\n", sep="")
    packageStartupMessage(txt)
}
