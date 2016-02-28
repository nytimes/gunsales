.onAttach <- function(libname, pkgname) {
    txt <- paste0("Package 'gunsales' -- use 'vignette(\"gunsales\")' to see a short summary.\n",
                  "See the repository at https://github.com/NYTimes/gunsales/ for details.")
    packageStartupMessage(txt)
}
