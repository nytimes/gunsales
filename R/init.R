.onAttach <- function(libname, pkgname) {
    txt <- paste0("Package 'gunsales' -- use 'vignette(\"gunsales\")' to see a short summary.\n",
                  "See the repository at https://github.com/NYTimes/gunsales/ for details.")
    if (!.goodOS()) {
        txt <- paste0(txt, "This OS is not fully supported by the x13binary package.\n",
                      "See its documentation for more details and manual setup tips.")
    }
    packageStartupMessage(txt)
}
