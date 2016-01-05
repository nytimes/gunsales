 .onLoad <- function(libname, pkgname) {
     ## while seasonal remains borked in its interaction with x13binary we just do this
     x13path <- system.file("bin", package="x13binary")
     stopifnot(x13path != "")
     Sys.setenv("X13_PATH"=x13path)
}