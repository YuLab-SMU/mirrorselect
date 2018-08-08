mirrorselect_internal <- function(mirrors) {
    res <- vapply(mirrors, function(url) {
        url <- paste0(url, "src/base/COPYING")
        tryCatch(system.time(downloader(url))[["elapsed"]],
                 error = function(e) NA)
    }, FUN.VALUE = numeric(1))
}

##' @importFrom utils download.file
downloader <- function(url) {
    download.file(url, tempfile(), quiet = TRUE)
}

##' extract mirror lists from https://cran.r-project.org/mirrors.html
##'
##' 
##' @title get_mirror
##' @return a vector of mirror urls
##' @export
##' @author Guangchuang Yu
get_mirror <- function() {
    url <- "https://cran.r-project.org/mirrors.html"
    x <- readLines(url)
    mirrors <- unique(sub(".*(https{0,1}://[a-zA-z\\.\\/]+).*", "\\1", x[grep("^<a", x)]))
    return(mirrors)
}

##' test download speed of CRAN mirrors
##' by recording download time for mirror/src/base/COPYING
##'
##' 
##' @title mirrorselect
##' @param mirrors a vector of CRAN mirrors
##' @return data frame with a column of mirror and second column of speed
##' @export
##' @author Guangchuang Yu
mirrorselect <- function(mirrors) {
    speed <- mirrorselect_internal(mirrors)
    res <- data.frame(mirror=names(speed), speed=speed)
    res <- res[!is.na(speed),]
    res <- res[order(res$speed),]
    return(res)
}


