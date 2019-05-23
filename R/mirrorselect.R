mirrorselect_internal <- function(mirrors) {
    if (any(grepl('bioc', mirrors))) {
        file <- "packages/release/bioc/html/ggtree.html"
    } else {
        file <- "src/base/COPYING"
    }

    res <- vapply(mirrors, function(url) {
        url <- paste0(url, file)
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
##' @param repo one of 'CRAN' or 'BioC'
##' @return a vector of mirror urls
##' @export
##' @examples
##' head(get_mirror())
##' @author Guangchuang Yu
get_mirror <- function(repo = "CRAN") {
    if  (repo == "CRAN") {
        return(get_mirror_cran())
    }

    get_mirror_bioc()
}

get_mirror_cran <- function() {
    url <- "https://cran.r-project.org/mirrors.html"
    x <- readLines(url)
    unique(sub(".*(https{0,1}://[a-zA-z\\.\\/]+).*", "\\1", x[grep("^<a", x)]))
}

##' @importFrom magrittr %>%
get_mirror_bioc <- function() {
    url <- 'https://www.bioconductor.org/about/mirrors/'
    x <- readLines(url)
    x[grep('URLs', x)] %>% strsplit(';') %>%
        unlist %>% gsub('<[^>]+>', '', .) %>%
        sub("URLs:", '', .) %>%
        sub("\\s+", '', .)
}

##' test download speed of CRAN mirrors
##' by recording download time for mirror/src/base/COPYING
##'
##' 
##' @title mirrorselect
##' @param mirrors a vector of CRAN mirrors
##' @return data frame with a column of mirror and second column of speed
##' @export
##' @examples
##' x <- mirrorselect(c("http://cran.stat.upd.edu.ph/",
##'                   "http://healthstat.snu.ac.kr/CRAN/",
##'                   "http://cran.ism.ac.jp/"))
##' head(x)
##' @author Guangchuang Yu
mirrorselect <- function(mirrors) {
    speed <- mirrorselect_internal(mirrors)
    res <- data.frame(mirror=names(speed), speed=speed)
    res <- res[!is.na(speed),]
    res <- res[order(res$speed),]
    return(res)
}


utils::globalVariables(".")
