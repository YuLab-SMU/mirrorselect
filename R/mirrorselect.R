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
                  
# Add an argument letting users to specify the mirrors from a specific country to test, default is China
get_mirror <- function(repo = "CRAN", country = "cn") {
    if  (repo == "CRAN") {
        return(get_mirror_cran(country))
    }

    get_mirror_bioc()
}

get_mirror_cran <- function(country = "cn") {
    library(magrittr)
    utils::getCRANmirrors()$URL %>% grep(pattern = paste(".", country, "/", sep = ""), value=TRUE)
}

get_mirror_bioc <- function(country = "cn") {
    library(magrittr)
    .getMirrors <- utils::getFromNamespace('.getMirrors', 'utils')
    .getMirrors("https://bioconductor.org/BioC_mirrors.csv",
                file.path(R.home("doc"), "BioC_mirrors.csv"),
                all = FALSE, local.only = FALSE)$URL%>% grep(pattern = paste(".", country, "/", sep = ""), value=TRUE)
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
