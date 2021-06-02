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

##' Access CRAN or Bioconductor mirror
##'
##' 
##' The mirror lists are obtained from 
##' https://cran.r-project.org/mirrors.html (CRAN) or
##' https://bioconductor.org/BioC_mirrors.csv (Bioconductor).
##' This function allows user to extract mirrors from a specific country using internet country code.                  
##' @title get_mirror
##' @param repo one of 'CRAN' or 'BioC'
##' @param country specify the mirrors from a specific country. 
##'                Default to 'global' without filtering.
##' @return a vector of mirror urls
##' @export
##' @examples
##' head(get_mirror())
##' @author Guangchuang Yu
get_mirror <- function(repo = "CRAN", country = "global") {
    if  (repo == "CRAN") {
        return(get_mirror_cran(country))
    }

    get_mirror_bioc(country)
}

get_mirror_cran <- function(country = "global") {
    ## url <- "https://cran.r-project.org/mirrors.html"
    ## x <- readLines(url)
    ## unique(sub(".*(https{0,1}://[a-zA-z\\.\\/]+).*", "\\1", x[grep("^<a", x)]))
    mirrors <- utils::getCRANmirrors()$URL 
    extract_mirror(mirror, country)
}

get_mirror_bioc <- function(country = "global") {
    ## url <- 'https://www.bioconductor.org/about/mirrors/'
    ## x <- readLines(url)
    ## x[grep('URLs', x)] %>% strsplit(';') %>%
    ##     unlist %>% gsub('<[^>]+>', '', .) %>%
    ##     sub("URLs:", '', .) %>%
    ##     sub("\\s+", '', .)
    .getMirrors <- utils::getFromNamespace('.getMirrors', 'utils')
    mirrors <- .getMirrors("https://bioconductor.org/BioC_mirrors.csv",
                file.path(R.home("doc"), "BioC_mirrors.csv"),
                all = FALSE, local.only = FALSE)$URL
    extract_mirror(mirror, country)
}

extract_mirror <- function(mirror, country) {
    if (country == 'global') return (mirror)
    grep(pattern = paste(".", country, "/", sep = ""), x = mirrors, value=TRUE)    
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
