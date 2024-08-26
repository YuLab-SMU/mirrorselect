mirrorselect_internal <- function(mirrors) {
    if (any(grepl('bioc', mirrors))) {
        file <- "packages/release/bioc/html/ggtree.html"
    } else {
        file <- "src/base/COPYING"
    }

    mirrors <- sprintf("%s%s", mirrors, file)

    res <- vapply(mirrors, function(url) {
        tryCatch(system.time(downloader(url))[["elapsed"]],
                 error = function(e) NA)
    }, FUN.VALUE = numeric(1))
    return(res)
}


downloader <- function(url) {
    yulab.utils:::mydownload(url, tempfile())
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

##' @importFrom utils getCRANmirrors
get_mirror_cran <- function(country = "global") {
    ## url <- "https://cran.r-project.org/mirrors.html"
    ## x <- readLines(url)
    ## unique(sub(".*(https{0,1}://[a-zA-z\\.\\/]+).*", "\\1", x[grep("^<a", x)]))
    mirrors <- utils::getCRANmirrors()$URL 
    extract_mirror(mirrors, country)
}

get_mirror_bioc <- function(country = "global") {
    ## url <- 'https://www.bioconductor.org/about/mirrors/'
    ## x <- readLines(url)
    ## x[grep('URLs', x)] %>% strsplit(';') %>%
    ##     unlist %>% gsub('<[^>]+>', '', .) %>%
    ##     sub("URLs:", '', .) %>%
    ##     sub("\\s+", '', .)
    .getMirrors <- utils::getFromNamespace('.getMirrors', 'utils')

    Bioc_mirror <- file.path(R.home("doc"), "BioC_mirrors.csv")
    local_file <- FALSE
    if (file.exists(Bioc_mirror)) {
        local_file <- TRUE
    }

    mirrors <- .getMirrors(
        "https://bioconductor.org/BioC_mirrors.csv",
        Bioc_mirror,
        all = FALSE, local.only = local_file
    )

    extract_mirror(mirrors$URL, country)
}

extract_mirror <- function(mirrors, country) {
    if (country == 'global') return (mirrors)
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
##' if (yulab.utils::has_internet()) {
##'     x <- mirrorselect(c("http://cran.stat.upd.edu.ph/",
##'                       "http://healthstat.snu.ac.kr/CRAN/",
##'                       "http://cran.ism.ac.jp/"))
##'     head(x)
##' }
##' @author Guangchuang Yu
mirrorselect <- function(mirrors) {
    speed <- mirrorselect_internal(mirrors)
    res <- data.frame(mirror=names(speed), speed=speed)
    res <- res[!is.na(speed),]
    res <- res[order(res$speed),]
    return(res)
}


# utils::globalVariables(".")
