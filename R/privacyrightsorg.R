#' parse.page
#' @name parse.page
#' @description Parse downloaded html pages from the privacyrights.org breach database
#' @param pg html text to be parsed
#' @import XML stringr
#' @importFrom rvest html_nodes
#' @return data.frame containing breach incident information (date, name, location, 
#' entity.type, breach.type, records.compromised, description, info.links, 
#' info.source)
parse.page <- function(pg) {
  html <- htmlParse(as.character(pg))
  df <- try(expr = {
    # get breach date
    dates <- html_nodes(html, '.data-breach-table > tbody:nth-child(2) > tr > td > span')
    dates <- unlist(lapply(dates, xmlValue))
    dates <- as.Date(dates, format = '%B %d, %Y')
    dates <- dates[ which(!is.na(dates)) ]
    # get victim name and location 
    id <- html_nodes(html, '.data-breach-table > tbody:nth-child(2) > tr > td > strong')
    id <- lapply(lapply(id, xmlToList), unlist)
    name <- unname(unlist(lapply(id, '[', 1)))
    location <- unname(unlist(lapply(id, '[', 2)))
    # get entity type
    entity <- str_trim(lapply(html_nodes(html, '.data-breach-table > tbody:nth-child(2) > tr > td:nth-child(3)'), xmlValue))
    # get breach type
    type <- str_trim(lapply(html_nodes(html, '.data-breach-table > tbody:nth-child(2) > tr > td:nth-child(4)'), xmlValue))
    # get number of records compromised
    records <- str_trim(lapply(html_nodes(html, '.data-breach-table > tbody:nth-child(2) > tr > td:nth-child(5)'), xmlValue))
    # get breach description and source links
    info <- html_nodes(html, '.data-breach-table > tbody:nth-child(2) > tr > td:nth-child(1)')[ seq(2, 150, 3) ]
    info <- lapply(info, html_nodes, 'p')
    info <- lapply(info, function(x) lapply(x, xmlValue))
    info <- lapply(info, unlist)
    info <- lapply(info, str_replace_all, '\\s+', ' ')
    # we will need to re-work this session to get the real href for the more information
    # links rather than the sometimes-truncated link text we currently pull out of the html
    more.info <- lapply(info, str_extract_all, 'More Information\\:.*')
    more.info <- lapply(more.info, unlist)
    more.info <- lapply(more.info, str_replace_all, 'More Information\\:', '')
    more.info <- lapply(more.info, str_trim)
    more.info <- lapply(more.info, paste0, collapse = '\n')
    more.info <- unlist(more.info)
    idx <- lapply(lapply(lapply(info, str_detect, 'More Information\\:'), '!'), which)
    info <- lapply(1:length(info), function(x) {
      info[[ x ]][ unlist(idx[ x ])]
    })
    info <- lapply(info, str_trim)
    info <- lapply(info, paste0, collapse = '\n')
    info <- unlist(info)
    # get breach reporter
    info.source <- html_nodes(html, '.data-breach-table > tbody:nth-child(2) > tr > td:nth-child(1)')[ seq(3, 150, 3)]
    info.source <- str_trim(unlist(lapply(lapply(info.source, xmlToList), '[', 5)))
    info.source <- info.source[ which(str_detect(info.source, '[^0-9]')) ]
    print(dates)
    # put it all together into a data.frame
    df <- data.frame(date = dates, name = name, location = location, 
                     entity.type = entity, breach.type = type, records.compromised = records, 
                     description = info, info.links = more.info, info.source = info.source)    
    df
  })
  df
}

#' refresh.data
#' @name refresh.data
#' @description Download all available html pages from the privacyrights.org breach database
#' at \url{http://privacyrights.org/data-breach}. This method can be called whenever
#' you wish to update your local copy of the database
#' @param data.file rdata file where we cache copies of all downloaded html pages
#' @param polite.pause number of seconds to wait between http requests default 1.5
#' @param download.data whether or not to download all html data AND rebuild the db 
#' (TRUE) or to just rebuild db with existing cached html data (FALSE). Default is FALSE
#' @import XML stringr parallel
#' @importFrom httr content GET
#' @importFrom data.table rbindlist
#' @export
#' @return list containing html content of each page in the breach database
refresh.data <- function(data.file = file.path(system.file(c('inst', 'extdata'),  
                                                           package = 'privacyrightsorg'), 
                                               'privacyrightsorg-html.rdata'),
                         polite.pause = 1.5,
                         download.data = TRUE) {
  message('Please be patient, this can take several minutes to complete!')
  if(download.data) {
    url <- 'http://privacyrights.org/data-breach?title='
    # get page count
    links <- xpathApply(content(GET(url)), '//a')
    max.page <- str_extract(xmlGetAttr(links[[ which(str_detect(lapply(links, xmlValue), 
                                                                '^last')) ]], 'href'), '[0-9]+')
    # create list of urls to download
    urls <- paste0('http://privacyrights.org/data-breach?title=&page=', 1:max.page)
    urls <- c(url, urls)
    pgs <- lapply(urls, function(url) {
      message('Fetching: ', url)
      pg <- content(GET(url), as = 'text')
      Sys.sleep(polite.pause)
      pg
    })
    names(pgs) <- urls
    # save for future re-use
    save('pgs', file = data.file)  
  } else {
    # this will load the "pgs" list into the env
    load(data.file)
  }
  # now parse all pages
  breaches <- mclapply(pgs, parse.page)
  good <- which(lapply(breaches, class) != 'try-error')
  breaches <- breaches[ good ]
  breaches <- data.frame(rbindlist(breaches))
  # return breaches db to caller
  breaches
}