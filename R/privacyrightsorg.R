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
  
  # get victim name and location 
  id <- html_nodes(html, '.data-breach-table > tbody:nth-child(2) > tr > td > strong')
  id <- lapply(lapply(id, xmlToList), unlist)
  name <- unname(unlist(lapply(id, '[', 1)))
  location <- unname(unlist(lapply(id, '[', 2)))
  
  # get breach date
  dates <- html_nodes(html, '.data-breach-table > tbody:nth-child(2) > tr > td > span')
  dates <- unlist(lapply(dates, xmlValue))
  dates <- as.Date(dates, format = '%B %d, %Y')
  dates <- dates[ which(!is.na(dates)) ]
  
  # get entity type
  entity <- str_trim(lapply(html_nodes(html, '.data-breach-table > tbody:nth-child(2) > tr > td:nth-child(3)'), 
                            xmlValue))
  # get breach type
  type <- str_trim(lapply(html_nodes(html, '.data-breach-table > tbody:nth-child(2) > tr > td:nth-child(4)'), 
                          xmlValue))
  
  # get number of records compromised
  records <- str_trim(lapply(html_nodes(html, '.data-breach-table > tbody:nth-child(2) > tr > td:nth-child(5)'), 
                             xmlValue))
  
  # get breach description and source links
  info <- html_nodes(html, '.data-breach-table > tbody:nth-child(2) > tr > td:nth-child(1)')
  info <- info[ seq(2, 150, 3) ] # second row of every record has description + source links
  # here is where we error out if we have an empty record
  info <- try(lapply(info, html_nodes, 'p'))
  if(class(info) == 'try-error') {
    # this is one of those weird empty records with only a breach name and source
    
    # fix missing dates
    if(length(dates) < length(name)) dates <- c(dates, rep(NA, length(name) - length(dates)))
    
    # get records used for total
    records.used <- html_nodes(html, '.data-breach-table > tbody:nth-child(2) > tr > td:nth-child(2) > small > em')
    records.used <- str_extract(lapply(records.used, xmlValue), '[0-9]+(,[0-9]+)*')
    records.used <- str_replace_all(records.used, '[^0-9]', '')
    records.used <- as.numeric(records.used)
    records[ which(records.used == '') ] <- NA
    
    df <- data.frame(date = dates, name = name, location = location, 
                     entity.type = entity, breach.type = type, records.compromised = records,
                     records.used.for.total = records.used, 
                     description = rep(NA, length(name)), source.links = rep(NA, length(name)), 
                     info.source = rep(NA, length(name)))    
    return(df)
  } else {
    # normal record
    info <- lapply(info, function(x) lapply(x, xmlValue))
    info <- lapply(info, unlist)
    info <- lapply(info, str_replace_all, '\\s+', ' ')
    # get source links links
    source.links <- html_nodes(html, '.data-breach-table > tbody:nth-child(2) > tr > td:nth-child(1)')
    source.links <- source.links[ seq(2, 150, 3) ] # second row of every record has description + source links
    source.links <- lapply(source.links, html_nodes, 'a')
    source.links <- lapply(source.links, function(x) xmlApply(x, xmlGetAttr, 'href'))
    source.links <- lapply(source.links, paste0, collapse = ', ')
    source.links <- unlist(source.links)
    idx <- lapply(lapply(lapply(info, str_detect, 'More Information\\:'), '!'), which)
    info <- lapply(1:length(info), function(x) {
      info[[ x ]][ unlist(idx[ x ])]
    })
    info <- lapply(info, str_trim)
    info <- lapply(info, paste0, collapse = '\n')
    info <- unlist(info)
    # get breach reporter
    info.source <- html_nodes(html, '.data-breach-table > tbody:nth-child(2) > tr > td:nth-child(1)')
    info.source <- info.source[ seq(3, 150, 3) ] # 3rd row of each record has reporter
    info.source <- str_trim(unlist(lapply(lapply(info.source, xmlToList), '[', 5)))
    info.source <- info.source[ which(str_detect(info.source, '[^0-9]')) ]
    # get records used for total
    records.used <- html_nodes(html, '.data-breach-table > tbody:nth-child(2) > tr > td:nth-child(2) > small > em')
    records.used <- str_extract(lapply(records.used, xmlValue), '[0-9]+(,[0-9]+)*')
    records.used <- str_replace_all(records.used, '[^0-9]', '')
    records.used <- as.numeric(records.used)
    # put it all together into a data.frame
    df <- data.frame(date = dates, name = name, location = location, 
                     entity.type = entity, breach.type = type, records.compromised = records,
                     records.used.for.total = records.used, description = info, source.links = source.links, 
                     info.source = info.source)    
    return(df)
  }
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
    pages <- xpathApply(content(GET(url)), '//a')
    pages <- pages[[ which(str_detect(lapply(pages, xmlValue), '^last')) ]]
    max.page <- str_extract(xmlGetAttr(pages, 'href'), '[0-9]+')
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
  breaches <- data.frame(rbindlist(breaches))
  breaches$records.used.for.total <- as.numeric(as.character(breaches$records.used.for.total))
  breaches$description <- as.character(breaches$description)
  breaches$source.links <- as.character(breaches$source.links)
  # return breaches db to caller
  breaches
}

#' breaches.timeseries.plot
#' @name breaches.timeseries.plot
#' @description Plot count of breaches as a monthly time series
#' @param breaches data.frame containing privacyrights.org breach database
#' @import zoo RColorBrewer
#' @export
#' @return plot
breaches.timeseries.plot <- function(breaches = privacyrightsorg::breaches) {
  tbl <- tapply(breaches$date, as.yearmon(breaches$date), length)
  tbl <- zoo(tbl, order.by = as.yearmon(names(tbl)))
  pal <- brewer.pal(3, 'Paired')[ 2 ]
  p <- plot(tbl, type = 'l', lwd = 3, bty = 'n', main = 'Breaches per month', 
            xlab = 'date', ylab = 'breaches', col = pal)
  p
}

#' breaches.records.timeseries.plot
#' @name breaches.records.timeseries.plot
#' @description Plot count of records breached as a monthly time series
#' @param breaches data.frame containing privacyrights.org breach database
#' @import zoo RColorBrewer
#' @export
#' @return plot
breaches.records.timeseries.plot <- function(breaches = privacyrightsorg::breaches) {
  tbl <- tapply(breaches$records.used.for.total, as.yearmon(breaches$date), sum)
  tbl <- zoo(tbl, order.by = as.yearmon(names(tbl)))
  pal <- brewer.pal(3, 'Paired')[ 2 ]
  p <- plot(tbl, type = 'l', lwd = 3, bty = 'n', main = 'Breached records per month', 
            xlab = 'date', ylab = 'records', col = pal)
  p
}

#' breaches.type.timeseries.plot
#' @name breaches.type.timeseries.plot
#' @description Plot count of breaches by breach type as a monthly time series
#' @param breaches data.frame containing privacyrights.org breach database
#' @import zoo RColorBrewer
#' @export
#' @return plot
breaches.type.timeseries.plot <- function(breaches = privacyrightsorg::breaches) {
  tbl <- tapply(breaches$date, list(as.yearmon(breaches$date), breaches$breach.type), length)
  tbl <- apply(tbl, 2, function(x) { 
    x[ which(is.na(x)) ] <- 0
    x
  })
  tbl <- zoo(tbl, order.by = as.yearmon(rownames(tbl)))
  tbl <- tbl[ , order(colMeans(tbl, na.rm = TRUE), decreasing = TRUE) ]
  pal <- brewer.pal(12, 'Paired')[ 1:ncol(tbl) ]
  plot(tbl, plot.type = 'single', col = pal, type = 'l', lwd = 2, 
       main = 'Breaches by type with time', bty = 'n', ylab = 'breaches', 
       xlab = 'date')
  legend('topleft', legend = colnames(tbl), fill = pal, bty = 'n', 
         ncol = ncol(tbl) / 2, cex = 0.6)
}

#' breaches.entity.timeseries.plot
#' @name breaches.entity.timeseries.plot
#' @description Plot count of breaches by entity type as a monthly time series
#' @param breaches data.frame containing privacyrights.org breach database
#' @import zoo RColorBrewer
#' @export
#' @return plot
breaches.entity.timeseries.plot <- function(breaches = privacyrightsorg::breaches) {
  tbl <- tapply(breaches$date, list(as.yearmon(breaches$date), breaches$entity.type), length)
  tbl <- apply(tbl, 2, function(x) { 
    x[ which(is.na(x)) ] <- 0
    x
  })
  tbl <- zoo(tbl, order.by = as.yearmon(rownames(tbl)))
  tbl <- tbl[ , order(colMeans(tbl, na.rm = TRUE), decreasing = TRUE) ]
  pal <- brewer.pal(12, 'Paired')[ 1:ncol(tbl) ]
  plot(tbl, plot.type = 'single', col = pal, type = 'l', lwd = 2, 
       main = 'Breaches by entity type with time', bty = 'n', ylab = 'breaches', 
       xlab = 'date')
  legend('topleft', legend = colnames(tbl), fill = pal, bty = 'n', 
         ncol = ncol(tbl) / 2 + 1, cex = 0.6)
}

#' breaches.description.wordcloud
#' @name breaches.description.wordcloud
#' @description Create wordlcoud with most commonly occurring breach description words
#' @param breaches data.frame containing privacyrights.org breach database
#' @param random.seed seed for reproducibility default 1234
#' @param min.freq minimum word frequency default 50
#' @importFrom wordcloud wordcloud
#' @import RColorBrewer
#' @export
#' @return plot
breaches.description.wordcloud <- function(breaches = privacyrightsorg::breaches,
                                           random.seed = 1234,
                                           min.freq = 50) {
  message('Please be patient, this could take a while!')
  set.seed(random.seed)
  pal <- brewer.pal(12, 'Paired')
  p <- wordcloud(breaches$description, min.freq = min.freq, random.order = FALSE, colors = pal)
  p
}

# #' breaches.info.source.plot
# #' @name breaches.info.source.plot
# #' @description Create barplot of info sources
# #' @param breaches data.frame containing privacyrights.org breach database
# #' @param random.seed seed for reproducibility default 1234
# #' @param min.freq minimum word frequency default 1
# #' @import RColorBrewer
# #' @export
# #' @return plot
# breaches.info.source.plot <- function(breaches = privacyrightsorg::breaches) {
#   pal <- brewer.pal(12, 'Paired')
#   tbl <- sort(prop.table(table(breaches$info.source)))
#   pie(tbl, labels = names(tbl), cex = 0.6, radius = 0.8)
# }
