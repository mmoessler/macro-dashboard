

get_eurostat_edit_xx <- function (id, time_format = "date", filters = "none", type = "code", 
                                  select_time = NULL, cache = TRUE, update_cache = FALSE, cache_dir = NULL, 
                                  compress_file = TRUE, stringsAsFactors = FALSE, keepFlags = FALSE, 
                                  legacy_bulk_download = TRUE, ...) {
  
  if (!check_access_to_data()) {
    message("You have no access to ec.europe.eu.\n      Please check your connection and/or review your proxy settings")
  } else {
    if (keepFlags & !is.character(filters) && filters != "none") {
      warning("The keepFlags argument of the get_eurostat function\n can be used only without filters. No Flags returned.")
    }
    if (is.null(filters) || identical(filters, "none")) {
      cache <- FALSE
    }
    if (cache) {
      update_cache <- update_cache | getOption("eurostat_update", FALSE)
      cache_dir <- eur_helper_cachedir(cache_dir)
      cache_file <- file.path(cache_dir, paste0(id, "_", time_format, "_", type, select_time, "_", strtrim(stringsAsFactors, 1), strtrim(keepFlags, 1), ".rds"))
    }
    if (!cache || update_cache || !file.exists(cache_file)) {
      if (is.null(filters) || is.list(filters)) {
        y <- get_eurostat_json(id, filters, type = type, stringsAsFactors = stringsAsFactors, ...)
        y$time <- convert_time_col(factor(y$time), time_format = time_format)
      } else if (filters == "none") {
        if (legacy_bulk_download == TRUE) {
          y_raw <- try(get_eurostat_raw(id))
          if ("try-error" %in% class(y_raw)) {
            stop(paste("get_eurostat_raw fails with the id", id, "\n"))
          }
          y <- tidy_eurostat(y_raw, time_format, select_time, stringsAsFactors = stringsAsFactors, keepFlags = keepFlags)
        } else {
          message("Trying to download from the new dissemination API... \n")
          y_raw <- try(get_eurostat_raw2(id))
          if ("try-error" %in% class(y_raw)) {
            stop(paste("get_eurostat_raw fails with the id", id))
          }
          y <- tidy_eurostat2(y_raw, time_format, select_time, stringsAsFactors = stringsAsFactors, keepFlags = keepFlags)
        }
        if (type == "code") {
          y <- y
        } else if (type == "label" && legacy_bulk_download == TRUE) {
          y <- label_eurostat(y)
        } else if (type == "label" && legacy_bulk_download == FALSE) {
          y <- label_eurostat2(y)
        } else if (type == "both") {
          stop("type = \"both\" can be only used with JSON API. Set filters argument")
        } else {
          stop("Invalid type.")
        }
      }
    } else {
      cf <- path.expand(cache_file)
      message(paste("Reading cache file", cf))
      y <- readRDS(cache_file)
      message(paste("Table ", id, " read from cache file: ", cf))
    }
    if (cache && (update_cache || !file.exists(cache_file))) {
      saveRDS(y, file = cache_file, compress = compress_file)
      message("Table ", id, " cached at ", path.expand(cache_file))
    }
    
    y
    
  }
  
}

get_eurostat_json_edit_xx <- function (id, filters = NULL, type = "code", lang = "EN", stringsAsFactors = FALSE, ...) {
  
  user_want_stop <- special_id_values(id)
  if (user_want_stop) {
    return(NULL)
  }
  if (!check_access_to_data()) {
    message("You have no access to ec.europe.eu.\n Please check your connection and/or review your proxy settings")
  } else {
    url <- eurostat_json_url(id = id, filters = filters, lang = lang)
    resp <- httr::RETRY("GET", url, terminate_on = c(404))
    if (httr::http_error(resp)) {
      stop(paste("The requested url cannot be found within the get_eurostat_json function:", url))
    }
    status <- httr::status_code(resp)
    msg <- ". Some datasets are not accessible via the eurostat\n interface. You can try to search the data manually from the comext\n  \t  database at http://epp.eurostat.ec.europa.eu/newxtweb/ or bulk\n  \t  download facility at\n  \t  http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing\n  \t  or annual Excel files\n  \t  http://ec.europa.eu/eurostat/web/prodcom/data/excel-files-nace-rev.2"
    if (status == 200) {
      jdat <- jsonlite::fromJSON(url)
    } else if (status == 400) {
      stop("Failure to get data. Probably invalid dataset id. Status code: ", status, msg)
    } else if (status == 500) {
      stop("Failure to get data. Probably filters did not return any data\n or data exceeded query size limitation. Status code: ", status, msg)
    } else if (status == 416) {
      stop("Too many categories have been requested. Maximum is 50.", status, msg)
    } else {
      stop("Failure to get data. Status code: ", status, msg)
    }
    dims <- jdat$dimension
    ids <- jdat$id
    dims_list <- lapply(dims[rev(ids)], function(x) {
      y <- x$category$label
      if (type == "label") {
        y <- unlist(y, use.names = FALSE)
      } else if (type == "code") {
        y <- names(unlist(y))
      } else if (type == "both") {
        y <- unlist(y)
      } else {
        stop("Invalid type ", type)
      }
    })
    variables <- expand.grid(dims_list, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = stringsAsFactors)
    dat <- as.data.frame(variables[rev(names(variables))])
    vals <- unlist(jdat$value, use.names = FALSE)
    dat$values <- rep(NA, nrow(dat))
    inds <- 1 + as.numeric(names(jdat$value))
    if (!length(vals) == length(inds)) {
      stop("Complex indexing not implemented.")
    }
    
    dat$values[inds] <- vals
    tibble::as_tibble(dat)
    
  }
  
}

eurostat_eurostat_json_url_edit_xx <- function (id, filters = NULL, lang = NULL) {
  
  id <- "namq_10_gdp"
  filters <- list(geo = c("DE","FR","EA19"),
                  na_item = c("B1G"),
                  unit = c("CLV05_MEUR"),
                  s_adj = c("SCA"))
  lang <- NULL
  
  
  
  filters2 <- as.list(unlist(filters))
  names(filters2) <- rep(names(filters), lapply(filters, length))
  if (!hasName(filters2, "format")) {
    filters2$format <- "JSON"
  }
  if (!is.null(lang)) {
    if (toupper(lang) %in% c("EN", "FR", "DE")) {
      filters2$lang <- toupper(lang)
    } else {
      message("Unsupported language code used. Using the default language: \"EN\"")
      filters2$lang <- "EN"
    }
  } else {
    message("Using the default language: \"EN\"")
    filters2$lang <- "EN"
  }
  host_url <- "https://ec.europa.eu/eurostat/api/dissemination/"
  service <- "statistics/"
  version <- "1.0/"
  response_type <- "data/"
  datasetCode <- id
  url_list <- httr::parse_url(host_url)
  url_list$path <- paste0(url_list$path, service, version, response_type, datasetCode)
  url_list$query <- filters2
  url <- httr::build_url(url_list)
  
  url
  
}
