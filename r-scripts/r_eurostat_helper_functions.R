
get_eurostat_xx <- function(url) {

  type <- c("code")
  stringsAsFactors <- FALSE

  resp <- httr::RETRY("GET", url, terminate_on = c(404))
  
  jdat <- jsonlite::fromJSON(url)
  
  dims <- jdat$dimension
  ids <- jdat$id
  dims_list <- lapply(dims[rev(ids)], function(x) {
    y <- x$category$label
    if (type[1] == "label") {
      y <- unlist(y, use.names = FALSE)
    } else if (type[1] == "code") {
      y <- names(unlist(y))
    } else if (type[1] == "both") {
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
  
  dat$values[inds] <- vals
  data <- tibble::as_tibble(dat)
  data
  
  data$time <- as.Date(as.yearmon(data$time))
  
  ser.dat <- subset(data, time %in% seq(as.Date("2015-01-01"), Sys.Date(), by = "month"))
  head(ser.dat)
  
  return(ser.dat)
  
}
