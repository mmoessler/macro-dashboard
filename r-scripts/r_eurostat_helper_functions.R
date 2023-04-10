
get_eurostat_xx <- function(url) {

  # # input
  # fre <- "month"
  # url <- "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/sts_inpr_m?format=JSON&geo=DE&geo=FR&geo=EA19&freq=M&unit=I15&s_adj=SCA&indic_bt=PROD&nace_r2=B-D"
  # 
  # fre <- "quarter"
  # url <- "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/namq_10_gdp?format=JSON&geo=DE&geo=FR&geo=EA19&freq=Q&unit=CLV15_MEUR&na_item=B1G&s_adj=SCA"

      
  
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

  return(data)
  
}
