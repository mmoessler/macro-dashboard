
test <- FALSE

if (test == TRUE) {
  
  ecb__get_data <- function (key, filter = NULL, ...) {
    
    key <- "FM.B.U2.EUR.4F.KR.MRR_FR.LEV"
    key <- "FM.D.U2.EUR.4F.KR.MRR_FR.LEV"
    # key <- "FM.B.U2.EUR.4F.KR.MLFR.LEV"
    filter <- NULL
    
    
    
    if (!"detail" %in% names(filter)) {
      filter <- c(filter, detail = "dataonly")
    }
    if (!filter[["detail"]] %in% c("full", "dataonly")) {
      return(get_dimensions(key))
    }
    query_url <- ecb:::create_query_url(key, filter = filter)
    req <- ecb:::make_request(query_url, "data")
    tmp <- tempfile()
    writeLines(httr::content(req, "text", encoding = "utf-8"), tmp)
    result <- rsdmx::readSDMX(tmp, FALSE)
    unlink(tmp)
    df <- as.data.frame(result)
    df <- structure(df, class = c("tbl_df", "tbl", "data.frame"), names = tolower(names(df)))
    df
  }
  
  
  
  
}