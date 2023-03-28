
library(rsdmx)

# OECD::get_data_structure
OECD_get_data_structure_edit <- function (dataset) {
  
  # # input
  # dataset <- "DUR_D"
  
  
  
  url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/", dataset)
  # data_structure <- readsdmx::read_sdmx(url)
  # see: https://github.com/expersso/OECD/issues/24
  data_structure <- rsdmx::readSDMX(url)
  variable_desc <- data.frame(data_structure@concepts)
  variable_desc[] <- lapply(variable_desc, as.character)
  variable_desc$en[is.na(variable_desc$en)] <- variable_desc$Name.en[!is.na(variable_desc$Name.en)]
  names(variable_desc)[length(names(variable_desc))] <- "description"
  variable_desc <- variable_desc[, c("id", "description")]
  code_names <- data_structure@codelists@codelists
  code_names <- vapply(code_names, function(x) x@id, "character")
  code_list <- lapply(code_names, function(x) {
    df <- as.data.frame(data_structure@codelists, codelistId = x)
    try({
      df <- df[, c("id", "label.en")]
      names(df)[2] <- "label"
    }, silent = TRUE)
    df
  })
  names(code_list) <- gsub(paste0("CL_", dataset, "_"), "", code_names)
  full_df_list <- c(VAR_DESC = list(variable_desc), code_list)
  full_df_list
}
# <bytecode: 0x000001f136934450>
#   <environment: namespace:OECD>

# OECD::get_dataset
OECD_get_dataset_edit <- function (dataset, filter = NULL, start_time = NULL, end_time = NULL, pre_formatted = FALSE, ...) {
  
  # inputs
  loc <- qna.str$LOCATION[,1]
  sub <- qna.str$SUBJECT[,1]
  mea <- c("LNBQRSA") # Measure: Constant prices seasonal adjusted
  fre <- c("Q") # Frequency: Quarterly frequency
  sta.dat <- as.Date("2005-01-01") # Periods: "yyyy-mm-dd"
  end.dat <- as.Date("2022-10-01")
  # Note: Q1: 2020-01-01; Q2: 2020-04-01; Q3: 2020-07-01; Q3: 2020-10-01
  fil <- list(loc,sub,mea,fre) # Collect inputs for get_dataset function
  
  
  
  # > Germany, output
  loc <- c("DEU") # Location: Germany
  mea <- c("LNBQRSA") # Measure: Constant prices seasonal adjusted
  fre <- c("Q") # Frequency: Quarterly frequency
  sta.dat <- as.Date("2005-01-01") # Periods: "yyyy-mm-dd"
  end.dat <- as.Date("2022-10-01")
  # Note: Q1: 2020-01-01; Q2: 2020-04-01; Q3: 2020-07-01; Q3: 2020-10-01
  
  sub <- c("B1_GA","B1GVA","B1GVB_E","B1GVF","B1GVG_I","B1GVJ","B1GVK","B1GVL","B1GVM_N","B1GVO_Q","B1GVR_U","D21_D31")
  sub.agr <- c("B1GVA")
  sub.ind <- c("B1GVB_E")
  sub.con <- c("B1GVF")
  sub.ser <- c("B1GVG_I","B1GVJ","B1GVK","B1GVL","B1GVM_N","B1GVO_Q","B1GVR_U")
  sub.tls <- c("D21_D31") # taxes less subsidies on products
  sub.tot <- c("B1_GA")
  sub.inp.lis <- list(agr = sub.agr,
                      ind = sub.ind,
                      con = sub.con,
                      ser = sub.ser,
                      tls = sub.tls,
                      tot = sub.tot)
  
  fil <- list(loc,sub,mea,fre) # Collect inputs for get_dataset function
  
  dataset <- "QNA"
  filter <- fil
  start_time <- sta.dat
  end_time <- end.dat
  
  pre_formatted <- FALSE

  
  
  
  
  if (is.null(filter) && pre_formatted) {
    stop("If pre_formatted is TRUE, you must provide a value to the filter argument.")
  }
  if (is.null(filter) && !pre_formatted) {
    filter <- "all"
  }
  if (!is.null(filter) && !pre_formatted) {
    filter <- lapply(filter, function(x) paste(x, collapse = "+"))
    filter <- paste(filter, collapse = ".")
  }
  if (!is.null(filter) && pre_formatted) {
    filter <- filter
  }
  path <- sprintf("restsdmx/sdmx.ashx/GetData/%s/%s/all", dataset, filter)
  url_list <- list(scheme = "https",
                   hostname = "stats.oecd.org", 
                   path = path,
                   query = list(startTime = start_time, endTime = end_time))
  class(url_list) <- "url"
  url <- httr::build_url(url_list)
  # df <- readsdmx::read_sdmx((url), ...)
  df <- readsdmx::read_sdmx((url))
  class(df) <- c("tbl_df", "tbl", "data.frame")
  df
  
}
# <bytecode: 0x000001f13a1041b0>
#   <environment: namespace:OECD>
