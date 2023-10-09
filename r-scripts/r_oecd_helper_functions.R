
library(rsdmx)

# OECD::get_data_structure
OECD_get_data_structure_fun <- function(dataset) {
  
  # # input
  # dataset <- "QNA"
  
  
  
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
