################################################################################
# FUNCTIONS                                                                    #
################################################################################

read_name <- function(path) {
  
  year <- stringr::str_extract(path, "[0-9]{4}.txt")
  year <- stringr::str_extract(year, "[0-9]{4}")
  
  df <- 
    readr::read_csv(path, 
                    col_names = c("name", "gender", "count"), 
                    col_types = "cfi") %>% 
    dplyr::mutate(year = base::as.Date(glue::glue("{year}-01-01"))) %>% 
    dplyr::group_by(year, gender) %>% 
    dplyr::arrange(year, gender, dplyr::desc(count)) %>% 
    dplyr::mutate(rank = 1:dplyr::n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(year, gender)
  
  return(df)
  
}