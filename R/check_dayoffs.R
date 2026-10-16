check_dayoffs <- function(
    date    = NULL,
    year    = NULL,
    month   = NULL,
    day     = NULL,
    cc      = getOption("timeperiodsR.official_day_offs_country"),
    pre     = getOption("timeperiodsR.official_day_offs_pre"),
    include_custom_day_offs = TRUE
    ) {
  
  # check httr
  if ( !requireNamespace("httr", quietly = TRUE) ) { 
    message("The 'httr' package is required to get official day off components. Please install it.")
    return(NA)
  }
  
  api_failed <- FALSE
  
  get_isdayoff_data <- function(query_list) {
    if ( api_failed ) {
      return("9")
    }
    
    ans <- tryCatch(
      httr::GET(
        "https://isdayoff.ru/api/getdata",
        query = query_list,
        httr::timeout(5)
      ),
      error = function(e) {
        api_failed <<- TRUE
        message("Failed to connect to isDayOff API: ", conditionMessage(e))
        return(NULL)
      }
    )
    
    if ( is.null(ans) ) {
      return("9")
    }
    
    # check status
    if ( httr::http_error(ans) ) {
      if ( !api_failed ) {
        api_failed <<- TRUE
        message("isDayOff API returned HTTP error: status code ", httr::status_code(ans))
      }
      return("9")
    }
    
    temp_out <- httr::content(ans, encoding = "UTF-8", as = "text")
    return(temp_out)
  }
  
  if ( ! is.null(date) ) {
  
    if ( inherits(date, "tpr") ) {
      date <- as.character(seq(date))
    }
    
    if ( inherits(date, "Date") ) {
      date <- as.character(date)
    }
    
    custom_day_offs_cha <- as.character(getOption("timeperiodsR.custom_day_offs"))
    
    out <-
      sapply(date, 
             function(x) {
              
              # check for custom day offs
              if ( x %in% custom_day_offs_cha & isTRUE( include_custom_day_offs ) ) {
                return("3")
              }
                
              if ( ! is.null(x) ) {
                if ( ! inherits(x, "Date") ) x <- as.Date(x)
                year  <- format(x, "%Y")
                month <- format(x, "%m")
                day   <- format(x, "%d")
              }
              
              temp_out <- get_isdayoff_data(
                list(
                  year  = year,
                  month = month,
                  day   = day,
                  cc    = cc,
                  pre   = pre
                )
              )
              
              return(temp_out)
              
             }, USE.NAMES = TRUE
     )
    
  } else {
    
    out <- get_isdayoff_data(
      list(
        year  = year,
        month = month,
        day   = day,
        cc    = cc,
        pre   = pre
      )
    )
    
  }
  
  return(out)
}
