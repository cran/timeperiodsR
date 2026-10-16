custom_period <- function( start, 
                           end,
                           part = getOption("timeperiodsR.parts")) {
  
    if ( ! "Date" %in% class(start) ) {
      start <- as.Date(start)
    }
    
    if ( ! "Date" %in% class(end) ) {
      end <- as.Date(end)
    }
    
    sequence <- seq.Date(from = start, to = end, by = "day")
    workdays <- sequence[ ! format(sequence, "%w") %in% c("0", "6") ]
    weekends <- sequence[ format(sequence, "%w") %in% c("0", "6") ]
    
   
    
    out   <- list(start           = start,
                  end             = end,                
                  sequence        = sequence,
                  workdays        = workdays,
                  weekends        = weekends,
                  first_workday   = if ( length(workdays) > 0 ) min(workdays) else as.Date(NA),
                  last_workday    = if ( length(workdays) > 0 ) max(workdays) else as.Date(NA),
                  first_weekend   = if ( length(weekends) > 0 ) min(weekends) else as.Date(NA),
                  last_weekend    = if ( length(weekends) > 0 ) max(weekends) else as.Date(NA),
                  length          = length(sequence),
                  workdays_length = length(workdays),
                  weekends_length = length(weekends))
    
    if ( isTRUE( getOption( "timeperiodsR.official_day_offs" ) ) ) {
      
        out$dayoffs_marks     <- check_dayoffs(date = as.character(sequence))
        
        out$official_day_offs <- sequence[ out$dayoffs_marks %in% c("1", "2", "3") ]
        out$official_workdays <- sequence[ ! out$dayoffs_marks %in% c("1", "2", "3") ]
        out$official_first_workday <- if ( length(out$official_workdays) > 0 ) min(out$official_workdays) else as.Date(NA)
        out$official_last_workday  <- if ( length(out$official_workdays) > 0 ) max(out$official_workdays) else as.Date(NA)
        out$custom_day_offs <- sequence[ out$dayoffs_marks == "3" ]
        
    } else if ( ! is.null(getOption( "timeperiodsR.custom_day_offs" )) ) {
      
        custom_day_offs_cha <- as.character(getOption("timeperiodsR.custom_day_offs"))
        out$dayoffs_marks <- ifelse(as.character(sequence) %in% custom_day_offs_cha, "3", "0")
        names(out$dayoffs_marks) <- as.character(sequence)
        out$custom_day_offs <- sequence[ as.character(sequence) %in% custom_day_offs_cha ]
        
    } 
    
    class(out) <- "tpr"
    
    part <- match.arg(part, getOption("timeperiodsR.parts"))
    
    if ( part == "all" ) {
      return(out) 
    } else {
      return(out[[part]]) 
    } 
}
