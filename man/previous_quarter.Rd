\name{previous_quarter}
\alias{previous_quarter}
\title{
Start and end of previous quarter
}
\description{
Defines first and last date in n previous quarter
}
\usage{
previous_quarter(x = Sys.Date(),
                 n = 1, 
                 part = getOption("timeperiodsR.parts"))
}
\arguments{
  \item{x}{Date object}
  \item{n}{Number of periods for offset}
  \item{part}{Part of period you need to receive, one of "all", "start", "end","sequence", "length". See details.}
}
\details{
You can get object of tpr class with all components or specify which component you need, use \code{part} for manage this option:
\itemize{
  \item all - get all components
  \item start - get only first date of period
  \item end - get only last date of period
  \item start - get vector of all dates in period
  \item length - get number of dates in period
}
}
\value{Object of tpr class}
\author{
Alexey Seleznev
}
\seealso{
For get next other periods see \code{\link[timeperiodsR:previous_week]{previous_week()}}, \code{\link[timeperiodsR:previous_month]{previous_month()}}, \code{\link[timeperiodsR:previous_year]{previous_year()}}
}
\examples{
## To get start, end and sequence of previous quarter
previousquarter <- previous_quarter()

## To get vector of date sequences 
previous_quarter(part = "sequence")
previous_quarter()$sequence
seq(previousquarter)

## Get number of days of previous quarter
day_nums <- previous_quarter(part = "length")
previous_quarter()$length
length(previousquarter)
}
