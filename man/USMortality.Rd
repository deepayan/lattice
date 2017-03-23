\name{H_USMortality}
\alias{USMortality}
\alias{USRegionalMortality}
\docType{data}
\title{ Mortality Rates in US by Cause and Gender }
\description{
  These datasets record mortality rates across all ages in the USA by
  cause of death, sex, and rural/urban status, 2011--2013. The two
  datasets represent the national aggregate rates and the region-wise
  rates for each administrative region under the Department of Health
  and Human Services (HHS).
}
\usage{
USMortality
USRegionalMortality
}
\format{
  \code{USRegionalMortality} is a data frame with 400 observations on
  the following 6 variables.

  \describe{
    \item{\code{Region}}{A factor specifying HHS Region. See details.}
    \item{\code{Status}}{A factor with levels \code{Rural} and \code{Urban}}
    \item{\code{Sex}}{A factor with levels \code{Female} and \code{Male}}
    \item{\code{Cause}}{Cause of death. A factor with levels
      \code{Alzheimers}, \code{Cancer}, \code{Cerebrovascular diseases},
      \code{Diabetes}, \code{Flu and pneumonia}, \code{Heart disease},
      \code{Lower respiratory}, \code{Nephritis}, \code{Suicide}, and
      \code{Unintentional injuries}}
    \item{\code{Rate}}{Age-adjusted death rate per 100,000 population}
    \item{\code{SE}}{Standard error for the rate}
  }

  \code{USMortality} is a data frame with 40 observations, containing
  the same variables with the exception of \code{Region}.
}


\details{ The region-wise data give estimated rates separately for each
  of 10 HHS regions. The location of the regional offices and their
  coverage area, available from
  \url{https://www.hhs.gov/about/agencies/iea/regional-offices/index.html},
  is given below.

  \describe{

    \item{HHS Region 01 - Boston: }{ Connecticut, Maine, Massachusetts,
      New Hampshire, Rhode Island, and Vermont }

    \item{ HHS Region 02 - New York: }{ New Jersey, New York, Puerto
      Rico, and the Virgin Islands }

    \item{HHS Region 03 - Philadelphia: }{ Delaware, District of
      Columbia, Maryland, Pennsylvania, Virginia, and West Virginia}

    \item{HHS Region 04 - Atlanta: }{ Alabama, Florida, Georgia, Kentucky,
      Mississippi, North Carolina, South Carolina, and Tennessee}

    \item{HHS Region 05 - Chicago: }{ Illinois, Indiana, Michigan,
      Minnesota, Ohio, and Wisconsin}

    \item{HHS Region 06 - Dallas: }{ Arkansas, Louisiana, New Mexico,
      Oklahoma, and Texas }

    \item{HHS Region 07 - Kansas City: }{ Iowa, Kansas, Missouri, and
      Nebraska}

    \item{HHS Region 08 - Denver: }{ Colorado, Montana, North Dakota,
      South Dakota, Utah, and Wyoming}

    \item{HHS Region 09 - San Francisco: }{ Arizona, California, Hawaii,
      Nevada, American Samoa, Commonwealth of the Northern Mariana
      Islands, Federated States of Micronesia, Guam, Marshall Islands,
      and Republic of Palau}

    \item{HHS Region 10 - Seattle: }{ Alaska, Idaho, Oregon, and
      Washington} } }

\references{

  Rural Health Reform Policy Research Center. _Exploring Rural and Urban
  Mortality Differences_, August 2015 Bethesda, MD. 
  \url{https://ruralhealth.und.edu/projects/health-reform-policy-research-center/rural-urban-mortality}

  
}

\examples{
dotplot(reorder(Cause, Rate) ~ Rate | Status,
        data = USMortality, groups = Sex,
        par.settings = simpleTheme(pch = 16), auto.key = list(columns = 2),
        scales = list(x = list(log = TRUE, equispaced.log = FALSE)))
dotplot(reorder(Cause, Rate):Sex ~ Rate | Status,
        data = USRegionalMortality, groups = Sex,
        scales = list(x = list(log = TRUE, equispaced.log = FALSE)))
}

\keyword{datasets}
