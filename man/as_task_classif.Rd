% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_task_classif_st.R
\name{as_task_classif}
\alias{as_task_classif}
\alias{as_task_classif.TaskClassifST}
\title{Convert to a Classification Task}
\usage{
as_task_classif(x, ...)

\method{as_task_classif}{TaskClassifST}(x)
}
\arguments{
\item{x}{(\code{any})\cr
Object to convert.}

\item{...}{(\code{any})\cr
Additional arguments.}
}
\value{
\link[mlr3]{TaskClassif}.
}
\description{
Convert object to a \link{TaskClassif}.
This is a S3 generic for \link{TaskClassifST}.
}
\seealso{
\code{\link[mlr3:as_task_classif]{mlr3::as_task_classif()}}
}
