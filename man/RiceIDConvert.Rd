% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RiceIDConvert.R
\name{RiceIDConvert}
\alias{RiceIDConvert}
\title{Convert Biological ID from One to Another for Oryza Sativa}
\usage{
RiceIDConvert(myID, fromType, toType)
}
\arguments{
\item{myID}{A vector of entrez  id}

\item{fromType}{Keytype of input  id}

\item{toType}{Keytype of output  id}
}
\value{
Return a vector or a datafram
}
\description{
\code{RiceIDConverter} Convert One Biological ID to Another for Oryza Sativa, such As SYMBOL to TRANSCRIPTID.
}
\examples{
convert_id <- RiceIDConvert('Os01g0100500','RAP',toType = 'MSU')
convert_id <- RiceIDConvert(myID = 'Os01g0100500',
                            fromType = 'RAP',
                            toType = 'SYMBOL')
}
\author{
Xiang LI <ynaulx@gmail.com>
}
