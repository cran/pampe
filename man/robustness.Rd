\name{robustness}
\encoding{ascii}
\alias{robustness}
\alias{plot.robustness}
\title{
Robustness check for the Implementation of the Panel Data Approach for Program Evaluation
}
\description{
Implements a leave-one-out robustness check for the Panel Data Approach for program evaluation as developed in Hsiao, Steve Ching and Ki Wan (2012). Robustness must be run after pampe and will automatically and iteratively remove each one of the controls resulted in pampe to check whether the results obtained in pampe are robust or are driven by a single control}
\usage{
robustness(pampe.object, time.pretr, time.tr, treated, data)
data(growth)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pampe.object}{
The object resulting from previously running the pampe function.
}
  \item{time.pretr}{
The pre-treatment periods, up to the introduction of the treatment. For example, if you have 10 pre-treatment periods and the treatment was introduced in the 11th period, this argument could be 1:10.
}
  \item{time.tr}{
The treatment period plus the periods for which you want to check the effect. For example, if the treatment was introduced in the 11th period and you want results for 9 more periods, it would be 11:20.
}
  \item{treated}{
The treated unit, the unit that receives the intervention. It can be a name or the index of the column if the columns in the data matrix are named (recommended). For example, in the case of the growth data included in the package, the name of the treated unit is "HongKong", and it is in the first column of the data matrix, so this argument can be either "HongKong" or 1.

}
  \item{data}{
The name of the data matrix to be used, e.g. growth. The data matrix should be in standard cross-sectional format: with the variables (the controls/units in the donor pool pool act as explanatory variables) extending across the columns and the quarters (time-periods) extending across rows. It is important for the user to have his or her data in this standard format to correctly apply the function. Naming the rows and especially the columns is also strongly recommended. Be careful to avoid characters such as spacing when naming.
}
}
\author{
Ainhoa Vega-Bayo
}
\note{
Check the references in \code{\link{pampe}} for more information on the placebo studies. The leaps package by Lumley is required for pampe to run properly.
}


\seealso{
\code{\link{pampe}}
\code{\link{leaps}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.