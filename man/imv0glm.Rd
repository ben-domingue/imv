\name{imv0glm}
\alias{imv0glm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
IMV for the argument 'm' compared to prediction based on prevalence alone.
}
\description{
This function computes the IMV based on predictions in an in-sample fold
(based on a training model constructed in out-of-sample folds) as
compared to a static prediction based on a null model (i.e., the
outcome's mean in the out-of-sample folds). 
}
\usage{
imv0glm(m, nfold = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
A 'glm' object (with restrictions, see below).
}
  \item{nfold}{
Number of folds to use for cross-validation.
}
}
\details{
This function will only work at present for objected retuned by fairly straightforward calls
to 'glm'. For example, if expressions use modifiers like 'factor' in the
call, this will result in an error.
}
\value{
A vector of length 'nfold' with the IMV value computed from each
fold. The mean of this vector is useful as an overall summary while the
SD of these values offers information on sampling-related uncertainty.
}
\references{
See https://osf.io/preprints/socarxiv/gu3ap/
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x<-rnorm(1000)
y<-rbinom(length(x),1,1/(1+exp(-x)))
df<-data.frame(x=x,y=y)
m<-glm(y~x,df,family="binomial")
imv0glm(m) #imv for full model versus null model
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
