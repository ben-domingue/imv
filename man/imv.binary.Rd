\name{imv.binary}
\alias{imv.binary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Basic function for computing IMV for binary outcomes.
}
\description{
This will compute the IMV associaetd with predictions 'p1' versus those
of 'p2' for outcomes 'y'.
}
\usage{
imv.binary(y, p1, p2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Outcomes (preferably out-of-sample) for which the IMV is to be computed.
}
  \item{p1}{
A vector of baseline predictions (same length as 'y').
}
  \item{p2}{
A vector of enhanced predictions (same length as 'y'). 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  The scalar IMV for this prediction exercise is returned.
}
\references{
https://osf.io/gu3ap/
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
library(imv)
x<-rnorm(1000)
y<-rbinom(length(x),1,1/(1+exp(-x)))
new.y<-rbinom(length(x),1,1/(1+exp(-x)))
m<-glm(y~x,df,family="binomial")
pr<-predict(m,data.frame(x=x),type='response')
imv.binary(new.y,mean(y),pr) #imv for new set of outcomes
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
