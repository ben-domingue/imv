\name{imvglm}
\alias{imvglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
IMV across two models.
}
\description{
This computes the IMV associated with predictions from 'm2' relative to
those from 'm1'. 
}
\usage{
imvglm(m1, m2, nfold = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m1}{
A baseline model returned from 'glm'.
}
  \item{m2}{
An enhanced model returned from 'glm'. This will need to be based on the
same data as 'm1'.
}
  \item{nfold}{
The number of folds.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector (of length 'nfold) of IMVs computed based on predictions in a
single fold (based on model trained in other folds).
}
\references{
%% ~put references to the literature/web site here ~
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
df<-data.frame(x=x,y=y)
z<-round(df$z)
df$z<-as.factor(ifelse(z<0 | z>2,0,z))
m1<-glm(y~x,df,family="binomial")
m2<-glm(y~x+z,df,family="binomial")
imv0glm(m2)
imvglm.rmvar(m2,var.nm='z')
imvglm(m1,m2)
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
