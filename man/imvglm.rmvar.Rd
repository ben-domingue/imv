\name{imv.glm.rmvar}
\alias{imv.glm.rmvar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
IMV based on full model versus model without 'var.nm'
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
imv.glm.rmvar(m, nfold = 5, var.nm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
A 'glm' object.
}
  \item{nfold}{
Number of folds.
}
  \item{var.nm}{
The variable to be removed from the expression in 'm'. 
}
}
\details{
  Note that 'var.nm' needs to be exactly as used in the call to 'glm'.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
x<-rnorm(1000)
y<-rbinom(length(x),1,1/(1+exp(-x)))
df<-data.frame(x=x,y=y)
df$z<-rnorm(nrow(df))
m<-glm(y~x+z,df,family="binomial")
imv.glm.rmvar(m,var.nm='z')
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
