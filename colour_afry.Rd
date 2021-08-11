\name{colour_afry}
\alias{colour_afry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  Apply AFRY Colour Scheme onto Charts and Graphs
}
\description{
%%  This function allows the AFRY color scheme to be applied to a plot or a chart. The priority of color choice goes from Dark Blue - Dark Green - Dark Brown - Grey -Blue - Green - Brown - Light Blue - Light Green - Light Brown.
\usage{
colour_afry(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
%%     Number of desired sections of different color to appear on graph.
}
}
\details{
%%
}
\value{
%%  Color palette of ten AFRY standard colors.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  Daniel Lee, Rita Razausakite
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

pal2 <- colour_afry(n=10)
par(mar = rep(0, 4))
pie(rep(1, length(pal2)), col = pal2)

n = 4
cols = colour_afry(n)
dev.new(width = 4, height = 4)
plot(1:n, pch = 16, cex = 2, col = cols)
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
