

#'Function for estimating period life tables
#'
#'@param x a numeric vector of ages
#'@param nMx a numeric vector of crude death rates at age x
#'@param n width of the intervals

#' @return A period life table given ages \code{x}     and death rates \code{nMx}.

#' @examples
#' x <- c(0,1,5,10,15,20,25,35,45,55,65,75,85)
#' females <- read.table(file=
#'"http://www.demog.berkeley.edu
#'/~eddieh/AppliedDemographyToolbox
#'/StanfordCourseLifeTable,
#'/StanfordCourseMortalityData.csv"
#'header=TRUE,sep=",")
#'nMx <- females$Death.Count / females$Population
#'lifetable(x,nMx)
#'
#'@export
life_table <- function( x, nMx){
  b0 <- 0.07;   b1<- 1.7;
  nmax <- length(x)
  n <- c(diff(x),999)
  nax <- n / 2;
  nax[1] <- b0 + b1 *nMx[1]
  nax[2] <- 1.5  ;
  nax[nmax] <- 1/nMx[nmax]
  nqx <- (n*nMx) / (1 + (n-nax)*nMx)
  nqx<-ifelse( nqx > 1, 1, nqx);
  nqx[nmax] <- 1.0
  lx <- c(1,cumprod(1-nqx)) ;
  lx <- lx[1:length(nMx)]
  ndx <- lx * nqx ;
  nLx <- n*lx - nax*ndx;
  nLx[nmax] <- lx[nmax]*nax[nmax]
  Tx <- rev(cumsum(rev(nLx)))
  ex <- ifelse( lx[1:nmax] > 0, Tx/lx[1:nmax] , NA);
  lt <- data.frame(x=x,nax=nax,nmx=nMx,nqx=nqx,lx=lx,ndx=ndx,nLx=nLx,Tx=Tx,ex=ex)
  return(lt)
}



