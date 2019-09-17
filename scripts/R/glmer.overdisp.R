#' over dispersion check function
#'
#' PChecks for overdispersion in a poisson glmm (glmer)
#'
#' @model glmer model
#' @example
#' DF <- data.frame(dv=sample(100,200,T),iv=as.factor(c(rep("A",100),rep("B",100))),random=as.factor(1:100))
#' fm <- glmer(dv~iv+(1|random),DF,family=poisson)
#' glmer.overdisp(fm)
glmer.overdisp <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
