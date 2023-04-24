plotPara <- function (x,group = 'DMSO') 
{
  fit <- x
  if (!class(fit) == "cellsurvLQfit") 
    stop("Fit object not of class 'cellsurvLQfit'!")
  data <- fit$data
  data$dose2 <- data$dose^2
  data$lcells <- log(data$ncells)
  doses <- unique(data$dose)
  b <- fit$coef[c("dose", "dose2")]
  
  if (0 %in% doses) {
    S0 <- pes(data)$S0
    names(S0) <- rownames(pes(data))
    meanSF <- sfpmean(data, S0)
  }
  if (!(0 %in% doses)) {
    data$pe <- exp(data$logPle)
    meanSF <- sfpmean(data)
  }
  pts <- meanSF[1, ]
  sems <- meanSF[2, ]
  
  para = list(rad_p = b, pd = data.frame(x = doses,st = pts,sd = sems,treat = group))
  
  return(para)
}
