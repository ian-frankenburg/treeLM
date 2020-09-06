#' Represent a fitted regression or classification tree as an ANOVA model
#'
#' @title Parse BART-BMA object into linear model form
#'
#' @description
#' The parseTreeSums function transforms the fitted BART-BMA object into a set of
#' linear models as follows from section 3.1 in Hernandez, Belinda & Raftery, Adrian & Pennington, Stephen & Parnell, Andrew. (2017). Bayesian Additive Regression Trees using Bayesian Model Averaging. Statistics and Computing. 28. 10.1007/s11222-017-9767-1.
#'
#' @param model a fitted BART-BMA object
#'
#' @return Following the notation in section 3.1
#' \itemize{
#'     \item O - list of stacked terminal node parameters for trees within the sum
#'     \item W - the big ANOVA design matrix combining all the individual ANOVAs to represent the sum-of-trees model
#'     \item anovas - a list comprising of the individual design matrices that make up W
#' }
#'
#' @author Ian Frankenburg
#'

parseTreeSums <- function(bart_bma){
  foo <- bart_bma$sumoftrees
  bar <- bart_bma$obs_to_termNodesMatrix
  Js <- leafParameters <- W <- O <- list()
  w <- 0
  i=j=m=1
  for(i in 1:length(bar)){
    # get a single sum of trees ie a single set of trees
    foo2 <- foo[[i]]
    # get set of matrices representing tree topologies within sum
    bar2 <- bar[[i]]
    # create list to store multiple sums of trees
    holder <- list()
    holder2 <- c()
    for(j in 1:length(bar2)){
      # get individual tree within a single sum of trees
      mat <- bar2[[j]]
      # get leaf node identifiers
      tnodes <- as.data.frame(foo2[[j]])
      # gets leaves
      leaflocs <- which(tnodes$status==-1)
      # get leaf nodes parameters
      leaf.mu <- tnodes$mean[leaflocs]
      tnodes <- rownames(tnodes[leaflocs, ])
      # X will be ANOVA matrix
      J <- matrix(0, nrow=nrow(mat), ncol=length(tnodes))
      colnames(J) <- tnodes
      for(m in 1:nrow(mat)){
        path <- mat[m,]
        path <- path[which(path!=0)]
        for(l in 1:(length(path))){
          if(as.character(path[l]) %in% tnodes){
            J[m, as.character(path[l])] <- 1
          }
        }
      }
      holder[[j]] <- J
      holder2[[j]] <- leaf.mu
      # get terminal node predicted values
    }
    Wmat <- c()
    for(k in 1:length(holder)){
      Wmat <- cbind(Wmat,matrix(unlist(holder[k]), nrow=nrow(J), ncol=length(holder2[[k]])))
    }
    W[[i]] <- Wmat
    O[[i]] <- unlist(holder2)
    Js[[i]] <- holder
  }
  return(
    list(
      "anovas" = Js,
      "W" = W,
      "O" = O
    )
  )
}
