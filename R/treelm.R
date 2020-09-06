#' Represent a fitted regression or classification tree as an ANOVA model
#'
#' @title Tree to linear model
#'
#' @description
#' The treelm function transforms the fitted rpart or ctree object into a
#' design matrix representing the tree structure. The default encoding follows from
#' Michael LeBlanc & Robert Tibshirani (1998) Monotone Shrinkage of Trees,
#' Journal of Computational and Graphical Statistics,
#' 7:4, 417-433, DOI: 10.1080/10618600.1998.10474786
#'
#' @param model a fitted rpart object or a fitted ctree model
#'
#' @return A list containing the tree topology and design matrix
#' \itemize{
#'     \item X - the ANOVA design matrix for the linear model
#' }
#'
#' @author Ian Frankenburg
#'
#' @example
#' treefit <- rpart(y ~ x)
#' X <- treelm(fit)$X
#' lmfit <- lm(y ~ X)
#'
#' @import partykit rpart data.tree stringr
#'
#' @export
treelm <- function(model){
  if(class(model)=="rpart"){
   # string parsing might take some time for deep trees!
    root <- as.Node(as.party(model))
    node_questions <- ">=|<"
    rules <- partykit:::.list.rules.party(as.party(model))
    action <- str_extract(node_questions, ">=")
  }else if(class(model)[1]=="constparty"){
    root <- as.Node(model)
    node_questions <- "<=|>"
    rules <- partykit:::.list.rules.party(model)
    action <- str_extract(node_questions, "<=")
  }else{
    throw("Must supply rpart or ctree fitted model")
  }
  # for rpart object, splitting node contains question ">=" or "<"
  # ctree object, splitting node contains question "<=" or ">"
  # left is yes decision (1); right is no (-1)
  splitting.rules <- str_extract_all(as.vector(rules), node_questions)
  leaves <- as.numeric(matrix(as.data.frame(root$leaves)))
  internal_nodes <- setdiff(1:root$totalCount,leaves)
  # build matrix representing tree topology
  # rows are leaf nodes; columns represent corresponding branch path
  tree_as_matrix <- data.frame(matrix(0, ncol = length(internal_nodes),
                                      nrow = length(leaves)))
  colnames(tree_as_matrix) <- internal_nodes
  rownames(tree_as_matrix) <- leaves
  leafPath <- list()
  # build list of branching paths to each leaf
  for(i in 1:length(leaves)){
    pathStr <- FindNode(root,leaves[i])$path
    leafPath[[i]] <- pathStr[-length(pathStr)]
  }
  # for each leaf, indicate branch path through series of left & right moves
  # -1 indicates left move; 1 indicates right move
  for(j in 1:nrow(tree_as_matrix)){
    leafNodePath <- unlist(splitting.rules[j])
    tree_as_matrix[j, leafPath[[j]]] <- ifelse(leafNodePath==action, -1, 1)
  }
  X <- tree_as_matrix[as.character(root$fitted$`(fitted)`), ]
  rownames(X) <- 1:nrow(X)
  return(list(
    "X"= X
  ))
}
