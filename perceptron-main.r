## This R code was kindly supplied by Søren Højsgaard, Aalborg University.
## © 2023 Søren Højsgaard <sorenh@math.aau.dk>

fork <- function(x, wgt){
    z <- sum(x * wgt)
    if (z > 0) return(2) else return(1)
}

update_wgt_worker <- function(wgt, r, ej, xj){
    if (ej != 0){
        ## cat("wgt before:"); print(wgt)
        for (i in 1:length(xj)){
            wgt[i] <- wgt[i] + r * ej * xj[i]
        }
        ## cat("wgt after:"); print(wgt)
    }
    wgt
}

update_wgt <- function(X, cls, r=1, wgt=NULL){
    if (is.null(wgt))
        wgt <- rep(0, ncol(X))
    e_sum <- 0
    wgt_chg <- 0
    for (j in 1:nrow(X)){
        xj <- X[j,]
        dj <- cls[j]
        yj <- fork(xj, wgt)
        ej <- dj - yj
        e_sum <- e_sum + abs(ej)
        wgt_new <- update_wgt_worker(wgt, r, ej, xj)
        wgt_chg <- wgt_chg + sum((wgt_new - wgt)^2)
        wgt <- wgt_new
    }
    e_rel     <- e_sum / nrow(X)
    wgt_chg_rel <- wgt_chg / nrow(X)

    list(wgt=wgt_new, e_sum=e_sum, wgt_chg=wgt_chg, e_rel=e_rel, wgt_chg_rel=wgt_chg_rel)
}

pred_fun <- function(X, wgt){
    prd <- apply(X, 1, FUN=fork, wgt)
    unname(prd)
}



perceptron_worker <- function(X, cls, rate=1, maxit=20, wgt=NULL, print=FALSE){
    if (is.null(wgt))
        wgt <- rep(0, ncol(X))
    rr <- rate
    it <- 0
    repeat{
        out <- update_wgt(X, cls, rr, wgt)
        it <- it + 1
        if (print){
            cat(sprintf("iter: %3.0f e_sum: %3.0f wgt_chg: %8.3f e_rel: %8.5f wgt_chg_rel : %8.6f\n",
                        it, out$e_sum, out$wgt_chg, out$e_rel, out$wgt_chg_rel))
        }
        wgt <- out$wgt

        if ((out$e_rel < 1e-3) || (it == maxit))
            break
    }
    out$X <- X
    out
}


#' @title Perceptron algorithm
#'
#' @description Perceptron algorithm - implemented for didactic purposes
#'
#' @param X A matrix or data frame or matrix containing the explanatory
#'           variables.
#'
#' @param grouping A factor specifying the class for each observation.
#'
#' @param rate Learning rate.
#' @param maxit Maximum number of iterations.
#' @param scale Should data be scaled to have zero mean and variance one.
#' @param wgt Initial value of wgt (regression coefficients)
#' @param print Print fitting info.
#'
#' @examples
#'
#' ## This will fail because iris data has three groups.
#' \dontrun{
#' X <- iris[1:4,]
#' grp <- iris$Species
#' perceptron(X, grp)
#' }
#' 
#' ## Classes er perfectly separable
#' iris_sv <- subset(iris, Species %in% c("setosa", "versicolor"))
#' pairs(iris_sv, col=iris_sv$Species)
#' 
#' X <- iris_sv[, 1:4]
#' grp <- factor(iris_sv$Species)
#' 
#' p <- perceptron(X, grp)
#' pred <- predict(p)$class
#' table(grp, pred)
#' 
#' ## Classes er not perfectly separable
#' iris_vv <- subset(iris, Species %in% c("versicolor", "virginica"))
#' pairs(iris_vv, col=iris_vv$Species)
#' 
#' X <- iris_vv[, 1:4]
#' grp <- factor(iris_vv$Species)
#' 
#' p <- perceptron(X, grp)
#' pred <- predict(p)$class
#' table(grp, pred)
#' 
#' @export
perceptron <- function(X, grouping, rate=1, maxit=99, scale=TRUE, wgt=NULL, print=FALSE){
    grouping_factor <- factor(grouping)
    if (length(levels(grouping_factor))!=2)
        stop("grouping must have two levels.\n")
    
    grouping01 <- as.numeric(grouping_factor)
    if (scale){
        if (print) cat("Scaling and centering data\n")
        X <- scale(X, center=TRUE, scale=TRUE)
    }
    ce <- attr(X, "scaled:center")
    sc <- attr(X, "scaled:scale")

    percep <- perceptron_worker(X, grouping01, rate=rate, maxit=maxit, wgt=wgt, print=print)
    percep$scale <- list(center=ce, scale=sc)
    percep$grouping01 <- grouping01
    percep$grouping <- grouping
    percep$label <- levels(grouping_factor)
    class(percep) <- "percep"    
    percep
}

#' @export
predict.percep <- function(object, newdata, ...){
    if (missing(newdata)){
        newdata <- object$X
    } else {
        sc <- object$scale
        if (!is.null(sc$center))
            newdata <- sweep(newdata, 2, sc$center, FUN="-")
        if (!is.null(sc$scale))
            newdata <- sweep(newdata, 2, sc$scale, FUN="/")        
    }

    pred_num <- pred_fun(newdata, object$wgt)
    pred_num
    class <- factor(object$label[pred_num])
    list(class=class)
}
