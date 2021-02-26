# Difference Plot

diff_plot <- function(dat, cats = NULL, ylim = NULL){
  
  x <- 0:length(dat$Y1)
  y1 <- cumsum(c(0, dat$Y1)) / sum(dat$Y1)
  y2 <- cumsum(c(0, dat$Y2)) / sum(dat$Y2) 
  y <- y1 - y2
  col <- rep("white", length(x))
  col[c(1, length(col))] <- "black"
  
  p <- ggplot(data.frame(x, y), aes(x, y)) +
    geom_hline(yintercept = 0, linetype = "dashed", lwd = 1) +
    geom_line(lwd = .75) + 
    geom_point(shape = 21, size = 4, fill = col) +
    labs(x = "Category", y = "Diff. in Cumulatives", title = "Difference Plot") +
    theme_classic() +
    coord_capped_cart(bottom='both', left = "both") +
    theme(axis.ticks.length = unit(.2, "cm"),
          axis.text = element_text(color = "black",
                                   size = 12),
          axis.title = element_text(size = 12),
          title = element_text(size = 12))
  
  if(!is.null(cats)){
    p <- p + 
      scale_x_continuous(labels = c("N/A", cats)) +
      theme(axis.text.x = element_text(angle = 45, 
                                       vjust = 1,
                                       hjust = 1))
  }
  
  if(!is.null(ylim))
    p <- p + ylim(ylim)
  
  return(p)
}


# ROC Plot

roc_plot <- function(dat){
  
  y <- cumsum(c(0, dat$Y1)) / sum(dat$Y1)
  x <- cumsum(c(0, dat$Y2)) / sum(dat$Y2) 
  col <- rep("white", length(x))
  col[c(1, length(col))] <- "black"
  
  ggplot(data.frame(x, y), aes(x, y)) +
    geom_abline(slope = 1, linetype = "dashed", lwd = 1) +
    geom_line(lwd = .75) + 
    geom_point(shape = 21, size = 4, fill = col) +
    scale_x_continuous(breaks = seq(0, 1, .2)) +
    scale_y_continuous(breaks = seq(0, 1, .2)) +   
    labs(x = "Condition 2", y = "Condition 1", title = "ROC Plot") +
    theme_classic() +
    coord_capped_cart(bottom="both", left = "both") +
    theme(axis.ticks.length = unit(.2, "cm"),
          axis.text = element_text(color = "black",
                                   size = 12),
          axis.title = element_text(size = 12),
          title = element_text(size = 12))
}

# Cumulatives Plot

cum_plot <- function(x, y){
  
  cowplot::plot_grid(x, y, nrow = 2)
  
}



# MCMC Plot

mcmc_plot <- function(x){
  
  alpha <- x$alpha
  theta <- x$theta
  
  par(mfrow=c(1,2))
  
  matplot(alpha, type = "l"
          , las = 1
          , xlab = "Posterior Samples"
          , ylab = "Value"
          , cex.lab = 1
          , cex.axis = 1
          , cex.main = 1.5
          , main = expression(alpha)
          , lwd = 2)
  abline(h = colMeans(alpha)
         , lty = "dashed"
         , col = 1:ncol(alpha)
         , lwd = 2)
  
  matplot(theta, type = "l"
          , las = 1
          , xlab = "Posterior Samples"
          , ylab = "Value"
          , cex.lab = 1
          , cex.axis = 1
          , cex.main = 1.5
          , lwd = 2
          , main = expression(theta))
  abline(h = colMeans(theta)
         , lty = "dashed"
         , col = 1:ncol(theta)
         , lwd = 2)
}
