plotMRE <- function(results, absolute=FALSE, parameter){


    color <- c(gray(0.2), "tomato")
        if(parameter=="msy"){
          x <- results$msy_est
          re_x <- (x-msy_true)/msy_true
        }

        if(parameter=="u"){
          x <- results$c_rep[,,datyrs]/results$b_est[,,datyrs]
          y <- results$c_true[,,datyrs]/results$b_true[,,datyrs]
          re_x <- (x - y)/y
        }

        if(parameter=="B"){
          x <- results$b_est[,,datyrs]
          y <- results$b_true[,,datyrs]
          re_x <- (x - y)/y
        }

        if(parameter=="K"){
          x <- results$K_est
          y <- K_true
          re_x <- (x - y)/y
        }


    if(absolute==TRUE) ylim <- c(-0.2, 1.1)
    if(absolute==FALSE) ylim <- c(-1, 1)

    par(mfcol=c(2,5), mar=c(0,0,0,0), omi=c(1,1,1,1))
        for(mod in 1:10){
          plot(x=1, y=1, type="n", xlim=c(0, max(nrow(x))), ylim=ylim, 
              xaxs="i", yaxs="i", ann=F, xaxt="n", yaxt="n")
          if(mod %in% c(1,2)) axis(2, pretty(c(-1,1)), cex.axis=2, las=2)
          if(mod %in% seq(2,10,by=2)) axis(1, pretty(c(0,max(nrow(x)))), cex.axis=2)
          if(mod==1) mtext(rmodel_vec_names[1], side=3, font=2, cex=1.5, line=2)
          if(mod==3) mtext(rmodel_vec_names[2], side=3, font=2, cex=1.5, line=2)
          if(mod==5) mtext(rmodel_vec_names[3], side=3, font=2, cex=1.5, line=2)
          if(mod==7) mtext(rmodel_vec_names[4], side=3, font=2, cex=1.5, line=2)
          if(mod==9) mtext(rmodel_vec_names[5], side=3, font=2, cex=1.5, line=2)

          if(absolute==TRUE) show <- abs(re_x)
          if(absolute==FALSE) show <- re_x
          for(iter in 1:nrow(x)){
              points(x=iter, y=median(show[1:iter,mod]), col=color[1])
          }
        }
    
    mtext("Iteration", side=1, line=4, font=2, cex=1.7, outer=TRUE)
    if(absolute==FALSE) mtext("Mean Relative Error", side=2, line=4.5, font=2, cex=1.7, outer=TRUE)
    if(absolute==TRUE) mtext("Mean Absolute Relative Error", side=2, line=4.5, font=2, cex=1.7, outer=TRUE)

}