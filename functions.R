#### Basic Stats ####
## convert r to Cohen's D
r2d = function(r){
  d = sqrt((4*(r^2))/(1-r^2))
  return(d)
}

##extracts standard deviation table for DV (either 1 variable or a vector of variables)--for more information on how tapply works, use the R help or RStudio help menu
##--DV can be a single variable or a data.frame/matrix of multiple variables 
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Block)
sd.function = function(data, DV, IV){
	sd=with(data, tapply(DV, IV, sd))
	return(sd)
}

sd.function.na = function(data, DV, IV){
  sd=with(data, tapply(DV, IV, sd, na.rm=T))
  return(sd)
}


##extracts standard error table for DV (either 1 variable or a set of variables)
##--to use with bar_graph.se, set function equal to er
##  eg. er=se.function()
##--DV can be a single variable or a data.frame/matrix of multiple variables 
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Load)
se.function=function(data, DV, IV){
	sd=with(data, tapply(DV, IV, sd))
	length=with(data, tapply(DV, IV, length))
  #length is determining the n of the data
	er=sd/sqrt(length)
	return(er)
}

se.function.na=function(data, DV, IV){
  sd=with(data, tapply(DV, IV, sd, na.rm=T))
  length=with(data, tapply(DV, IV, length))
  #length is determining the n of the data
  er=sd/sqrt(length)
  return(er)
}

##extracts mean table for DV (either 1 variable or a set of variables)
##--to use with bar_graph.se, set function equal to means
##  eg. means=means.function()
##--DV can be a single variable or a data.frame/matrix of multiple variables 
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Load)
means.function = function(data, DV, IV){
	means=with(data, tapply(DV, IV, mean))
	return(means)
}

means.function.na = function(data, DV, IV){
  means=with(data, tapply(DV, IV, mean, na.rm=T))
  return(means)
}

##extracts median table for DV (either 1 variable or a set of variables)
##--to use with bar_graph.se, set function equal to medians
##  eg. medians=med.function()
##--DV can be a single variable or a data.frame/matrix of multiple variables 
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Load)
med.function = function(data, DV, IV){
  means=with(data, tapply(DV, IV, median))
  return(means)
}

med.function.na = function(data, DV, IV){
  means=with(data, tapply(DV, IV, median, na.rm = T))
  return(means)
}

##extracts IQR table for DV (either 1 variable or a set of variables)
##--to use with bar_graph.se, set function equal to interquartile range
##  eg. IQR=IQR.function()
##--DV can be a single variable or a data.frame/matrix of multiple variables 
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Load)
IQR.function = function(data, DV, IV){
  means=with(data, tapply(DV, IV, IQR))
  return(means)
}

IQR.function.na = function(data, DV, IV){
  means=with(data, tapply(DV, IV, IQR, na.rm = T))
  return(means)
}

##extracts range table for DV (either 1 variable or a set of variables)
##--to use with bar_graph.se, set function equal to interquartile range
##  eg. IQR=IQR.function()
##--DV can be a single variable or a data.frame/matrix of multiple variables 
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Load)
range.function = function(data, DV, IV){
  ranges=with(data, tapply(DV, IV, range))
  return(ranges)
}

range.function.na = function(data, DV, IV){
  ranges=with(data, tapply(DV, IV, range, na.rm=T))
  return(ranges)
}

##extracts 95% confidence intervals for on DV overall or for levels of a grouping variable based on the t distribution (smaller sample sizes--if over 100 obs than need to use Z)
##IV=0, levelnum=0 means want overall CI for DV
##IV=factor, levelnum=0 means want CI for DV for each level of factor separately
##IV=factor, levelnum=# means want CI for DV for specific level of factor 
##  --to see factor level number: levels(factorname)
CI_95.function = function(data, DV, IV, levelnum){

  if(length(IV)==1){
    stat=qt(0.975,df=(nrow(data)-1))
    error=(sd(DV)/sqrt(length(DV)))*stat
    right=as.numeric(mean(DV))+error
    left=as.numeric(mean(DV))-error
    CI_stat=concatonate_2col(round(left, 3),round(right, 3), "CI")
    output=CI_stat[1]
  }
  else {
    se=se.function(data, DV, IV)
    mean=means.function(data,DV,IV)
    if(levelnum==0){
      nlevel=length(levels(IV))
      levelname=c(levels(IV))
      CI=rep(0, nlevel)
      output=rbind(levelname, CI)
      for(i in 1:nlevel){
        level_data=data[which(IV==levels(IV)[i]), ]
        stat=qt(0.975,df=(nrow(level_data)-1))
        error=as.numeric(se)[i]*stat
        right=as.numeric(mean)[i]+error
        left=as.numeric(mean)[i]-error
        CI_stat=concatonate_2col(round(left, 3),round(right, 3), CI)
        output[2,i]=CI_stat[1]
      }
    }
    else {
      level_data=data[which(IV==levels(IV)[levelnum]), ]
      
      stat=qt(0.975,df=(nrow(level_data)-1))
      error=as.numeric(se)[levelnum]*stat
      right=as.numeric(mean)[levelnum]+error
      left=as.numeric(mean)[levelnum]-error
      CI_stat=concatonate_2col(round(left,3),round(right,3), CI)
      output=CI_stat[1]
    }
  }
return(output)
}

#produces a matrix of pvalues from two-sided t-test (no correction, then bonferoni correct)

pvalue.matrix_none=function(var_vector, var_names){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  p_num=((l-1)/2)
  
  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      res=t.test(x,y,alternative='two.sided')$p.value
      
      p_res=res
      p_res=round(p_res, digits=3)
      
      if (p_res>0.150){
        p_res="NA"
      }
      if (p_res<0.001){
        p_res="<0.001"
      }
      
      res_matrix[icount,jcount]=p_res
    }
    
  }
  
  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- "" 
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

pvalue.matrix_bon=function(var_vector, var_names){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  p_num=((l-1)/2)
  
  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      res=t.test(x,y,alternative='two.sided')$p.value
      
      p_res=p.adjust(res, method = "bonferroni", n = (p_num))
      p_res=round(p_res, digits=3)
      
      if (p_res>0.150){
        p_res="NA"
      }
      if (p_res<0.001){
        p_res="<0.001"
      }
      
      res_matrix[icount,jcount]=p_res
    }
    
  }
  
  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- "" 
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix_0.10=function(var_vector, var_names){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  
  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      
      p_res=round(cor.test(x,y, na.rm=TRUE)$p.value, 3)
      c_res=round(cor.test(x,y, na.rm=TRUE)$estimate, 2)
      
      if (p_res>0.050){
        c_res="NA"
      }
      
      res_matrix[icount,jcount]=c_res
      
    }
    
  }
  
  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- "" 
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix_0.05=function(var_vector, var_names){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  
  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      
      p_res=round(cor.test(x,y, na.rm=TRUE)$p.value, 3)
      c_res=round(cor.test(x,y, na.rm=TRUE)$estimate, 2)
      
      if (p_res>0.050){
        c_res="NA"
      }
      
      res_matrix[icount,jcount]=c_res
      
    }
    
  }
  
  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- "" 
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}


##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix_Method_0.05=function(var_vector, var_names, method){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  
  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      
      p_res=round(cor.test(x,y, na.rm=TRUE, method = method)$p.value, 3)
      c_res=round(cor.test(x,y, na.rm=TRUE, method = method)$estimate, 2)
      
      if (p_res>0.050){
        c_res="NA"
      }
      
      res_matrix[icount,jcount]=c_res
      
    }
    
  }
  
  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- "" 
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix=function(var_vector, var_names){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  
  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      
      p_res=round(cor.test(x,y, na.rm=TRUE)$p.value, 3)
      c_res=round(cor.test(x,y, na.rm=TRUE)$estimate, 2)
      
      res_matrix[icount,jcount]=c_res
      
    }
    
  }
  
  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- "" 
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix_ps=function(var_vector, var_names){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  
  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      
      p_res=round(cor.test(x,y, na.rm=TRUE)$p.value, 3)
      c_res=round(cor.test(x,y, na.rm=TRUE)$estimate, 2)
      
      res_matrix[icount,jcount]=p_res
      
    }
    
  }
  
  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- "" 
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix_Method=function(var_vector, var_names, method){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  
  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      
      p_res=round(cor.test(x,y, na.rm=TRUE, method=method)$p.value, 3)
      c_res=round(cor.test(x,y, na.rm=TRUE, method=method)$estimate, 2)
      
      res_matrix[icount,jcount]=c_res
      
    }
    
  }
  
  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- "" 
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}



#### Clustering Stats ####
##code from https://github.com/GladeLiu/MultivariateStatistics/blob/master/ClusterNum.R
##SPRSQ=semipartial R-squared; PSF=sudo F-statistic, PST2=psudo T2 
##enter X as data, not hclust
ClusterNum_scale<-function(X,d="euclidean",method="ward.D2",hclust=NULL){
  #X is required
  if(class(hclust)=="NULL"){ ##do clustering on data if not hclust
    X<-data.frame(scale(X));
    d<-dist(X,d);
    hc<-hclust(d,method);
    N<-dim(X)[1];
  }else if(class(hclust)=="hclust"){ ##if X is hclust
    hc<-hclust;           ##hc=hclust results (X)
    N<-length(hc$order);  ##permutation of the original observations suitable for plotting
  }else{
    stop("The class of hclust is wrong!");
  }  
  R2<-seq(0,1,length=N);      ##make = to zero?
  PSF<-rep(0,N); PST2<-PSF;  ##make two vectors of length N filled with zeros
  T<-sum(diag(cov(X)*(N-1))); ##
  Nm<-rep(0,N-1);Wm<-rep(0,N-1);##make two zero vectors
  pdf("cluster");plot(1,type="n"); ##pdf export of cluster
  rect0<-rect.hclust(hc,k=2);
  for(t in 2:(N-1)){
    rect<-rect0;
    if(t<N-1){
      rect0<-rect.hclust(hc,k=t+1);
    }
    b<-colMeans(X[rect[[1]],])-colMeans(X);
    B<-length(rect[[1]])*t(b)%*%b;
    for(j in 1:t){
      if(length(rect[[j]])!=length(rect0[[j]])){
        break;}
    }
    Nm[t]<-length(rect[[j]]);
    Wm[t]<-sum(diag(cov(X[rect[[j]],])*(length(rect[[j]])-1)));
    for(i in t:2){
      b<-colMeans(X[rect[[i]],])-colMeans(X);
      B<-B+length(rect[[i]])*t(b)%*%b;
    }
    R2[t]<-B/T;
    PSF[t]<-(B*(N-t))/((T-B)*(t-1));
  }
  dev.off();
  R1<-R2[-1];SPRSQ<-matrix(R1-R2[1:(N-1)]);
  Bkl<-SPRSQ*T;
  PST2<-(Nm-2)*Bkl/(Wm-Bkl);
  PST2[1]<-(N-2)*Bkl[1]/(T-Bkl[1]);
  PST2[c(N-1,N-2)]<-0;
  data.frame(RSQ=R2[1:(N-1)],SPRSQ,PSF=PSF[1:(N-1)],PST2);
}

ClusterNum_notscale<-function(X,d="euclidean",method="ward.D2",hclust=NULL){
  #X is required
  if(class(hclust)=="NULL"){ ##do clustering on data if not hclust
    X<-data.frame(X);
    d<-dist(X,d);
    hc<-hclust(d,method);
    N<-dim(X)[1];
  }else if(class(hclust)=="hclust"){ ##if X is hclust
    hc<-hclust;           ##hc=hclust results (X)
    N<-length(hc$order);  ##permutation of the original observations suitable for plotting
  }else{
    stop("The class of hclust is wrong!");
  }  
  R2<-seq(0,1,length=N);      ##make = to zero?
  PSF<-rep(0,N); PST2<-PSF;  ##make two vectors of length N filled with zeros
  T<-sum(diag(cov(X)*(N-1))); ##
  Nm<-rep(0,N-1);Wm<-rep(0,N-1);##make two zero vectors
  pdf("cluster");plot(1,type="n"); ##pdf export of cluster
  rect0<-rect.hclust(hc,k=2);
  for(t in 2:(N-1)){
    rect<-rect0;
    if(t<N-1){
      rect0<-rect.hclust(hc,k=t+1);
    }
    b<-colMeans(X[rect[[1]],])-colMeans(X);
    B<-length(rect[[1]])*t(b)%*%b;
    for(j in 1:t){
      if(length(rect[[j]])!=length(rect0[[j]])){
        break;}
    }
    Nm[t]<-length(rect[[j]]);
    Wm[t]<-sum(diag(cov(X[rect[[j]],])*(length(rect[[j]])-1)));
    for(i in t:2){
      b<-colMeans(X[rect[[i]],])-colMeans(X);
      B<-B+length(rect[[i]])*t(b)%*%b;
    }
    R2[t]<-B/T;
    PSF[t]<-(B*(N-t))/((T-B)*(t-1));
  }
  dev.off();
  R1<-R2[-1];SPRSQ<-matrix(R1-R2[1:(N-1)]);
  Bkl<-SPRSQ*T;
  PST2<-(Nm-2)*Bkl/(Wm-Bkl);
  PST2[1]<-(N-2)*Bkl[1]/(T-Bkl[1]);
  PST2[c(N-1,N-2)]<-0;
  data.frame(RSQ=R2[1:(N-1)],SPRSQ,PSF=PSF[1:(N-1)],PST2);
}


#### Graphing ####

##make bar graph with standard error bars and is designed to be used in congunction with the means and se functions above.  In this case it will only work if your DV vector has 2 or less variables.  If graphing a 3-way interaction see other function for splitting data sets by factors

##--group=0 if only have 1 DV, if DV is multiple variables, group is the variable name for the grouping one
##if group =! 0, it means you have two DV's/a DV and a covariate. The first variable listed in your DV vector will be represtened by different colors in the legend. This will be the "group" variable and will create side by side bars. The second variable will have levels represented on x-axis. note: xpd=False restrains bars to graphic pane (can truncate lower part of graph)
bar_graph.se = function(means, er, xlab, ylab, ymax, ymin, group){
  if (group==0) {
    barx<-barplot(means, col="cornflowerblue", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE)
    axis(2)
    axis(1, at=c(0,7), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
  }
  
  else {
    #palette(c("steelblue4", "lightsteelblue2", "cornflowerblue", "cyan3", "darkcyan", "aquamarine4", "royalblue4","cornflowerblue", "darkturquoise"))
    palette(c("blue", "cadetblue1", "cornflowerblue", "cyan3", "darkcyan", "aquamarine4", "royalblue4","cornflowerblue", "darkturquoise"))
    len=length(levels(group))
    col.list = 1:len
    col.list_dif = 7:9
    par(fig=c(0, 0.8,0,1), mar=c(4,4,4,4))
    barx<-barplot(means, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(45), density=10)
    barx<-barplot(means, add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1)
    #axis(2)
    axis(1, at=c(0,20), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
    #create space for legend
    par(new=T)
    par(fig=c(0, 0.8, 0, 1), mar=c(4, 4, 4, 0))
    plot(5,5,axes=FALSE, ann=FALSE, xlim=c(0,10),ylim=c(0,10), type="n")
    legend("topright", legend=levels(group), fill=c(col.list), bty="n",cex=1.2)
    
  } 
}

bar_graph.se_food = function(means, er, xlab, ylab, ymax, ymin, group){
  if (group==0) {
    par(fig=c(0, 1, 0.2, 1), mar = c(6, 4.1, 4.1, 2.1))
    barx<-barplot(means, col="cornflowerblue", ylab=ylab, xlab="", ylim=c(ymin, ymax), xpd=FALSE, las=3, cex.names=0.8)
    axis(2)
    axis(1, at=c(0,7), labels=FALSE)
    mtext(xlab, side=1, line=7)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.1)
    
  }
  
  else {
    #palette(c("steelblue4", "lightsteelblue2", "cornflowerblue", "cyan3", "darkcyan", "aquamarine4", "royalblue4","cornflowerblue", "darkturquoise"))
    palette(c("blue", "cadetblue1", "cornflowerblue", "cyan3", "darkcyan", "aquamarine4", "royalblue4","cornflowerblue", "darkturquoise"))
    len=length(levels(group))
    col.list = 1:len
    col.list_dif = 7:9
    par(fig=c(0, 0.8,0,1), mar=c(4,4,4,4))
    barx<-barplot(means, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(45), density=10, las=3)
    barx<-barplot(means, add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, las=3)
    #axis(2)
    axis(1, at=c(0,20), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
    #create space for legend
    par(new=T)
    par(fig=c(0, 0.8, 0, 1), mar=c(4, 4, 4, 0))
    plot(5,5,axes=FALSE, ann=FALSE, xlim=c(0,10),ylim=c(0,10), type="n")
    legend("topright", legend=levels(group), fill=c(col.list), bty="n",cex=1.2)
    
  } 
}

texture_bar_graph.se = function(means, er, xlab, ylab, ymax, ymin, group){
  if (group==0) {
		barx<-barplot(means, col="cornflowerblue", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE)
		barx<-barplot(means, add=TRUE, col="white", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE, lwd=1:2, angle=c(35, 45, 0), density=10)
		barx<-barplot(means, add=TRUE, col="white", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE, lwd=1:2, angle=c(70, 135, 90), density=10)
		#barx<-barplot(means, add=TRUE, col="white", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE, lwd=1:2, angle=c(105, 135, 90), density=10)
		#barx<-barplot(means, add=TRUE, col="white", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE, lwd=1:2, angle=c(140, 135, 90), density=10)
    axis(2)
		axis(1, at=c(0,7), labels=FALSE)
		#this adds the SE wiskers
		arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
	}
	
	else {
    palette(c("blue", "cyan3", "cornflowerblue", "cadetblue1",  "darkcyan", "aquamarine4", "chocolate1", "springgreen3","cornflowerblue"))
	  len=length(levels(group))
    col.list = 1:len
    col.list_dif = c(7, 7, 7, 8, 8, 8,9,9,9)
    par(fig=c(0, 0.8,0,1), mar=c(4,4,4,4))
    barx<-barplot(means, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE)
    barx<-barplot(means,  add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(45, 45), density=8)
    barx<-barplot(means, add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(135, 135), density=8)
   # barx<-barplot(means, add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(105, 135, 90), density=10)
   # barx<-barplot(means, add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(140, 135, 90), density=10)
    #axis(2)
    axis(1, at=c(0,20), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
    #create space for legend
    par(new=T)
    par(fig=c(0, 0.8, 0, 1), mar=c(4, 4, 4, 0))
    plot(5,5,axes=FALSE, ann=FALSE, xlim=c(0,10),ylim=c(0,10), type="n")
    legend("topright", legend=levels(group),  bty="n",cex=1.5, pch(8,5,0))
    legend("topright", legend=levels(group), fill=c(col.list), bty="n",cex=1.5, angle=c(45, 45), density=8)
    legend("topright", legend=levels(group), fill=c(col.list), bty="n",cex=1.5, angle=c(135, 135), density=8)
  #legend("topright", legend=levels(group), fill=c(col.list), bty="n",cex=1.5, angle=c(105, 135, 90), density=8)
  #legend("topright", legend=levels(group), fill=c(col.list), bty="n",cex=1.5, angle=c(140, 135, 90), density=8)
	} 
}

bar_graph.se_legendloc = function(means, er, xlab, ylab, ymax, ymin, group, location){
  if (group==0) {
    barx<-barplot(means, col="cornflowerblue", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE)
    axis(2)
    axis(1, at=c(0,7), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
  }
  
  else {
    palette(c("blue", "cadetblue1", "cornflowerblue", "cyan3", "darkcyan", "aquamarine4", "royalblue4","cornflowerblue", "darkturquoise"))
    len=length(levels(group))
    col.list = 1:len
    col.list_dif = 7:9
    par(fig=c(0, 0.8,0,1), mar=c(4,4,4,4))
    barx<-barplot(means, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(45), density=10, las=3)
    barx<-barplot(means, add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, las=3)
    #axis(2)
    axis(1, at=c(0,20), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
    #create space for legend
    par(new=T)
    par(fig=c(0, 0.8, 0, 1), mar=c(4, 4, 4, 0))
    plot(5,5,axes=FALSE, ann=FALSE, xlim=c(0,10),ylim=c(0,10), type="n")
    legend(location, legend=levels(group), fill=c(col.list), bty="n",cex=1.2)
    
  } 
}

bar_graph.se_legendloc_food = function(means, er, xlab, ylab, ymax, ymin, group, location){
  if (group==0) {
    barx<-barplot(means, col="cornflowerblue", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE)
    axis(2)
    axis(1, at=c(0,7), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
  }
  
  else {
    palette(c("blue", "cadetblue1", "cornflowerblue", "cyan3", "darkcyan", "aquamarine4", "royalblue4","cornflowerblue", "darkturquoise"))
    len=length(levels(group))
    col.list = 1:len
    col.list_dif = 7:9
    par(fig=c(0, 0.8,0,1), mar=c(4,4,4,4))
    barx<-barplot(means, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(45), density=10, las=3)
    barx<-barplot(means, add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, las=3)
    #axis(2)
    axis(1, at=c(0,20), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.05)
    #create space for legend
    par(new=T)
    par(fig=c(0, 0.8, 0, 1), mar=c(4, 4, 4, 0))
    plot(5,5,axes=FALSE, ann=FALSE, xlim=c(0,10),ylim=c(0,10), type="n")
    legend(location, legend=levels(group), fill=c(col.list), bty="n",cex=1.2)
    
  } 
}

bar_graph_BW.se = function(means, er, xlab, ylab, ymax, ymin, group){
  if (group==0) {
    barx<-barplot(means, col="grey", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE)
    axis(2)
    axis(1, at=c(0,7), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
  }
  
  else {
    palette(c("grey40", "grey", "grey100"))
    len=length(levels(group))
    col.list = 1:len
    col.list_dif = 7:9
    par(fig=c(0, 0.8,0,1), mar=c(4,4,4,4))
    barx<-barplot(means, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(45), density=10)
    barx<-barplot(means, add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1)
    #axis(2)
    axis(1, at=c(0,20), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
    #create space for legend
    par(new=T)
    par(fig=c(0, 0.8, 0, 1), mar=c(4, 4, 4, 0))
    plot(5,5,axes=FALSE, ann=FALSE, xlim=c(0,10),ylim=c(0,10), type="n")
    legend("topright", legend=levels(group), fill=c(col.list), bty="n",cex=1.2)
    
  } 
}

bar_graph_BW.se_legendloc = function(means, er, xlab, ylab, ymax, ymin, group, location){
  if (group==0) {
    barx<-barplot(means, col="grey", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE)
    axis(2)
    axis(1, at=c(0,7), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
  }
  
  else {
    palette(c("grey40", "grey", "grey100"))
    len=length(levels(group))
    col.list = 1:len
    col.list_dif = 7:9
    par(fig=c(0, 0.8,0,1), mar=c(4,4,4,4))
    barx<-barplot(means, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(45), density=10)
    barx<-barplot(means, add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1)
    #axis(2)
    axis(1, at=c(0,20), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
    #create space for legend
    par(new=T)
    par(fig=c(0, 0.8, 0, 1), mar=c(4, 4, 4, 0))
    plot(5,5,axes=FALSE, ann=FALSE, xlim=c(0,10),ylim=c(0,10), type="n")
    legend(location, legend=levels(group), fill=c(col.list), bty="n",cex=1.2)
    
  } 
}

texture_bar_graph_BW.se = function(means, er, xlab, ylab, ymax, ymin, group){
  if (group==0) {
    barx<-barplot(means, col="grey", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE)
    barx<-barplot(means, add=TRUE, col="black", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE, lwd=1:2, angle=c(35, 45, 0), density=10)
    barx<-barplot(means, add=TRUE, col="black", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE, lwd=1:2, angle=c(70, 135, 90), density=10)
    barx<-barplot(means, add=TRUE, col="black", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE, lwd=1:2, angle=c(105, 135, 90), density=10)
    barx<-barplot(means, add=TRUE, col="black", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE, lwd=1:2, angle=c(140, 135, 90), density=10)
    axis(2)
    axis(1, at=c(0,7), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
  }
  
  else {
    #palette(c("grey40", "grey", "grey100"))
    len=length(levels(group))
    col.list = 1:len
    col.list_dif = c(7, 7, 7, 8, 8, 8,9,9,9)
    par(fig=c(0, 0.8,0,1), mar=c(4,4,4,4))
    barx<-barplot(means, col = 0, beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE)
##uncomment below for 2 groups  
    barx<-barplot(means,  add=TRUE, col="black", beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(45, 0), density=5)
     barx<-barplot(means, add=TRUE, col="black", beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(135, 90), density=5)
##uncomment below for 3 gropus
#     barx<-barplot(means,  add=TRUE, col="black", beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(35, 45, 0), density=10)
#     barx<-barplot(means, add=TRUE, col="black", beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(70, 135, 90), density=10)
#     barx<-barplot(means, add=TRUE, col="black", beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(105, 135, 90), density=10)
#     barx<-barplot(means, add=TRUE, col="black", beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(140, 135, 90), density=10)
    #axis(2)
    axis(1, at=c(0,20), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
    #create space for legend
    par(new=T)
    par(fig=c(0, 0.8, 0, 1), mar=c(4, 4, 4, 0))
    plot(5,5,axes=FALSE, ann=FALSE, xlim=c(0,10),ylim=c(0,10), type="n")
##uncommnet below for 2 groups
    legend("topright", legend=levels(group), bty="n", cex=2, pch(10,0))
    legend("topright", legend=levels(group), fill=c("black"), bty="n",cex=2, angle=c(45, 0), density=15)
    legend("topright", legend=levels(group), fill=c("black"), bty="n",cex=2, angle=c(135, 90), density=15)
##uncomment below for 3 groups    
#     legend("bottomright", legend=levels(group), fill=c("grey"),  bty="n",cex=1.5, pch(8,5,0))
#     legend("bottomright", legend=levels(group), fill=c("black"), bty="n",cex=1.5, angle=c(35, 45, 0), density=15)
#     legend("bottomright", legend=levels(group), fill=c("black"), bty="n",cex=1.5, angle=c(70, 135, 90), density=15)
#     legend("bottomright", legend=levels(group), fill=c("black"), bty="n",cex=1.5, angle=c(105, 135, 90), density=15)
#     legend("bottomright", legend=levels(group), fill=c("black"), bty="n",cex=1.5, angle=c(140, 135, 90), density=15)
  } 
}

##make bar graph without any standard error bars and is designed to be used in congunction with the means functions above.  In this case it will only work if your DV vector has 2 or less variables.  If graphing a 3-way interaction see other function for splitting data sets by factors

##--group=0 if only have 1 DV, if DV is multiple variables, group is the variable name for the grouping one
##if group =! 0, it means you have two DV's/a DV and a covariate. The first variable listed in your DV vector will be represtened by different colors in the legend. This will be the "group" variable and will create side by side bars. The second variable will have levels represented on x-axis. note: xpd=False restrains bars to graphic pane (can truncate lower part of graph)

bar_graph_BW.no.se = function(means, xlab, ylab, ymax, ymin, group){
  
  library(plotrix)
  if (group==0) {
    barx<-barplot(means, col="grey", pch=5, ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE)
    axis(2)
    axis(1, at=c(0,7), labels=FALSE)
  }
  
  else {
    palette(c("grey40", "grey", "grey100"))
    angle_list=c(0, 0, 80, 170, 90)
    density_list=c(0, 0, 15, 20, 20)
    len=length(levels(group))
    col.list = 1:len
    par(fig=c(0, 0.8,0,1), mar=c(4,4,4,4))
    barx<-barplot(means, col=c(col.list),  beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1)
    #axis(2)
    axis(1, at=c(0,20), labels=FALSE)
    #create space for legend
    par(new=T)
    par(fig=c(0, 0.8, 0, 1), mar=c(4, 4, 4, 0))
    plot(5,5,axes=FALSE, ann=FALSE, xlim=c(0,10),ylim=c(0,10), type="n")
    legend("topright", legend=levels(group), fill=c(col.list), bty="n",cex=1.2)
  } 
}

##This is an easier to use function for the ggplot comand for an interaction plot of one continuous and one categorical variable. Make sure xvar and yvar are both continous and cat.var is categorical.  Additionally, xname, yname, and tital must be in quotations.

int.plot_cont.cat=function(dat, yvar, xvar, cond, yname, xname, title){
  ggplot(dat, aes(x= xvar, y=yvar, color=cond),   environment=environment ()) + geom_point(shape=1) + scale_colour_hue(l=50) + geom_smooth(method=lm, se=FALSE, fullrange=F) + scale_y_continuous(name=yname) + scale_x_continuous(name=xname) + ggtitle(title)
}


##This interaction plot is the same as the previous, but with this one you will get the standard error space shaded around the lines of the graph.
int.plot_cont.cat_se=function(data.set, xvar, yvar, cat.var, xname, yname, title){
  ggplot(data.set, aes(x=xvar, y=yvar, color=cat.var), environment=environment ()) + geom_point(shape=1) + scale_colour_hue(l=50) + geom_smooth(method=lm, se=TRUE, fullrange=F) + scale_y_continuous(name=yname) + scale_x_continuous(name=xname) + ggtitle(title)
}

#### ANOVA Table -> Xtable ####

##To use with ANOVA: output paramater refers to ANVOA table--need to set your ANOVA equal to a name  eg. ANOVA=Anova(y~x)
##To use with regression: output paramater refers to summary table output   eg. Summary=summary(lm(y~x))
##sig_vector is a data frame you create with sig stars entered
##  eg sig_vector=data.frame(c("",".","*","","","",""))--need to be sure to includ blank " " for rows with no sig star so your output and sig tables are of equal length

sig_stars.table=function(output, sig_vector){
  options(xtable.comment = FALSE)
  output_table=data.frame(output)
  output_table1=data.frame(output_table, sig_vector)
  names(output_table1)=c(colnames(output),"")
  output_xtable=xtable(output_table1, align="lccccl", comment = FALSE)
  digits(output_xtable)=c(0,3,3,3,3,0)
  return(output_xtable)
}

sig_stars_lmerTestAll.table=function(output, sig_vector){
  options(xtable.comment = FALSE)
  output_table=data.frame(output)
  output_table1=data.frame(output_table, sig_vector)
  names(output_table1)=c(colnames(output),"")
  output_xtable=xtable(output_table1, align="lccccccl", comment = FALSE)
  digits(output_xtable)=c(0,3,3,3,3,3,3,0)
  return(output_xtable)
}


#### Dataset Manipulations ####

subset_longdata=function(resp_var, group1,g1_varnames,group2,g2_varnames,dataset){
  g1_l=length(g1_varnames)
  g2_l=length(g2_varnames)

  data=c()

  for (i in 1:g1_l){
    for (j in 1:g2_l){
      newdata <- dataset[ which(group1==g1_varnames[[i]]
                               & group2==g2_varnames[[j]]), ]
      d=newdata$resp_var
      data=c(data, d)
    }
  }
  return(data)
}


##concatonate two columns of categorical values (strings after melt function) so can do pair-wise comparisons of two-way ANOVA. Need to create new column with new column name first and then run this (note--new column name cant be coppied from a categorical column. I suggest newcolname=dataset$subject; then newcolname=as.character(newcolname)). Need column1 and column2 to be entered as dataset$colname. in end, convert to factor: newcolname=factor(newcolname)
concatonate_2col=function(column1, column2, newcolname){
  if (length(column1) == length(column2)){
    for (i in 1:length(column1)) {
      x=i
      names = c(toString(column1[x]), toString(column2[x]))
      new_name = paste(names, collapse="_")
      newcolname[x]=c(new_name)
    }
  }
  return(newcolname)
}

#### Functions for specific data #####

##This function converts BMI to percent of a reference level using CDC BMI for age BMI values at 50th percentile (year 2016). Create a dataset with the following variables in the order: participant ID, Gender (should be entered as data$gender with 'F' denoting female and 'M' denoting male), age (should be entered as months), and BMI. The function will return that dataset with added column. The refernceCat should be is the level you want computed against (underweight.3, underweight.5, average, overweight.85, obese.95, obese.97) 

BMI_2_percentRef = function (OrigData, referenceCat){
  
  percentRef_name = paste('perc', referenceCat, sep = "_")
  OrigData$percentRef = NA
  names(OrigData)[5] = percentRef_name
  
  male_dat = read.csv("~/Documents/Graduate/Research/BMIgeneral/BMIchart_male.csv", header=TRUE)
  female_dat = read.csv("~/Documents/Graduate/Research/BMIgeneral/BMIchart_female.csv", header=TRUE)
  
  ##Ipull out reference category BMI values
  male_ref = ifelse(referenceCat == 'underweight.3', male_dat[2], ifelse(referenceCat == 'underweight.5', male_dat[3], ifelse(referenceCat == 'average', male_dat[6], ifelse(referenceCat == 'overweight.85', male_dat[8], ifelse(referenceCat == 'obese.95', male_dat[10], male_dat[11])))))
  male_dat$ref = as.data.frame(male_ref[1])
  
  female_ref = ifelse(referenceCat == 'underweight.3', female_dat[2], ifelse(referenceCat == 'underweight.5', female_dat[3], ifelse(referenceCat == 'average', female_dat[6], ifelse(referenceCat == 'overweight.85', female_dat[8], ifelse(referenceCat == 'obese.95', female_dat[10], female_dat[11])))))
  female_dat$ref = as.data.frame(female_ref[1])
  
  for (r in 1:length(OrigData[, 1])){
    if (OrigData[r, 2] == 'M'){
      data = male_dat
    } else{
      data = female_dat
    }
    
    for (v in 1:length(data[,1])){
      if (OrigData[r, 3] == data[v, 1]){
        BMIdif = as.numeric(OrigData[r, 4]) - data[v, 12]
        OrigData[r, 5] = 100*(BMIdif/data[v, 12])
      } else {if ((OrigData[r, 3] + 0.5) == data[v, 1]){
        BMIdif = as.numeric(OrigData[r, 4]) - data[v, 12]
        OrigData[r, 5] = 100*(BMIdif/data[v, 12])
        }
      }
    }
  }
  return(OrigData)
}


# Calculate age at a given reference date
# Create an interval between the date of birth and the enrollment date; 
# intervals are specific to the two dates. Periods give the actual length
# of time between those dates, so convert to period and extract the year.

calc_age.month <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),
                      unit="month")
  period$month
}

# calucate the estimated V matrix or variance/covariance matrix
lmer.varcov.matrix_CompSym=function(Zmat, Rmat, Gmat, TimePoints){
  I = diag(1,TimePoints,TimePoints)
  V =  ((Zmat%*%Gmat)%*%t(Zmat)) + (Rmat*I)
}

# Get simple slopes of data
simpleSlopes <- function(linearSlope, quadSlope, xvalues) {
  n = length(xvalues)
  ss = rep(999, n)
  for (n in 1:length(xvalues)){
    ss[n] = linearSlope + 2*quadSlope*xvalues[n]
  }
  ss = cbind(xvalues, ss)
  return(ss)
}