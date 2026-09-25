# Linear regression: one full model, explicit coding and covariance-based contrasts.
reg_name <- function(x) paste(deparse(as.name(x),backtick=TRUE),collapse='')
reg_formula <- function(s, response=TRUE) {
 terms<-c(vapply(s$predictors,reg_name,character(1)),vapply(s$interactions,function(p)paste(vapply(p,reg_name,character(1)),collapse=':'),character(1)))
 as.formula(paste(if(response)reg_name(s$outcome)else'', '~',if(length(terms))paste(terms,collapse=' + ')else'1'))
}
reg_prepare <- function(d,s){
 cols<-c(s$outcome,s$predictors)
 if(length(s$outcome)!=1||!length(s$predictors)||length(s$predictors)>6||anyDuplicated(cols)||!all(cols%in%names(d)))stop('Choose one outcome and 1–6 different predictors. The outcome cannot also be a predictor.')
 if(!is.numeric(d[[s$outcome]]))stop('The outcome must be a numerical measurement.')
 if(!all(s$categorical%in%s$predictors))stop('Categorical choices must be selected predictors.')
 if(length(s$interactions)&&any(vapply(s$interactions,function(p)length(p)!=2||anyDuplicated(p)||!all(p%in%s$predictors),logical(1))))stop('Each interaction needs two different selected predictors.')
 if(length(s$interactions)&&anyDuplicated(vapply(s$interactions,function(p)paste(sort(p),collapse='\r'),character(1))))stop('An interaction has been selected more than once.')
 keep<-complete.cases(d[cols]);a<-as.data.frame(d[keep,cols,drop=FALSE]);row_id<-which(keep)
 if(nrow(a)<4)stop('At least four complete observations are needed. Check missing values and filters.')
 if(length(unique(a[[s$outcome]]))<=2)stop('Linear regression here is for a numerical measurement with more than two values. A binary outcome needs a different model.')
 for(v in cols){
  if(v%in%s$categorical){
   lev<-if(is.factor(a[[v]]))levels(droplevels(a[[v]]))else sort(unique(as.character(a[[v]])))
   if(length(lev)<2||length(lev)>10)stop(paste(v,'needs 2–10 observed categories among complete rows.'))
   ref<-s$references[[v]];if(is.null(ref)||!ref%in%lev)stop(paste('Choose an observed reference category for',v))
   a[[v]]<-factor(as.character(a[[v]]),levels=c(ref,setdiff(lev,ref)))
  }else if(!is.numeric(a[[v]])||any(!is.finite(a[[v]])))stop(paste(v,'must be numerical with finite values, or marked categorical.'))
  if(length(unique(a[[v]]))<2)stop(paste(v,'does not vary among the complete observations.'))
 }
 list(data=a,n=nrow(a),total=nrow(d),excluded=sum(!keep),row_id=row_id,spec=s)
}
reg_fit <- function(d,s){
 r<-reg_prepare(d,s);a<-r$data
 if(is.null(s$conf)||!is.finite(s$conf)||s$conf<=0||s$conf>=1)stop('Choose a confidence level between 0 and 1.')
 contrasts<-if(length(s$categorical))setNames(lapply(s$categorical,function(v)contr.treatment(levels(a[[v]]),base=1)),s$categorical)else NULL
 fit<-lm(reg_formula(s),data=a,contrasts=contrasts,na.action=na.fail)
 if(fit$rank<ncol(model.matrix(fit)))stop('Some model terms contain the same information or some category combinations are absent. Remove redundant predictors/interactions or inspect the category counts; coefficients cannot be uniquely estimated.')
 if(df.residual(fit)<3)stop('This model leaves fewer than three residual degrees of freedom. Use fewer predictors/interactions or more observations.')
 sm<-summary(fit)
 if(!is.finite(sm$sigma)||sm$sigma<sqrt(.Machine$double.eps)*max(1,sd(a[[s$outcome]])))stop('The fit is essentially exact. Check for an outcome copied into a predictor or a derived variable. Uncertainty cannot be interpreted reliably.')
 ct<-coef(sm);ci<-confint(fit,level=s$conf)
 r$fit<-fit;r$coefficients<-data.frame(Term=rownames(ct),Estimate=ct[,1],SE=ct[,2],Lower=ci[,1],Upper=ci[,2],t=ct[,3],p_value=ct[,4],row.names=NULL)
 f<-sm$fstatistic;r$fit_table<-data.frame(Observations=r$n,Excluded=r$excluded,Parameters=fit$rank,Residual_df=df.residual(fit),R_squared=sm$r.squared,Adjusted_R_squared=sm$adj.r.squared,Residual_SD=sm$sigma,Overall_F=unname(f[1]),Overall_p=pf(f[1],f[2],f[3],lower.tail=FALSE),row.names=NULL)
 r$diagnostics<-data.frame(Row=row_id<-r$row_id,Fitted=fitted(fit),Residual=residuals(fit),Standardised=rstandard(fit),Leverage=hatvalues(fit),Cooks_distance=cooks.distance(fit))
 r$interaction_tests<-if(length(s$interactions))do.call(rbind,lapply(seq_along(s$interactions),function(i){reduced<-s;reduced$interactions<-s$interactions[-i];small<-lm(reg_formula(reduced),data=a,contrasts=contrasts,na.action=na.fail);tab<-anova(small,fit);data.frame(Interaction=paste(s$interactions[[i]],collapse=' × '),df=tab$Df[2],F=tab$F[2],p_value=tab$`Pr(>F)`[2])}))else NULL
 tests<-drop1(fit,test='F');tests<-tests[rownames(tests)!='<none>',,drop=FALSE]
 r$term_tests<-data.frame(Term=rownames(tests),df=tests$Df,F=tests$`F value`,p_value=tests$`Pr(>F)`,row.names=NULL)
 r
}
reg_profile <- function(r,zero=FALSE){
 a<-r$data[1,r$spec$predictors,drop=FALSE]
 for(v in r$spec$predictors)a[[v]]<-if(is.factor(r$data[[v]]))factor(levels(r$data[[v]])[1],levels=levels(r$data[[v]]))else if(zero)0 else median(r$data[[v]])
 a
}
reg_set <- function(r,d,v,value){
 d[[v]]<-if(is.factor(r$data[[v]]))factor(value,levels=levels(r$data[[v]]))else as.numeric(value)
 if(anyNA(d[[v]])||(!is.factor(d[[v]])&&any(!is.finite(d[[v]]))))stop(paste('Choose a valid value for',v))
 d
}
reg_matrix <- function(r,d) model.matrix(delete.response(terms(r$fit)),d,contrasts.arg=r$fit$contrasts,xlev=r$fit$xlevels)[,names(coef(r$fit)),drop=FALSE]
reg_linear <- function(r,L){
 est<-as.numeric(L%*%coef(r$fit));se<-sqrt(pmax(0,rowSums((L%*%vcov(r$fit))*L)));q<-qt((1+r$spec$conf)/2,df.residual(r$fit))
 stat<-est/se;pv<-2*pt(-abs(stat),df.residual(r$fit));pv[se==0]<-NA_real_
 data.frame(Estimate=est,SE=se,Lower=est-q*se,Upper=est+q*se,t=stat,p_value=pv)
}
reg_effects <- function(r,focal,by='',profile=reg_profile(r)){
 if(!focal%in%r$spec$predictors||identical(focal,by))stop('Choose different effect and stratifying variables.')
 if(nzchar(by)&&!by%in%r$spec$categorical)stop('The stratifying variable must be categorical.')
 strata<-if(nzchar(by))levels(r$data[[by]])else'All observations'
 vals<-if(focal%in%r$spec$categorical)levels(r$data[[focal]])[-1]else NA
 rows<-list();L<-list()
 for(g in strata)for(val in vals){
  base<-profile;if(nzchar(by))base<-reg_set(r,base,by,g)
  if(focal%in%r$spec$categorical){ref<-levels(r$data[[focal]])[1];lo<-reg_set(r,base,focal,ref);hi<-reg_set(r,base,focal,val);label<-paste(val,'minus',ref)}else{lo<-reg_set(r,base,focal,profile[[focal]]);hi<-reg_set(r,base,focal,profile[[focal]]+1);label<-paste('Per 1-unit increase in',focal)}
  delta<-reg_matrix(r,hi)-reg_matrix(r,lo);L[[length(L)+1]]<-delta
  rows[[length(rows)+1]]<-cbind(data.frame(Stratum=g,Effect=label),reg_linear(r,delta))
 }
 list(table=do.call(rbind,rows),contrast=do.call(rbind,L))
}
# Expand the SAME fitted model after substituting a selected category.
# Coefficients, covariance and residual degrees of freedom remain from the full fit.
reg_conditional <- function(r,by=''){
 if(nzchar(by)&&!by%in%r$spec$categorical)stop('Choose a categorical variable for the conditional equations.')
 strata<-if(nzchar(by))levels(r$data[[by]])else'Full model';preds<-setdiff(r$spec$predictors,by);out<-list();equations<-character()
 for(g in strata){
  base<-reg_profile(r,TRUE);if(nzchar(by))base<-reg_set(r,base,by,g)
  M0<-reg_matrix(r,base);L<-list(M0);labels<-'Intercept';components<-list()
  for(v in preds){
   vals<-if(v%in%r$spec$categorical)levels(r$data[[v]])[-1]else 1
   components[[v]]<-lapply(vals,function(val)list(value=val,label=if(v%in%r$spec$categorical)paste0('I(',v,' = ',val,')')else v))
   for(cmp in components[[v]]){z<-reg_set(r,base,v,cmp$value);L[[length(L)+1]]<-reg_matrix(r,z)-M0;labels<-c(labels,cmp$label)}
  }
  for(pair in r$spec$interactions)if(!by%in%pair){
   for(a in components[[pair[1]]])for(b in components[[pair[2]]]){
    da<-reg_set(r,base,pair[1],a$value);db<-reg_set(r,base,pair[2],b$value);dab<-reg_set(r,da,pair[2],b$value)
    L[[length(L)+1]]<-reg_matrix(r,dab)-reg_matrix(r,da)-reg_matrix(r,db)+M0;labels<-c(labels,paste(a$label,b$label,sep=' × '))
   }
  }
  tab<-cbind(data.frame(Stratum=g,Term=labels),reg_linear(r,do.call(rbind,L)));out[[g]]<-tab
  pieces<-vapply(seq_len(nrow(tab))[-1],function(i)paste(if(tab$Estimate[i]<0)'−'else'+',format(abs(tab$Estimate[i]),digits=4),paste0('[',tab$Term[i],']')),character(1))
  equations[g]<-paste('Predicted',r$spec$outcome,'=',format(tab$Estimate[1],digits=4),paste(pieces,collapse=' '))
 }
 list(table=do.call(rbind,out),equations=equations)
}
reg_grid <- function(r,focal,by='',profile=reg_profile(r)){
 if(!focal%in%r$spec$predictors||identical(focal,by))stop('Choose different horizontal and colour variables.')
 xs<-if(focal%in%r$spec$categorical)levels(r$data[[focal]])else seq(min(r$data[[focal]]),max(r$data[[focal]]),length.out=80)
 gs<-if(!nzchar(by))''else if(by%in%r$spec$categorical)levels(r$data[[by]])else unique(as.numeric(quantile(r$data[[by]],c(.25,.5,.75))))
 blocks<-lapply(gs,function(g){z<-profile[rep(1,length(xs)),,drop=FALSE];z<-reg_set(r,z,focal,xs);if(nzchar(by))z<-reg_set(r,z,by,rep(g,length(xs)));z})
 nd<-do.call(rbind,blocks);ci<-predict(r$fit,newdata=nd,interval='confidence',level=r$spec$conf)
 list(newdata=nd,table=data.frame(X=nd[[focal]],Group=if(nzchar(by))as.character(nd[[by]])else'Fitted mean',Estimate=ci[,1],Lower=ci[,2],Upper=ci[,3]))
}
reg_theme <- function()ggplot2::theme_minimal(base_size=12)+ggplot2::theme(legend.position='bottom',plot.title=ggplot2::element_text(face='bold',size=14),plot.margin=ggplot2::margin(12,18,12,12))
reg_wrap <- function(x)vapply(x,function(v)paste(strwrap(v,24),collapse='\n'),character(1))
reg_plot <- function(r,focal,by='',profile=reg_profile(r)){
 d<-reg_grid(r,focal,by,profile)$table
 p<-ggplot2::ggplot(d,ggplot2::aes(x=X,y=Estimate,colour=Group,group=Group))
 if(focal%in%r$spec$categorical)p<-p+ggplot2::geom_errorbar(ggplot2::aes(ymin=Lower,ymax=Upper),width=.12,position=ggplot2::position_dodge(.45))+ggplot2::geom_point(size=2.6,position=ggplot2::position_dodge(.45))+ggplot2::scale_x_discrete(labels=reg_wrap)
 else p<-p+ggplot2::geom_ribbon(ggplot2::aes(ymin=Lower,ymax=Upper,fill=Group),alpha=.13,colour=NA)+ggplot2::geom_line(linewidth=.9)
 p+ggplot2::labs(x=reg_wrap(focal),y=paste('Predicted mean',paste(reg_wrap(r$spec$outcome),collapse='')),colour=if(nzchar(by))by else NULL,fill=if(nzchar(by))by else NULL)+ggplot2::guides(colour=ggplot2::guide_legend(ncol=2),fill=ggplot2::guide_legend(ncol=2))+reg_theme()
}
reg_observed <- function(r,focal){
 d<-data.frame(X=r$data[[focal]],Y=r$data[[r$spec$outcome]])
 p<-ggplot2::ggplot(d,ggplot2::aes(x=X,y=Y))
 if(is.factor(d$X))p<-p+ggplot2::geom_boxplot(fill='#b8dfd9',outlier.shape=NA)+ggplot2::geom_point(position=ggplot2::position_jitter(width=.1,height=0,seed=2026),alpha=.5)+ggplot2::scale_x_discrete(labels=reg_wrap)else p<-p+ggplot2::geom_point(colour='#087f8c',alpha=.5)
 p+ggplot2::labs(x=reg_wrap(focal),y=reg_wrap(r$spec$outcome))+reg_theme()
}
reg_diagnostic <- function(r,kind){
 d<-r$diagnostics
 switch(kind,
  residual=ggplot2::ggplot(d,ggplot2::aes(Fitted,Residual))+ggplot2::geom_hline(yintercept=0,linetype='dashed')+ggplot2::geom_point(alpha=.6,colour='#087f8c')+ggplot2::labs(x='Fitted value',y='Observed minus fitted')+reg_theme(),
  qq=ggplot2::ggplot(d,ggplot2::aes(sample=Standardised))+ggplot2::stat_qq(colour='#087f8c',alpha=.6)+ggplot2::stat_qq_line()+ggplot2::labs(x='Values expected from a normal shape',y='Standardised residual')+reg_theme(),
  cooks=ggplot2::ggplot(d,ggplot2::aes(Row,Cooks_distance))+ggplot2::geom_col(fill='#087f8c')+ggplot2::labs(x='Row in the prepared dataset',y="Cook’s distance")+reg_theme())
}
reg_compare <- function(r,focal){
 if(length(r$spec$interactions))return(NULL)
 simple<-r$spec;simple$predictors<-focal;simple$categorical<-intersect(focal,simple$categorical);simple$references<-simple$references[simple$categorical];simple$interactions<-list()
 a<-reg_fit(r$data,simple)
 x<-reg_effects(a,focal)$table;y<-reg_effects(r,focal)$table
 rbind(cbind(Model='Unadjusted: same complete observations',x),cbind(Model='Adjusted: selected predictors held fixed',y))
}
reg_p <- function(x)ifelse(is.na(x),'unavailable',ifelse(x<.0001,'< 0.0001',formatC(x,digits=4,format='f')))
reg_interpret <- function(r,term){
 a<-r$coefficients[match(term,r$coefficients$Term),];ci<-paste0(format(a$Lower,digits=4),' to ',format(a$Upper,digits=4))
 c(paste('This coefficient is',format(a$Estimate,digits=4),'; its',100*r$spec$conf,'% confidence interval is',ci,'.'),
 if(term=='(Intercept)')'The intercept is the predicted mean when numerical predictors equal zero and categorical predictors are in their reference categories. If those values are outside the data, it is an equation anchor rather than a meaningful person.'else if(grepl(':',term,fixed=TRUE))'An interaction coefficient changes another effect: for a numerical-by-category interaction it is a difference in slopes; for two categories it is a difference of differences; for two numerical variables it describes how a slope changes per unit of the other variable.'else if(length(r$spec$interactions))'With interactions, main coefficients refer to zero values of numerical interactors and reference categories of categorical interactors. Use the effects and conditional equations below for other settings.'else'For a numerical predictor this is a change in predicted mean per one unit. For a categorical indicator it is a difference from its reference category. Other selected predictors are held fixed.',
 paste('The two-sided p-value for this individual coefficient versus zero is',reg_p(a$p_value),'.',if(a$Lower<=0&&a$Upper>=0)'The interval includes zero: these data are compatible with a zero coefficient under the model; they do not prove no association.'else'The interval excludes zero at this confidence level. Consider the size and precision as well as the significance threshold.'),
 'Coefficient intervals and p-values are unadjusted for multiple testing. A multi-level categorical variable or interaction has several coefficients; one coefficient does not test the whole term. Association and statistical adjustment alone do not establish causation.')
}
