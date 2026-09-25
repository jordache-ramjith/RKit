library(mmbslearn)
library(dplyr)
library(ggplot2)
for(f in c('engine.R','estimation.R','comparisons.R'))source(system.file('app',f,package='mmbslearn'))
# Welch intervals: independent stats::t.test reference, unequal sizes/variances and reversal.
set.seed(712)
for(level in c(.90,.95,.99))for(n in c(5,17,80)){
 x<-rnorm(n,6,2);y<-rnorm(n+9,4,.7)
 a<-mean_difference(x,y,level);b<-t.test(x,y,conf.level=level)
 stopifnot(isTRUE(all.equal(c(a$lower,a$upper),as.numeric(b$conf.int))),isTRUE(all.equal(a$se,unname(b$stderr))))
 rev<-mean_difference(y,x,level)
 stopifnot(isTRUE(all.equal(c(rev$estimate,rev$lower,rev$upper),-c(a$estimate,a$upper,a$lower))))
}
stopifnot(is.na(mean_difference(c(NA,2),1:4)$lower),is.na(mean_difference(c(NA,NA),1:4)$estimate),is.na(mean_difference(rep(3,4),rep(5,5))$lower))
stopifnot(is.finite(mean_difference(rep(3,4),1:5)$lower))
# Newcombe score examples from DescTools BinomDiffCI documentation (rounded to four decimals).
a<-proportion_comparison(56,70,48,80)
stopifnot(max(abs(c(a$lower[1],a$upper[1])/100-c(.0524,.3339)))<.00005)
a<-proportion_comparison(9,10,3,10)
stopifnot(max(abs(c(a$lower[1],a$upper[1])/100-c(.1705,.8090)))<.00005)
# Risk/odds log intervals agree with independent grouped-binomial GLM fits.
for(level in c(.90,.95,.99))for(counts in list(c(50,100,40,100),c(56,70,48,80),c(10,80,20,90))){
 a<-counts[1];n1<-counts[2];c0<-counts[3];n0<-counts[4]
 r<-proportion_comparison(a,n1,c0,n0,level)
 dd<-data.frame(group=c(0,1),events=c(c0,a),other=c(n0-c0,n1-a))
 for(k in 2:3){
 fit<-glm(cbind(events,other)~group,data=dd,family=binomial(link=if(k==2)'log'else'logit'))
 coef<-summary(fit)$coefficients['group',];ref<-exp(coef[1]+c(0,-1,1)*qnorm((1+level)/2)*coef[2])
 stopifnot(isTRUE(all.equal(c(r$estimate[k],r$lower[k],r$upper[k]),unname(ref),tolerance=2e-5)))
 }
 rev<-proportion_comparison(c0,n0,a,n1,level)
 stopifnot(isTRUE(all.equal(rev$estimate[1],-r$estimate[1])),isTRUE(all.equal(rev$lower[1],-r$upper[1])),isTRUE(all.equal(rev$upper[1],-r$lower[1])))
 stopifnot(isTRUE(all.equal(rev$estimate[2:3],1/r$estimate[2:3])),isTRUE(all.equal(rev$lower[2:3],1/r$upper[2:3])))
}
for(a in c(0,10))for(b in c(0,10)){
 r<-proportion_comparison(a,10,b,10)
 stopifnot(is.finite(r$lower[1]),is.finite(r$upper[1]),is.na(r$lower[3]))
}
stopifnot(all(is.na(proportion_comparison(0,0,5,10)$lower)))
r<-proportion_comparison(50,100,40,100,design='case_control')
stopifnot(all(is.na(r$estimate[1:2])),r$estimate[3]==1.5,is.finite(r$lower[3]))
stopifnot(proportion_comparison(50,100,40,100,design='cross_sectional')$measure[2]=='Prevalence ratio')
# Full scripts: original import + a derived variable + filter + every comparison and figure.
d<-data.frame(g=rep(c('A','B'),each=40),value=1:80,event=rep(c(rep('Yes',20),rep('No',20)),2))
d$event[41:50]<-'No';d$value[3]<-NA;d$event[4]<-NA
meta<-list(kind='csv',name='study.csv',delim=',',decimal='.',encoding='UTF-8',na=c('','NA'))
steps<-list(list(kind='derive',column='value',operation='scale',name='scaled',number=2),list(kind='filter',column='',join='all',rules=list(list(column='value',operator='ge',value='5',numeric=TRUE))))
for(kind in c('mean','proportion')){
 s<-list(kind=kind,x=if(kind=='mean')'scaled'else'event',group='g',event='Yes',compare=TRUE,reference='B',design='cohort',unit='hours',conf=.95)
 prepared<-apply_steps(d,steps);expected<-run_estimation(prepared,s)
 if(kind=='proportion')stopifnot(identical(rownames(expected$counts),c('A','B')),expected$counts['A','Event']==16,expected$counts['B','Event']==10)
 td<-tempfile();dir.create(td);dir.create(file.path(td,'data'));readr::write_csv(d,file.path(td,'data','study.csv'))
 writeLines(estimation_script(meta,steps,s),file.path(td,'analysis.R'))
 writeLines(c('source("analysis.R")','saveRDS(list(table=comparison_table,groups=estimate_table,data=study),"result.rds")'),file.path(td,'check.R'))
 wd<-getwd();setwd(td);status<-system2(file.path(R.home('bin'),'Rscript'),c('--vanilla','check.R'),stdout='run.log',stderr='run.log',env='R_TESTS=')
 if(status)stop(paste(readLines('run.log'),collapse='\n'))
 actual<-readRDS('result.rds');setwd(wd)
 stopifnot(isTRUE(all.equal(actual$table,expected$comparison)),isTRUE(all.equal(actual$groups,expected$table)),isTRUE(all.equal(as.data.frame(actual$data),as.data.frame(prepared))))
 for(p in expected$comparison_plots)ggplot_build(p)
 unlink(td,recursive=TRUE)
}
cat('PASS: Welch reference; published Newcombe examples; independent GLM ratio references; boundaries, reversals, design labels and full comparison scripts.\n')
