library(mmbslearn)
library(dplyr)
library(ggplot2)
source(system.file('app','engine.R',package='mmbslearn'))
raw_path <- system.file('extdata','wellbeing.csv',package='mmbslearn')
meta <- list(kind='csv',name='wellbeing.csv',delim=',',decimal='.',encoding='UTF-8',na=c('','NA'))
d <- read_source(raw_path,meta)
stopifnot(nrow(d)==120,ncol(d)==8)
stopifnot(identical(d$short_sleep,ifelse(is.na(d$sleep_hours),NA_character_,ifelse(d$sleep_hours<7,"Yes","No"))))
steps <- list(
 list(kind='type',column='programme',type='categorical'),
 list(kind='filter',column='',join='all',rules=list(list(column='age',operator='ge',value='30',numeric=TRUE),list(column='smoking',operator='ne',value='Current',numeric=FALSE))),
 list(kind='derive',column='marker',operation='log',name='log_marker'),
 list(kind='derive',column='age',operation='threshold',name='age_group',number=50,above='50 or above',below='Under 50'))
prepared<-apply_steps(d,steps)
stopifnot(nrow(prepared)==sum(d$age>=30 & d$smoking!='Current'),isTRUE(all.equal(prepared$log_marker,log(prepared$marker))))
spec <- function(route,x,y='',z='',denom='row')list(route=route,x=x,y=y,z=z,bins=15L,measure='percent',denom=denom,cor='pearson')
ss<-list(spec('num','sleep_hours'),spec('cat','smoking'),spec('group','wellbeing_score','programme'),spec('group','wellbeing_score','programme','smoking'),spec('cat2','programme','smoking'),spec('cat2','programme','smoking',denom='column'),spec('cat2','programme','smoking',denom='all'),spec('num2','sleep_hours','wellbeing_score'),spec('num2','sleep_hours','wellbeing_score','programme'))
for(s in ss){
 result<-run_analysis(prepared,s)
 stopifnot(inherits(result$plot,'ggplot'),nrow(result$table)>0)
 ggplot_build(result$plot)
 wd<-getwd();td<-tempfile();dir.create(td);dir.create(file.path(td,'data'));file.copy(raw_path,file.path(td,'data','wellbeing.csv'))
 writeLines(full_script(meta,steps,s),file.path(td,'analysis.R'))
 setwd(td)
 # Evaluate as a fresh R process, with no app helper functions loaded.
 writeLines(c('source("analysis.R")','saveRDS(summary_table,"table.rds")','saveRDS(study,"study.rds")'), 'check.R')
 exit<-system2(file.path(R.home('bin'),'Rscript'),c('--vanilla','check.R'),stdout='run.log',stderr='run.log',env='R_TESTS=')
 if(exit!=0)stop(paste(readLines('run.log'),collapse='\n'))
 stopifnot(isTRUE(all.equal(as.data.frame(readRDS('table.rds')),as.data.frame(result$table))),isTRUE(all.equal(as.data.frame(readRDS('study.rds')),prepared)))
 setwd(wd);unlink(td,recursive=TRUE)
}
# All-missing and singleton groups retain clear missing summaries, not Inf.
d2<-data.frame(group=c('A','A','B'),value=c(NA,NA,2))
r<-run_analysis(d2,spec('group','value','group'))
stopifnot(is.na(r$table$mean[1]),is.na(r$table$sd[2]),r$table$missing[1]==2)
# Explicit OR, missing-value condition and safe column quoting.
odd<-data.frame(check.names=FALSE,'age; stop("bad")'=c(1,NA,3),'group name'=c('a','b','c'))
f<-list(kind='filter',column='',join='any',rules=list(list(column=names(odd)[1],operator='eq',numeric=TRUE,value='1'),list(column=names(odd)[1],operator='missing',numeric=TRUE,value='')))
stopifnot(nrow(apply_steps(odd,list(f)))==2)
# Invalid log, unsafe conversion and zero denominators fail without data loss.
bad_log<-list(kind='derive',operation='log',column='x',name='log_x')
stopifnot(inherits(try(check_step(data.frame(x=c(0,1)),bad_log),silent=TRUE),'try-error'))
stopifnot(inherits(try(check_step(data.frame(x=c('a','2')),list(kind='type',column='x',type='numerical')),silent=TRUE),'try-error'))
# Degenerate correlations report unavailable rather than misleading numeric values.
r<-run_analysis(data.frame(x=1:5,y=rep(2,5)),spec('num2','x','y'))
stopifnot(is.na(r$table$correlation))
cat('PASS: all routes, clean-session scripts, missingness, safe filtering, invalid conversions, and constant correlation.\n')
# Independent hand-calculated reference values and all preparation operations.
tiny<-data.frame(value=c(1,2,3,NA),group=c('A','A','B','B'),other=c(2,2,2,2))
r<-run_analysis(tiny,spec('num','value'))
stopifnot(r$table$n==3,r$table$missing==1,r$table$mean==2,r$table$variance==1,r$table$q1==1.5,r$table$q3==2.5)
r<-run_analysis(tiny,spec('cat','group'))
stopifnot(all(r$table$n==2),all(r$table$percent==50))
for(op in c('scale','add','subtract','ratio')){
 st<-list(kind='derive',column='value',operation=op,name='derived',number=2,other='other')
 check_step(tiny,st);x<-apply_steps(tiny,list(st))$derived
 expected<-switch(op,scale=c(2,4,6,NA),add=c(3,4,5,NA),subtract=c(-1,0,1,NA),ratio=c(.5,1,1.5,NA))
 stopifnot(isTRUE(all.equal(x,expected)))
}
st<-list(kind='derive',column='group',operation='recode',name='renamed_group',old='A',new='First')
check_step(tiny,st);stopifnot(identical(apply_steps(tiny,list(st))$renamed_group,c('First','First','B','B')))
cat('PASS: hand-calculated reference values and arithmetic/recode operations.\n')
