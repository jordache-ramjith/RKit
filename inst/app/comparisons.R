# Two independent groups. Shared calculation templates also appear in exported scripts.
comparison_title <- function(kind) if(kind=='mean')'Compare means in two groups'else'Compare proportions in two groups'
mean_difference <- function(x, y, level=.95) {
  x<-x[!is.na(x)];y<-y[!is.na(y)];nx<-length(x);ny<-length(y)
  estimate<-if(nx&&ny)mean(x)-mean(y)else NA_real_
  se<-df<-lower<-upper<-NA_real_;note<-'Welch interval: separate variability in the two independent groups.'
  if(nx<2||ny<2)note<-'At least two recorded measurements in each group are needed for a mean-difference interval.' else {
    vx<-stats::var(x)/nx;vy<-stats::var(y)/ny;se<-sqrt(vx+vy)
    if(!is.finite(se)||se<=0)note<-'There is no observed variation in either group. This routine interval is unavailable.' else {
      df<-(vx+vy)^2/(vx^2/(nx-1)+vy^2/(ny-1));margin<-stats::qt((1+level)/2,df)*se
      lower<-estimate-margin;upper<-estimate+margin
    }
  }
  data.frame(measure='Mean difference',estimate=estimate,lower=lower,upper=upper,null=0,unit='measurement units',se=se,method='Welch t interval',note=note)
}
proportion_comparison <- function(a, n1, c, n0, level=.95, design='cohort') {
  stopifnot(design%in%c('cohort','cross_sectional','case_control'),n1>=0,n0>=0,a>=0,c>=0,a<=n1,c<=n0)
  b<-n1-a;d<-n0-c;p1<-if(n1)a/n1 else NA_real_;p0<-if(n0)c/n0 else NA_real_
  ratio<-function(num,den)if(is.na(num)||is.na(den)||num==0&&den==0)NA_real_ else if(den==0)Inf else num/den
  rd<-p1-p0;rr<-ratio(p1,p0);or<-ratio(a*d,b*c)
  lower<-upper<-rep(NA_real_,3);notes<-rep('Independent groups; interpret alongside the observed counts.',3)
  if(n1==0||n0==0){notes[]<-'Both groups need at least one recorded outcome.'}else {
    # Newcombe hybrid-score interval, using Wilson intervals without continuity correction.
    w1<-wilson_interval(a,n1,level);w0<-wilson_interval(c,n0,level)
    lower[1]<-max(-1,rd-sqrt((p1-w1[1])^2+(w0[2]-p0)^2))
    upper[1]<-min(1,rd+sqrt((w1[2]-p1)^2+(p0-w0[1])^2))
    z<-stats::qnorm((1+level)/2)
    if(a>0&&c>0&&(b>0||d>0)){
      se_log_rr<-sqrt(1/a-1/n1+1/c-1/n0)
      lower[2]<-exp(log(rr)-z*se_log_rr);upper[2]<-exp(log(rr)+z*se_log_rr)
    }else notes[2]<-'The usual log risk-ratio interval is unavailable at this boundary. No artificial events were added.'
    if(all(c(a,b,c,d)>0)){
      se_log_or<-sqrt(1/a+1/b+1/c+1/d)
      lower[3]<-exp(log(or)-z*se_log_or);upper[3]<-exp(log(or)+z*se_log_or)
    }else notes[3]<-'A zero count prevents the usual log odds-ratio interval. No artificial events were added.'
    if(min(a,b,c,d)<5){
      notes[1]<-'Some event/non-event counts are small. Read the interval together with the counts.'
      for(j in 2:3)if(is.finite(lower[j]))notes[j]<-'Some counts are below 5: this approximate log interval may be unreliable. A sparse-data method may be needed.'
    }
  }
  if(design=='case_control'){
    rd<-rr<-NA_real_;lower[1:2]<-upper[1:2]<-NA_real_
    notes[1:2]<-'Not reported as a population risk estimate: the sample percentages and their comparison can be calculated, but depend on how cases and controls were selected.'
  }
  data.frame(measure=c(if(design=='cross_sectional')'Prevalence difference'else'Risk difference',if(design=='cross_sectional')'Prevalence ratio'else'Risk ratio','Odds ratio'),
    estimate=c(100*rd,rr,or),lower=lower*c(100,1,1),upper=upper*c(100,1,1),null=c(0,1,1),
    unit=c('percentage points','ratio','ratio'),se=NA_real_,method=c('Newcombe score interval','Log Wald interval','Log Wald interval'),note=notes)
}
comparison_code <- function(s) {
  design<-if(nonempty(s$design))s$design else 'cohort'
  c(paste0('reference_group <- ',r_string(s$reference)),
    'comparison_levels <- sort(unique(analysis_data$group))',
    'if(length(comparison_levels)!=2L || !reference_group %in% comparison_levels) stop("Choose exactly two observed groups and a valid reference group.")',
    'comparison_group <- setdiff(comparison_levels,reference_group)',
    'x <- analysis_data$value[analysis_data$group == comparison_group]',
    'y <- analysis_data$value[analysis_data$group == reference_group]',
    '# Direction is always comparison group minus reference, or comparison divided by reference.',
    if(s$kind=='mean')c(est_definition('mean_difference',mean_difference),'comparison_table <- mean_difference(x,y,confidence_level)')else c(
      est_definition('proportion_comparison',proportion_comparison),
      paste0('study_design <- ',r_string(design)),
      'comparison_counts <- rbind(c(sum(x==event_category,na.rm=TRUE),sum(x!=event_category,na.rm=TRUE)),c(sum(y==event_category,na.rm=TRUE),sum(y!=event_category,na.rm=TRUE)))',
      'rownames(comparison_counts) <- c(comparison_group,reference_group)\ncolnames(comparison_counts) <- c("Event","Other outcome")',
      'comparison_table <- proportion_comparison(comparison_counts[1,1],sum(comparison_counts[1,]),comparison_counts[2,1],sum(comparison_counts[2,]),confidence_level,study_design)'),
    'comparison_table$comparison <- paste(comparison_group,"versus",reference_group)',
    if(s$kind=='mean'&&nonempty(s$unit))paste0('comparison_table$unit <- ',r_string(s$unit)),
    'comparison_plots <- lapply(seq_len(nrow(comparison_table)),function(i) {\n  a <- comparison_table[i,,drop=FALSE]\n  ok <- is.finite(a$estimate) && is.finite(a$lower) && is.finite(a$upper)\n  p <- ggplot(a,aes(y=1,x=.data$estimate)) + geom_vline(xintercept=a$null,linetype=2,colour="#7161AC")\n  if(ok) p <- p + geom_segment(aes(x=.data$lower,xend=.data$upper,yend=1),colour="#087F8C",linewidth=1.2) + geom_point(colour="#087F8C",size=3)\n  if(!ok) p <- p + annotate("text",x=a$null,y=1,label="Interval unavailable: read the table note",size=3.5)\n  if(a$null==1 && ok && a$lower>0) p <- p + scale_x_log10()\n  p + scale_y_continuous(breaks=NULL,limits=c(.6,1.4)) + labs(y=NULL,x=a$unit,title=est_wrap(a$measure,40),subtitle=est_wrap(paste(comparison_group,"versus",reference_group),50)) + theme_minimal(base_size=12)\n})')
}
comparison_display <- function(r,s) {
  a<-r$comparison;d<-a[c('measure','estimate','lower','upper','unit','method','note')]
  if(s$kind=='mean'&&nonempty(s$unit))d$unit<-s$unit
  names(d)<-c('Measure','Estimate',paste0(round(s$conf*100),'% CI lower'),paste0(round(s$conf*100),'% CI upper'),'Units','Interval method','Read with the result');d
}
comparison_description <- function(r,s) {
  f<-function(x)formatC(x,format='f',digits=2)
  head<-paste0('Direction: ',r$comparison_group,' compared with ',s$reference,'. Differences subtract ',s$reference,'; ratios divide by ',s$reference,'.')
  c(head,vapply(seq_len(nrow(r$comparison)),function(i){a<-r$comparison[i,];unit<-if(s$kind=='mean'&&nonempty(s$unit))s$unit else a$unit
    if(is.na(a$estimate))return(paste(a$measure,'is unavailable.',a$note))
    point<-paste0(a$measure,': ',if(is.infinite(a$estimate))'infinite (a denominator in this ratio is zero)'else f(a$estimate),if(unit!='ratio')paste0(' ',unit)else'', '.')
    if(!is.finite(a$lower)||!is.finite(a$upper))return(paste(point,a$note))
    same<-a$lower<=a$null&&a$upper>=a$null
    equal<-if(s$kind=='mean')'Equal population means'else if(a$measure=='Odds ratio')'Equal population odds'else if(s$design=='cross_sectional')'Equal population prevalences'else'Equal population risks'
    meaning<-if(unit=='ratio')paste0(' This is ',f(a$estimate),' times the ',if(a$measure=='Odds ratio')'odds'else if(s$design=='cross_sectional')'prevalence'else'risk',' of the event in ',r$comparison_group,' compared with ',s$reference,'.')else''
    paste0(point,meaning,' The ',round(s$conf*100),'% confidence interval runs from ',f(a$lower),' to ',f(a$upper),if(unit!='ratio')paste0(' ',unit)else'',
      '. It ',if(same)'includes 'else'does not include ',a$null,'. ',
      equal,if(same)' remain compatible with these data and the method’s assumptions. This does not prove the groups are the same.'else' are outside this interval under the method’s assumptions. Consider the size of the comparison and the study design, not just whether the interval crosses the line.')
  },character(1)))
}
