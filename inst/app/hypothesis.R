# Statistical engine: ordinary stats functions, explicit direction and complete pairs.
h_routes <- c(one='One numerical variable',paired='Two paired numerical measurements',two='A numerical variable in two groups',many='A numerical variable in three or more groups',binary='One binary outcome',categorical='Two categorical variables',paired_binary='Two paired binary measurements')
h_prompts <- c(one='Compare a population mean with a stated value, such as 7 hours of sleep.',paired='Compare before and after measurements from the same people.',two='Compare a measurement between two groups of different people.',many='Compare a measurement between three or more independent groups.',binary='Compare an event proportion with a stated percentage.',categorical='Explore an association, including a binary outcome in two independent groups.',paired_binary='Compare a Yes/No outcome before and after in the same people.')
h_methods <- c(t_one='One-sample t-test',t_paired='Paired t-test',welch='Welch two-sample t-test',pooled='Equal-variance two-sample t-test',anova='One-way ANOVA',welch_anova='Welch one-way ANOVA',signed='Wilcoxon signed-rank test',rank='Wilcoxon rank-sum (Mann–Whitney) test',sign='Exact sign test',kruskal='Kruskal–Wallis test',binomial='Exact binomial test',prop='One-sample proportion test',chi='Pearson chi-squared test',fisher="Fisher's exact test",fisher_mc="Fisher's test with simulated p-value",mcnemar="McNemar's test",mcnemar_exact="Exact McNemar test")
h_numeric <- function(k) k %in% c('one','paired','two','many')
h_levels <- function(x) if(is.factor(x))levels(droplevels(x))else sort(unique(as.character(x[!is.na(x)])))
h_prepare <- function(d,s) {
 k<-s$route;cols<-unique(c(s$x,if(k%in%c('paired','paired_binary','two','many','categorical'))s$y))
 if(!all(cols%in%names(d)))stop('Choose the variables for this question.')
 if(length(cols)==1&&k%in%c('paired','paired_binary','two','many','categorical'))stop('Choose two different variables.')
 keep<-complete.cases(d[cols]);a<-d[keep,cols,drop=FALSE]
 if(!nrow(a))stop('No rows have all the selected values recorded. Check missing values and filters.')
 x<-a[[s$x]];y<-if(length(cols)>1)a[[s$y]]else NULL;g<-NULL;counts<-expected<-NULL;event<-s$event
 if(h_numeric(k)){
  if(!is.numeric(x)||any(!is.finite(x)))stop('Choose a numerical outcome with finite values.')
  if(k=='paired'&&(!is.numeric(y)||any(!is.finite(y))))stop('Both paired measurements must be numerical.')
  if(k%in%c('two','many')){
   lev<-h_levels(y);if((k=='two'&&length(lev)!=2)||(k=='many'&&(length(lev)<3||length(lev)>10)))stop(if(k=='two')'The grouping variable must have two observed categories among complete rows.'else'Choose a grouping variable with 3–10 observed categories among complete rows.')
   if(k=='two'){
    if(!s$reference%in%lev)stop('Choose a reference group present in the complete observations.')
    lev<-c(s$reference,setdiff(lev,s$reference))
   }
   g<-factor(as.character(y),levels=lev)
  }
  if(isTRUE(s$log)){
   if(any(x<=0)||(k=='paired'&&any(y<=0)))stop('A log transformation requires all selected measurements to be greater than zero. No observations have been removed to force this.')
   if(k=='one'&&s$null<=0)stop('A one-sample log comparison needs a positive reference value.')
   if(k=='paired'&&s$null!=0)stop('The paired log route compares ratios with 1. Use a zero no-change value before choosing it.')
   x<-log(x);if(k=='paired')y<-log(y)
  }
  values<-if(k=='paired')y-x else x
  null<-if(k=='one'&&isTRUE(s$log))log(s$null)else s$null
  group<-if(is.null(g))factor(rep(if(k=='paired')'After minus before'else'All observations',length(values)))else g
  plot_data<-data.frame(value=values,group=group)
  summary<-do.call(rbind,lapply(levels(group),function(l){v<-values[group==l];data.frame(Group=l,n=length(v),Mean=mean(v),SD=sd(v),Variance=var(v),Median=median(v),IQR=IQR(v),check.names=FALSE)}));rownames(summary)<-NULL
 }else{
  values<-NULL;null<-s$null;plot_data<-NULL;summary<-NULL
  if(k=='binary'){
   lev<-h_levels(x);if(length(lev)>2||!nzchar(event))stop('Choose a binary outcome and name the event category.')
   # An unobserved event is allowed, so zero-event samples remain usable.
   counts<-table(factor(ifelse(as.character(x)==event,'Event','Other'),levels=c('Other','Event')))
   expected<-c(Other=length(x)*(1-s$null),Event=length(x)*s$null)
   summary<-data.frame(Event=event,Events=as.integer(counts['Event']),Observed=length(x),Percent=100*as.integer(counts['Event'])/length(x))
  }else if(k=='paired_binary'){
   lev<-unique(c(h_levels(x),h_levels(y)))
   if(length(lev)>2||!event%in%lev)stop('Use the same two category labels at both times, and choose the event. Recode mismatched labels in Import / prepare data first.')
   counts<-table(Before=factor(ifelse(as.character(x)==event,'Event','Other'),levels=c('Other','Event')),After=factor(ifelse(as.character(y)==event,'Event','Other'),levels=c('Other','Event')))
   summary<-data.frame(Time=c('Before','After'),Events=c(sum(x==event),sum(y==event)),Observed=length(x),Percent=100*c(mean(x==event),mean(y==event)))
  }else{
   gx<-factor(as.character(x),levels=h_levels(x));gy<-factor(as.character(y),levels=h_levels(y))
   if(any(c(nlevels(gx),nlevels(gy))<2)||any(c(nlevels(gx),nlevels(gy))>10))stop('Each categorical variable needs 2–10 observed categories among complete rows.')
   counts<-table(gx,gy,dnn=c(s$x,s$y));expected<-outer(rowSums(counts),colSums(counts))/sum(counts);dimnames(expected)<-dimnames(counts)
  }
 }
 list(data=a,x=x,y=y,g=g,values=values,null=null,counts=counts,expected=expected,summary=summary,plot_data=plot_data,n=nrow(a),excluded=sum(!keep),total=nrow(d))
}
h_exact <- function(p,s) {
 if(s$method=='signed'){z<-p$values-p$null;return(length(z)<50&&!any(z==0)&&!anyDuplicated(signif(abs(z),7)))}
 if(s$method=='rank')return(all(table(p$g)<50)&&!anyDuplicated(signif(p$x,7)))
 FALSE
}
h_run <- function(d,s) {
 p<-h_prepare(d,s);m<-s$method;k<-s$route
 allowed<-switch(k,one=c('t_one','signed','sign'),paired=c('t_paired','signed','sign'),two=c('welch','pooled','rank'),many=c('anova','welch_anova','kruskal'),binary=c('binomial','prop'),categorical=c('chi','fisher','fisher_mc'),paired_binary=c('mcnemar','mcnemar_exact'))
 if(!m%in%allowed)stop('This test does not match the selected study structure.')
 if(!is.finite(s$alpha)||s$alpha<=0||s$alpha>=1)stop('Choose a valid significance level.')
 if(!s$alternative%in%c('two.sided','greater','less'))stop('Choose the direction of your question.')
 if(!is.finite(s$null))stop('Enter a finite reference value.')
 if(k=='binary'&&(s$null<=0||s$null>=1))stop('Use a reference percentage strictly between 0 and 100.')
 if(isTRUE(s$log)&&!m%in%c('t_one','t_paired','welch','pooled','anova','welch_anova'))stop('Non-parametric tests in this app use the original scale. Return to the original data before running one.')
 alt<-s$alternative;conf<-1-s$alpha;fit<-post<-effect<-NULL;notes<-character();warn<-character()
 if(k%in%c('two','many')&&any(table(p$g)<2))stop('Each group needs at least two complete observations for these comparisons.')
 if(m%in%c('t_one','t_paired')&&length(p$values)<2)stop('At least two complete observations or pairs are needed.')
 if(m%in%c('welch_anova','anova')&&any(p$summary$Variance<=0))stop('At least one group has no variation. This ANOVA route needs variation within every group; inspect the data before choosing another method.')
 exact<-h_exact(p,s)
 fit<-withCallingHandlers(switch(m,
  t_one=t.test(p$x,mu=p$null,alternative=alt,conf.level=conf),
  t_paired=t.test(p$y,p$x,paired=TRUE,mu=p$null,alternative=alt,conf.level=conf),
  welch=t.test(p$x[p$g==levels(p$g)[2]],p$x[p$g==levels(p$g)[1]],var.equal=FALSE,alternative=alt,conf.level=conf),
  pooled=t.test(p$x[p$g==levels(p$g)[2]],p$x[p$g==levels(p$g)[1]],var.equal=TRUE,alternative=alt,conf.level=conf),
  signed={if(!any(p$values!=p$null))stop('All values equal the no-change value. There are no non-zero differences for a signed-rank test.');wilcox.test(p$values,mu=p$null,alternative=alt,exact=exact,correct=TRUE,digits.rank=7)},
  rank=wilcox.test(p$x[p$g==levels(p$g)[2]],p$x[p$g==levels(p$g)[1]],alternative=alt,exact=exact,correct=TRUE,digits.rank=7),
  sign={z<-p$values-p$null;z<-z[z!=0];if(!length(z))stop('There are no non-zero differences for a sign test. Describe the unchanged observations.');binom.test(sum(z>0),length(z),p=.5,alternative=alt,conf.level=conf)},
  anova=oneway.test(value~group,data=p$plot_data,var.equal=TRUE),
  welch_anova=oneway.test(value~group,data=p$plot_data,var.equal=FALSE),
  kruskal=kruskal.test(value~group,data=p$plot_data),
  binomial=binom.test(as.integer(p$counts['Event']),p$n,p=s$null,alternative=alt,conf.level=conf),
  prop=prop.test(as.integer(p$counts['Event']),p$n,p=s$null,alternative=alt,conf.level=conf,correct=FALSE),
  chi=chisq.test(p$counts,correct=FALSE),
  fisher={if(!all(dim(p$counts)==2))stop('Use the simulated Fisher option for a table larger than 2 by 2.');fisher.test(p$counts,alternative='two.sided',conf.level=conf)},
  fisher_mc={if(all(dim(p$counts)==2))stop('For a 2 by 2 table, use Fisher’s exact calculation instead of simulation.');set.seed(2026);fisher.test(p$counts,simulate.p.value=TRUE,B=49999)},
  mcnemar={if(sum(p$counts[cbind(c(1,2),c(2,1))])==0)stop('No complete pairs changed category. McNemar testing has no discordant pairs to compare.');mcnemar.test(p$counts,correct=TRUE)},
  mcnemar_exact={b<-p$counts[1,2];c<-p$counts[2,1];if(b+c==0)stop('No complete pairs changed category. The exact test has no discordant pairs to compare.');binom.test(b,b+c,p=.5,alternative='two.sided',conf.level=conf)}
 ),warning=function(w){warn<<-c(warn,conditionMessage(w));invokeRestart('muffleWarning')})
 if(!is.finite(fit$p.value))stop('This test cannot be calculated from these data, for example because all measurements are identical. Inspect the observations and variation.')
 if(m%in%c('t_one','t_paired','welch','pooled','binomial','prop')){
  estimate<-if(k=='two')mean(p$x[p$g==levels(p$g)[2]])-mean(p$x[p$g==levels(p$g)[1]])else if(k=='binary')as.integer(p$counts['Event'])/p$n else mean(p$values)
  label<-switch(k,one='Population mean',paired='Mean change (after minus before)',two=paste('Mean difference:',levels(p$g)[2],'minus',levels(p$g)[1]),binary=paste('Proportion:',s$event))
  low<-fit$conf.int[1];high<-fit$conf.int[2]
  if(isTRUE(s$log)){estimate<-exp(estimate);low<-exp(low);high<-exp(high);label<-if(k=='one')'Geometric mean'else if(k=='paired')'Geometric mean of after/before ratios'else paste('Geometric mean ratio:',levels(p$g)[2],'/',levels(p$g)[1])}
  effect<-data.frame(Quantity=label,Estimate=estimate,Lower=low,Upper=high,Confidence=conf)
 }
 if(isTRUE(s$posthoc)&&k=='many'){
  adjust<-if(is.null(s$adjust))if(m=='anova')'tukey'else'holm'else s$adjust
  if(!adjust%in%c('tukey','holm','bonferroni')||(adjust=='tukey'&&m!='anova'))stop('Choose a correction appropriate to this follow-up method.')
  if(adjust=='tukey'){
   mod<-aov(value~group,data=p$plot_data);tk<-TukeyHSD(mod,conf.level=conf)$group
   post<-data.frame(Comparison=rownames(tk),Difference=tk[,1],Lower=tk[,2],Upper=tk[,3],Adjusted_p=tk[,4],Method='Tukey',row.names=NULL)
   notes<-c(notes,'Follow-up comparisons use Tukey simultaneous intervals and adjusted p-values for all pairs.')
  }else{
   pairs<-combn(levels(p$g),2,simplify=FALSE)
   mod<-if(m=='anova')lm(value~group,data=p$plot_data)else NULL
   post<-do.call(rbind,lapply(pairs,function(g){a<-p$x[p$g==g[2]];b<-p$x[p$g==g[1]]
    pv<-if(m=='kruskal')wilcox.test(a,b,exact=FALSE,correct=TRUE,digits.rank=7)$p.value else if(m=='anova')2*pt(-abs((mean(a)-mean(b))/(summary(mod)$sigma*sqrt(1/length(a)+1/length(b)))),df.residual(mod))else t.test(a,b,var.equal=FALSE)$p.value
    data.frame(Comparison=paste(g[2],'versus',g[1]),Unadjusted_p=pv)}))
   post$Adjusted_p<-p.adjust(post$Unadjusted_p,method=adjust)
   post$Method<-if(adjust=='holm')'Holm'else'Bonferroni'
   notes<-c(notes,paste('Follow-up comparisons use',if(m=='kruskal')'pairwise Wilcoxon rank-sum tests (normal approximation)'else if(m=='anova')'pairwise t-tests with a common SD from all groups'else'pairwise Welch t-tests','with',post$Method[1],'correction across all',nrow(post),'pairs.'))
  }
 }
 if(m%in%c('signed','rank'))notes<-c(notes,'Rank calculations use seven significant digits to avoid artificial distinctions caused by computer rounding.',if(exact)'An exact rank-test p-value is used: the samples are small and there are no relevant ties or zero differences.'else'A normal approximation with continuity correction is used for the rank test because of sample size, ties or zero differences. With very small tied samples this approximation may be poor.')
 if(m=='signed')notes<-c(notes,'The signed-rank test assumes a symmetric distribution of deviations from the stated centre. It is not a general test of a mean. If symmetry is doubtful, consider the sign test.')
 if(m=='sign')notes<-c(notes,paste(sum(p$values==p$null),'exact ties with the stated value are omitted. The sign test compares positive and negative deviations among the remaining observations; its usual median interpretation needs a continuous outcome.'))
 if(m%in%c('rank','kruskal'))notes<-c(notes,'Rank tests compare distributions through ranks. Interpreting the result solely as a median difference requires similarly shaped distributions. They do not test equality of means.')
 if(m%in%c('chi','prop'))notes<-c(notes,'The Pearson chi-squared approximation is used without a continuity correction. Small expected counts can make this approximation unreliable.')
 if(m=='fisher_mc')notes<-c(notes,'Monte Carlo p-value: 49,999 tables, seed 2026. This is a simulated approximation, not an exact numerical p-value; its smallest possible value is 1/50,000.')
 if(m=='mcnemar_exact')notes<-c(notes,paste('The exact binomial calculation uses only the',p$counts[1,2]+p$counts[2,1],'pairs that changed category. Its binomial interval would describe the direction among changers, not the difference in overall proportions.'))
 if(isTRUE(s$log)&&isTRUE(s$posthoc)&&k=='many')notes<-c(notes,'Follow-up differences and any intervals are on the natural log scale. Exponentiating a log difference gives a geometric mean ratio.')
 if(isTRUE(s$log))notes<-c(notes,'The test uses natural logarithms. Report geometric means or ratios, not an arithmetic mean difference on the original scale.')
 if(alt!='two.sided'&&k%in%c('one','paired','two','binary'))notes<-c(notes,'This is a one-sided test. The matching confidence bound is one-sided and has an open end. Choose direction before seeing the results.')
 c(p,list(test=fit,effect=effect,posthoc=post,notes=unique(c(notes,warn)),exact=exact,spec=s))
}
h_wrap <- function(x) vapply(x,function(v)paste(strwrap(v,width=26),collapse='\n'),character(1))
h_plot_width <- function(p) max(660,130*if(!is.null(p$g))nlevels(p$g)else if(is.matrix(p$counts))nrow(p$counts)else 1)
h_theme <- function() ggplot2::theme_minimal(base_size=12)+ggplot2::theme(legend.position='bottom',plot.title.position='plot',plot.title=ggplot2::element_text(face='bold',size=14),axis.text.x=ggplot2::element_text(angle=20,hjust=1),plot.margin=ggplot2::margin(12,20,12,12))
h_plots <- function(p,s) {
 if(h_numeric(s$route)){
  a<-p$plot_data;lab<-if(s$route=='paired')paste(s$y,'minus',s$x)else s$x
  if(isTRUE(s$log))lab<-if(s$route=='paired')paste0('log(',s$y,') minus log(',s$x,')')else paste('Natural log scale:',lab)
  lab<-h_wrap(lab)
  main<-ggplot2::ggplot(a,ggplot2::aes(x=group,y=value))+ggplot2::geom_boxplot(fill='#b8dfd9',outlier.shape=NA,width=.5)+ggplot2::geom_point(position=ggplot2::position_jitter(width=.10,height=0,seed=2026),alpha=.4,size=1.5)+ggplot2::labs(x=NULL,y=lab,title='Start with the observed measurements')+ggplot2::scale_x_discrete(labels=h_wrap)+h_theme()
  hist<-ggplot2::ggplot(a,ggplot2::aes(value))+ggplot2::geom_histogram(bins=15,fill='#087f8c',colour='white')+ggplot2::facet_wrap(~group,ncol=2,scales='free_y',labeller=ggplot2::label_wrap_gen(24))+ggplot2::labs(x=lab,y='Observations',title='Look at the shape in each group')+h_theme()
  qq<-ggplot2::ggplot(a,ggplot2::aes(sample=value))+ggplot2::stat_qq(size=1.3,alpha=.6)+ggplot2::stat_qq_line(colour='#7960ad')+ggplot2::facet_wrap(~group,ncol=2,labeller=ggplot2::label_wrap_gen(24))+ggplot2::labs(x='Values expected from a normal shape',y='Observed values',title='Do the dots roughly follow a straight line?')+h_theme()
  return(list(main=main,hist=hist,qq=qq,height=max(330,ceiling(nlevels(a$group)/2)*240)))
 }
 if(s$route=='binary'){
  a<-data.frame(Category=names(p$counts),Count=as.numeric(p$counts));main<-ggplot2::ggplot(a,ggplot2::aes(Category,Count))+ggplot2::geom_col(fill='#087f8c',width=.55)+ggplot2::labs(x=paste('Event =',s$event),y='People',title='How many people have each outcome?')+h_theme()
 }else if(s$route=='paired_binary'){
  a<-as.data.frame(p$counts);main<-ggplot2::ggplot(a,ggplot2::aes(Before,After,fill=Freq))+ggplot2::geom_tile(colour='white',linewidth=2)+ggplot2::geom_text(ggplot2::aes(label=Freq),size=5)+ggplot2::scale_fill_gradient(low='#f0f6f6',high='#63b5ad')+ggplot2::labs(title='The off-diagonal cells show who changed',subtitle=paste('Event =',s$event),fill='Pairs')+h_theme()
 }else{
  a<-as.data.frame(p$counts);names(a)<-c('Row','Column','Count');a$Percent<-100*a$Count/rep(rowSums(p$counts),ncol(p$counts))
  main<-ggplot2::ggplot(a,ggplot2::aes(Row,Percent,fill=Column))+ggplot2::geom_col(position='dodge')+ggplot2::labs(x=s$x,y='Percent within each\nrow category',fill=s$y,title='Compare the pattern of categories')+ggplot2::scale_x_discrete(labels=h_wrap)+ggplot2::scale_fill_discrete(labels=h_wrap)+ggplot2::guides(fill=ggplot2::guide_legend(ncol=2))+h_theme()
 }
 list(main=main,hist=NULL,qq=NULL,height=390)
}
h_ptext <- function(p) if(p<.001)'< 0.001'else paste('=',format.pval(p,digits=4,eps=.001))
h_interpret <- function(r,s) {
 m<-s$method;k<-s$route;pv<-r$test$p.value
 null<-switch(m,t_one=paste('the population mean equals',s$null),t_paired=paste('the population mean change (after minus before) equals',s$null),welch='the two population means are equal',pooled='the two population means are equal',anova='all population group means are equal',welch_anova='all population group means are equal',signed=paste('the distribution of',if(k=='paired')'paired changes'else'measurements','is symmetric about',s$null),rank='the two population distributions have the same location under the rank-test model',sign=paste('positive and negative deviations from',s$null,'are equally likely among non-ties'),kruskal='the population groups have the same distribution under the rank-test model',binomial=paste('the population event proportion equals',s$null),prop=paste('the population event proportion equals',s$null),chi='the two categorical variables are independent',fisher='the two categorical variables are independent',fisher_mc='the two categorical variables are independent',mcnemar='the two directions of change are equally likely',mcnemar_exact='the two directions of change are equally likely')
 if(isTRUE(s$log))null<-switch(k,one=paste('the population geometric mean equals',s$null),paired='the geometric mean of after/before ratios equals 1',two='the population geometric means are equal',many='all population group geometric means are equal')
 effect_text<-NULL
 if(!is.null(r$effect)){
  e<-r$effect;mult<-if(k=='binary')100 else 1;suffix<-if(k=='binary')'%'else''
  fmt<-function(v)paste0(format(signif(v*mult,4),trim=TRUE),suffix)
  effect_text<-paste0('Estimated ',tolower(e$Quantity),': ',fmt(e$Estimate),'. The ',100*(1-s$alpha),'% ',if(s$alternative=='two.sided')'confidence interval'else'one-sided confidence bound',' runs from ',fmt(e$Lower),' to ',fmt(e$Upper),'.')
  null_effect<-if(isTRUE(s$log)){if(k=='one')s$null else 1}else if(k%in%c('one','paired','binary'))s$null else 0
  if(s$alternative=='two.sided'&&e$Lower<=null_effect&&e$Upper>=null_effect)effect_text<-c(effect_text,'This interval includes the value stated by the null hypothesis. That value remains compatible with these observations and assumptions, alongside other values in the interval. This does not prove the null hypothesis.')
 }
 c(paste0('The null hypothesis says that ',null,'.'),effect_text,
 if(!is.null(r$test$stderr))paste0('The standard error is ',format(signif(r$test$stderr,4),trim=TRUE),if(isTRUE(s$log))' on the log scale'else'',', describing sampling uncertainty in this mean or mean difference. The t statistic compares its distance from the null value with this uncertainty.'),
 paste0(unname(h_methods[m]),': p ',h_ptext(pv),'. At the chosen ',100*s$alpha,'% significance level, ',if(pv<s$alpha)'the result provides evidence against this null hypothesis, under the test assumptions.'else'we do not have enough evidence to reject this null hypothesis. This does not establish that there is no difference or association.'),
 if(k=='many')'An overall result does not identify which groups differ. Use the optional adjusted follow-up comparisons to investigate individual pairs.'else NULL,
 'A p-value describes how unusual results this extreme or more extreme would be if the null hypothesis and the test assumptions held. It is not the probability that the null hypothesis is true.',
 'Read the observed sizes, uncertainty and study design alongside the p-value. Statistical significance does not establish practical importance or causation.')
}
