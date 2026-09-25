reg_routes <- c(simple='One predictor',adjusted='Several predictors and adjustment',interaction='Interactions')
reg_prompts <- c(simple='Relate a numerical outcome to one numerical or categorical predictor.',adjusted='Compare people with the same values of other selected predictors.',interaction='Investigate whether an association differs across another variable.')
reg_example <- function(route='interaction'){
 set.seed(52026);n<-120;programme<-rep(c('Usual routine','Walking','Workshop'),each=40)
 age<-round(runif(n,20,65));sleep_hours<-round(pmax(4,pmin(10,5+.035*age+rnorm(n,0,.7))),2)
 wellbeing_score<-round(18+3.4*sleep_hours+.36*age+ifelse(programme=='Walking',-7+1.5*sleep_hours,ifelse(programme=='Workshop',10-.8*sleep_hours,0))+rnorm(n,0,4),1)
 d<-data.frame(wellbeing_score,sleep_hours,age,programme,check.names=FALSE)
 predictors<-if(route=='simple')'sleep_hours'else c('sleep_hours','age','programme')
 s<-list(outcome='wellbeing_score',predictors=predictors,categorical=intersect('programme',predictors),references=list(programme='Usual routine'),interactions=if(route=='interaction')list(c('sleep_hours','programme'))else list(),conf=.95)
 list(raw=d,spec=s,meta=list(kind='csv',name='regression_example.csv',delim=',',decimal='.',encoding='UTF-8',na=c('','NA'),example=TRUE,reg_example=TRUE))
}
reg_lesson_titles <- c('What question can regression answer?','Turn the fitted line into an equation','Understand categorical predictors','Adjustment and confounding','Interactions and group-specific equations','Check the model assumptions','Interpret uncertainty and predict')
reg_lesson_state <- function(i){
 t<-reg_example(if(i<=2)'simple'else if(i<=4)'adjusted'else'interaction')
 if(i==3){t$spec$predictors<-'programme';t$spec$categorical<-'programme'}
 if(i==4){t$spec$predictors<-c('sleep_hours','age');t$spec$categorical<-character();t$spec$references<-list()}
 t$result<-reg_fit(t$raw,t$spec)
 t$view<-list(focal=if(i==3)'programme'else'sleep_hours',by=if(i>=5)'programme'else'',strata=if(i>=5)'programme'else'',effect=if(i==3)'programme'else'sleep_hours',profile=reg_profile(t$result))
 t
}
reg_lesson_text <- function(i,t){
 r<-t$result;s<-t$spec;b<-coef(r$fit);f<-function(x)format(round(x,3),trim=TRUE)
 switch(as.character(i),
 '1'=c('Imagine measuring sleep and wellbeing for 120 adults. Each dot below is one fictional person. We want to describe how average wellbeing changes as sleep increases. Sleep is the predictor; wellbeing is the outcome we want to explain. The outcome needs to be a numerical measurement.',
 'A straight line summarises the average pattern. People with the same sleep time will not all have the same wellbeing. Regression leaves room for those individual differences. The line predicts a mean, not an exact score for every person.',
 'An independent variable is another name for a predictor. It does not mean the predictors must be unrelated to one another. Independent observations means a different issue: the rows should represent independent people rather than repeated visits or clustered families.',
 'Begin with the plot and what each row represents. Do not select variables just because their p-values are small. These fictional data illustrate calculations; they are not evidence about actual sleep or programme effects.'),
 '2'=c(paste0('Our fitted equation is: predicted wellbeing = ',f(b[1]),if(b[2]>=0)' + 'else' − ',f(abs(b[2])),' × sleep hours. The table and line below use these same fitted coefficients.'),
 paste0('The slope is ',f(b[2]),'. Two otherwise comparable people whose sleep differs by one hour have predicted mean wellbeing scores differing by ',f(b[2]),' points in this simple model. For two hours, multiply that slope by 2.'),
 paste0('The intercept is ',f(b[1]),': the equation’s prediction at zero hours of sleep. Our participants sleep between ',min(t$raw$sleep_hours),' and ',max(t$raw$sleep_hours),' hours. Zero is outside that range, so we should not treat the intercept as a realistic wellbeing prediction.'),
 'R code such as lm(wellbeing_score ~ sleep_hours, data = analysis) reads “model wellbeing using sleep hours in analysis”. lm means linear model. The tilde ~ separates the outcome from its predictors. Save the result as model, then summary(model) shows estimates and tests; confint(model) gives confidence intervals.',
 'The fitted line minimises the sum of squared vertical gaps between observations and predictions. Those gaps are residuals. Later we will inspect their pattern rather than expecting every point to lie on the line.'),
 '3'=c('A categorical predictor tells us which group someone belongs to. Programme has three levels: Usual routine, Walking and Workshop. The names are not amounts, so we cannot multiply a programme name by a coefficient.',
 'Instead, we make two new columns, called dummy variables or indicator variables. Each can only be 0 or 1. Let x_1 be 1 for Walking and 0 otherwise. Let x_2 be 1 for Workshop and 0 otherwise. These are two columns describing the same original predictor, not two new characteristics of the person.',
 'Usual routine is the reference category: both x_1 and x_2 equal 0. For Walking, x_1 = 1 and x_2 = 0. For Workshop, x_1 = 0 and x_2 = 1. Nobody in this three-category example has both equal to 1.',
 'With an intercept, three categories need two dummy variables. More generally, K categories need K − 1 dummy variables. The intercept already represents the reference mean. A third indicator as well as an intercept would repeat information already supplied by the other columns.',
 'Below we replace x_1 and x_2 by the right 0s and 1s for each group. Here y means the predicted mean wellbeing score. Multiplying a term by 0 removes it; multiplying by 1 leaves it unchanged.'),
 '4'=c('Start with three variables: the outcome is wellbeing, our main predictor is sleep, and a possible confounder is age. Imagine that age influences both how long people sleep and their wellbeing. Then a sleep-only comparison can mix a sleep association with differences in age.',
 'In this simple setting, a confounder is a third variable associated with the main predictor and with the outcome, and is not a consequence of the main predictor along the pathway to the outcome. For example, age is not something that sleeping an extra hour causes. Associations with both variables are clues; we also need to understand the timing and how the variables are connected.',
 'If sleep had no effect of its own, ignoring age could still make sleep appear related to wellbeing. Confounding can also make an existing association look stronger, weaker or even point in the opposite direction. It does not always make the entire association disappear after adjustment.',
 'Adjustment here means comparing sleep values while holding age fixed in the model: asking about people of the same age. We include only sleep and age in this lesson so that the two predictors match the circles below. Both fitted models use exactly the same 120 participants.',
 'First look at the sleep circle on its own. Then reveal age: part of what sleep appeared to explain is shared with age. The part unique to sleep is smaller than the whole sleep circle. This picture helps explain shared information; it does not measure a coefficient, and overlap by itself does not prove confounding.',
 'The fitted coefficient is a separate calculation, in wellbeing points per extra hour of sleep. Read the actual before-and-after values below. Adjustment need not always reduce a coefficient; this is one illustrative pattern. A change alone does not prove causation or tell us every variable that should be included.'),
 '5'=c('An interaction asks whether an association differs across another variable. In our example, we allow the sleep–wellbeing slope to differ between Usual routine, Walking and Workshop. The direction and size must come from the fitted results, rather than from assuming one programme has the steepest slope.',
 'Reuse the two dummy variables from the categorical lesson: x_1 = 1 for Walking, x_2 = 1 for Workshop, and both are 0 for Usual routine. Let s mean sleep hours and a mean age in years. The extra terms s × x_1 and s × x_2 change the sleep slope in the corresponding programme.',
 'Start with the full fitted equation below. Substitute the dummy values for one programme, cross out the terms multiplied by 0, and collect the constant terms and the terms multiplying sleep. Repeat for the other programmes. This shows exactly where each of the three equations comes from.',
 'All three equations come from the one full model. The age term stays the same because this model gives age a common coefficient. We use the full model’s coefficient uncertainty when calculating each combined sleep slope; adding standard errors together would be incorrect.',
 'To ask whether the slopes differ, use the interaction test. A significant sleep slope in one group and a non-significant slope in another is not by itself evidence of a difference between slopes. The worked interpretation below distinguishes the individual slopes, their differences and the joint interaction test.',
 'In R, sleep_hours * programme expands to sleep_hours + programme + sleep_hours:programme. The colon supplies the extra products. The analysis page also supports two categorical variables (a difference of differences) or two numerical variables (a slope that changes with the other measurement).'),
 '6'=c('After fitting the model, calculate each person’s residual: observed wellbeing minus predicted wellbeing. A positive residual means the person scored above the prediction; a negative one means below. These differences tell us what the fitted equation did not explain.',
 'In the residual-versus-fitted plot, look for a cloud centred around zero. A curve suggests that the mean relationship is not represented well by straight-line terms. A widening funnel suggests that the remaining variation changes with the fitted mean, so the usual standard errors and tests may be unsuitable.',
 'The Q–Q plot checks the shape of residuals. Rough alignment with the line supports a normal approximation for the errors, particularly relevant for small-sample tests and intervals. Predictors themselves do not have to be normally distributed, and the outcome does not have to look normal when all groups are mixed together.',
 'Cook’s distance highlights observations that could noticeably influence the fitted coefficients. Investigate their original records, units and context. A large value is a prompt to inspect, not a reason to delete a participant. No observations are automatically removed here.',
 'Independence comes from how the study was conducted; a residual plot cannot establish it. Repeated observations, household clusters, strong curvature or unequal error variation may need methods beyond this introductory ordinary linear regression. Do not treat a significant result as a solution to a poor fit.'),
 '7'=c('A coefficient is an estimated association. Its standard error describes uncertainty in that coefficient across studies like this one. The confidence interval shows a range of values compatible with the data and model at the chosen confidence level.',
 'For the matching two-sided coefficient test, a 95% interval excluding zero corresponds to p < 0.05, apart from rounding. An interval including zero does not prove no association. Read the possible effect sizes: a wide interval can include both little association and an important one.',
 'R-squared describes the fraction of observed outcome variation accounted for by the fitted model in this sample. It is not prediction accuracy in new people, a causal measure, or the percentage of individuals correctly predicted. Adjusted R-squared penalises extra parameters and can be negative.',
 'Now imagine someone with the profile printed below. A confidence interval describes uncertainty about the average wellbeing of people with that profile. A prediction interval describes one new person and also allows for individual variation; it is therefore wider under the same model. Avoid prediction beyond observed ranges or for unsupported combinations.',
 'Report the outcome and units, selected predictors and reference categories, interactions, included and excluded counts, coefficients with intervals, and what the diagnostics showed. The displayed coefficient and conditional-effect tests are unadjusted; repeatedly searching many models creates a multiple-testing problem.')
 )
}

# Worked algebra is calculated from the same fitted examples as the tables.
reg_lesson_num <- function(x) formatC(as.numeric(x),digits=4,format='f')
reg_lesson_term <- function(b,label='') paste0(if(b<0)' - 'else' + ',reg_lesson_num(abs(b)),if(nzchar(label))paste0(' × ',label)else'')
reg_dummy_worked <- function(t){
 r<-t$result;b<-coef(r$fit);lev<-levels(r$data$programme)
 coding<-data.frame(Programme=lev,x_1=c(0L,1L,0L),x_2=c(0L,0L,1L),check.names=FALSE)
 means<-as.numeric(predict(r$fit,newdata=data.frame(programme=factor(lev,levels=lev))))
 eq<-paste0('y = ',reg_lesson_num(b[1]),reg_lesson_term(b[2],'x_1'),reg_lesson_term(b[3],'x_2'))
 substitutions<-vapply(1:3,function(i)paste0('y = ',reg_lesson_num(b[1]),reg_lesson_term(b[2],as.character(coding$x_1[i])),reg_lesson_term(b[3],as.character(coding$x_2[i])),'\n  = ',reg_lesson_num(means[i])),character(1))
 list(coding=coding,equation=eq,means=means,substitutions=substitutions)
}
reg_lesson_equation_box <- function(x) shiny::tags$pre(class='lesson-equation',style='white-space:pre-wrap;overflow-wrap:anywhere;max-height:none;font-size:15px;',x)
reg_dummy_ui <- function(t){
 w<-reg_dummy_worked(t);b<-coef(t$result$fit)
 shiny::tagList(shiny::h4('1. Create the two dummy variables'),
  shiny::p('The coding table tells us which numbers to put into the equation. x_1 represents Walking; x_2 represents Workshop.'),
  shiny::div(class='table-scroll',shiny::tags$table(class='table',shiny::tags$thead(shiny::tags$tr(lapply(names(w$coding),shiny::tags$th))),shiny::tags$tbody(lapply(1:3,function(i)shiny::tags$tr(lapply(w$coding[i,],shiny::tags$td)))))),
  shiny::h4('2. Write the fitted equation'),reg_lesson_equation_box(w$equation),
  shiny::p('The first number is the reference mean. The next two numbers are differences from that mean, so we add them only for people in the relevant group.'),
  shiny::h4('3. Substitute the values for each programme'),
  lapply(1:3,function(i)shiny::div(class='worked-example',shiny::h4(w$coding$Programme[i]),shiny::p(paste0('x_1 = ',w$coding$x_1[i],' and x_2 = ',w$coding$x_2[i],'.')),reg_lesson_equation_box(w$substitutions[i]),
   shiny::p(paste0('The predicted mean wellbeing score is ',reg_lesson_num(w$means[i]),' points.',if(i>1)paste0(' This is ',reg_lesson_num(abs(b[i])),' points ',if(b[i]>=0)'higher'else'lower',' than Usual routine.'))))),
  shiny::p('These are predicted group means, not the score every individual in a group must have. In this model containing programme alone, they also equal the three sample means. Calculations use unrounded coefficients.'),
  shiny::h4('4. Connect this to the R output'),shiny::p('R labels x_1 as programmeWalking and x_2 as programmeWorkshop in the coefficient table. lm() creates these columns automatically when programme is a factor. The optional code below constructs them explicitly so you can see the same result.'),
  shiny::p('Changing the reference changes the written coefficients, not the three predicted means. The overall test for programme considers both dummy coefficients together.'))
}
reg_confounding_svg <- function(reveal=FALSE){
 # Schematic areas: deliberately not a numerical variance decomposition.
 age<-if(reveal)'<circle cx="300" cy="245" r="115" fill="#7962AD" fill-opacity="0.34" stroke="#7962AD" stroke-width="3"/><text x="345" y="243" text-anchor="middle"><tspan x="345">Age</tspan><tspan x="345" dy="25">only</tspan></text><text x="238" y="235" text-anchor="middle" font-size="17"><tspan x="238">Shared</tspan><tspan x="238" dy="23">part</tspan></text>'else''
 sleep<-if(reveal)'<text x="120" y="243" text-anchor="middle"><tspan x="120">Sleep</tspan><tspan x="120" dy="25">only</tspan></text>'else'<text x="175" y="238" text-anchor="middle"><tspan x="175">Variation explained</tspan><tspan x="175" dy="25">by sleep alone</tspan></text>'
 shiny::HTML(paste0('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 480 510" style="display:block;width:100%;max-width:580px;height:auto;margin:auto" role="img" aria-label="Schematic: square is all wellbeing variation; sleep and age have overlapping explained variation"><title>Shared information when adjusting for age</title><g font-family="system-ui,sans-serif" font-size="20" fill="#19343d"><text x="240" y="28" text-anchor="middle" font-weight="700">All variation in wellbeing scores</text><rect x="30" y="50" width="420" height="420" rx="4" fill="#FAFCFC" stroke="#19343d" stroke-width="2"/><circle cx="175" cy="245" r="115" fill="#087F8C" fill-opacity="0.30" stroke="#087F8C" stroke-width="3"/>',age,sleep,'<text x="240" y="408" text-anchor="middle" font-size="18"><tspan x="240">Variation not explained</tspan><tspan x="240" dy="25">by the displayed predictor(s)</tspan></text></g></svg>'))
}
reg_confounding_text <- function(t){
 r<-t$result;rows<-reg_compare(r,'sleep_hours');u<-rows[1,];a<-rows[2,];f<-reg_lesson_num
 age<-r$coefficients[r$coefficients$Term=='age',];delta<-u$Estimate-a$Estimate
 c(paste0('Before adjustment: an extra hour of sleep is associated with ',f(abs(u$Estimate)),' ',if(u$Estimate>=0)'higher'else'lower',' wellbeing points on average (95% CI ',f(u$Lower),' to ',f(u$Upper),'; p ',if(u$p_value<.0001)reg_p(u$p_value)else paste0('= ',reg_p(u$p_value)),'). This comparison mixes participants of different ages.'),
 paste0('After adjustment for age: among participants of the same age in the model, an extra hour of sleep is associated with ',f(abs(a$Estimate)),' ',if(a$Estimate>=0)'higher'else'lower',' wellbeing points (95% CI ',f(a$Lower),' to ',f(a$Upper),'; p ',if(a$p_value<.0001)reg_p(a$p_value)else paste0('= ',reg_p(a$p_value)),'). The sleep coefficient ',if(delta>=0)'decreases'else'increases',' by ',f(abs(delta)),' points per hour after including age.'),
 paste0('The adjusted age coefficient is ',f(age$Estimate),': for people reporting the same sleep duration, each additional year of age is associated with that many wellbeing points on average in this model. The data also show that age and sleep are ',if(cor(r$data$age,r$data$sleep_hours)>=0)'positively'else'negatively',' associated.'),
 if(a$Lower<=0&&a$Upper>=0)'The adjusted interval includes zero. The data are compatible with no remaining sleep association under this model, but do not prove that sleep has no effect.'else'The adjusted interval still excludes zero. In this worked example, accounting for age reduces the apparent sleep association but does not remove it. We should not describe this as an association that vanished.',
 'This pattern is consistent with age accounting for part of the crude association. It is not proof that the remaining association is causal: these are fictional observational data, and selecting adjustment variables still needs knowledge of how the study and variables work.')
}
reg_confounding_plot <- function(t){
 r<-t$result;nd<-data.frame(sleep_hours=seq(min(r$data$sleep_hours),max(r$data$sleep_hours),length.out=80),age=median(r$data$age))
 u<-lm(wellbeing_score~sleep_hours,data=r$data)
 a<-predict(u,nd,interval='confidence');b<-predict(r$fit,nd,interval='confidence')
 d<-data.frame(Sleep=rep(nd$sleep_hours,2),Mean=c(a[,1],b[,1]),Lower=c(a[,2],b[,2]),Upper=c(a[,3],b[,3]),Model=rep(c('Sleep alone',paste('Sleep adjusted for age (age =',median(r$data$age),'years)')),each=nrow(nd)))
 ggplot2::ggplot(d,ggplot2::aes(Sleep,Mean,colour=Model,fill=Model))+ggplot2::geom_ribbon(ggplot2::aes(ymin=Lower,ymax=Upper),alpha=.1,colour=NA)+ggplot2::geom_line(linewidth=1)+ggplot2::labs(x='Sleep (hours)',y='Predicted mean wellbeing score',colour=NULL,fill=NULL)+ggplot2::guides(colour=ggplot2::guide_legend(ncol=1),fill=ggplot2::guide_legend(ncol=1))+reg_theme()
}
reg_interaction_worked <- function(t){
 r<-t$result;b<-coef(r$fit);f<-reg_lesson_num
 b0<-b['(Intercept)'];bs<-b['sleep_hours'];ba<-b['age'];b1<-b['programmeWalking'];b2<-b['programmeWorkshop'];j1<-b['sleep_hours:programmeWalking'];j2<-b['sleep_hours:programmeWorkshop']
 full<-paste0('y = ',f(b0),reg_lesson_term(bs,'s'),reg_lesson_term(ba,'a'),reg_lesson_term(b1,'x_1'),reg_lesson_term(b2,'x_2'),reg_lesson_term(j1,'s × x_1'),reg_lesson_term(j2,'s × x_2'))
 coding<-data.frame(Programme=levels(r$data$programme),x_1=c(0,1,0),x_2=c(0,0,1));steps<-list()
 for(i in 1:3){x1<-coding$x_1[i];x2<-coding$x_2[i];int<-b0+b1*x1+b2*x2;slope<-bs+j1*x1+j2*x2
  sub<-paste0('y = ',f(b0),reg_lesson_term(bs,'s'),reg_lesson_term(ba,'a'),reg_lesson_term(b1,as.character(x1)),reg_lesson_term(b2,as.character(x2)),reg_lesson_term(j1,paste0('s × ',x1)),reg_lesson_term(j2,paste0('s × ',x2)))
  collect<-paste0('y = (',f(b0),if(x1)reg_lesson_term(b1)else'',if(x2)reg_lesson_term(b2)else'',')\n    + (',f(bs),if(x1)reg_lesson_term(j1)else'',if(x2)reg_lesson_term(j2)else'',') × s',reg_lesson_term(ba,'a'))
  final<-paste0('y = ',f(int),reg_lesson_term(slope,'s'),reg_lesson_term(ba,'a'))
  steps[[i]]<-list(group=coding$Programme[i],x1=x1,x2=x2,substitute=sub,collect=collect,equation=final,intercept=as.numeric(int),slope=as.numeric(slope))
 }
 list(full=full,steps=steps,age=as.numeric(ba))
}
reg_interaction_ui <- function(t){
 w<-reg_interaction_worked(t)
 shiny::tagList(shiny::h4('1. Identify the variables'),shiny::p('y = predicted mean wellbeing; s = sleep hours; a = age in years. x_1 = 1 for Walking, otherwise 0. x_2 = 1 for Workshop, otherwise 0. For Usual routine both dummies are 0.'),
  shiny::h4('2. Start with the full fitted equation'),reg_lesson_equation_box(w$full),shiny::p('The last two terms are the extra sleep slopes for Walking and Workshop compared with Usual routine.'),
  shiny::h4('3. Substitute, collect and simplify'),
  lapply(w$steps,function(z)shiny::div(class='worked-example',shiny::h4(z$group),shiny::p(paste0('Put x_1 = ',z$x1,' and x_2 = ',z$x2,' into the full equation:')),reg_lesson_equation_box(z$substitute),shiny::p('Terms multiplied by 0 vanish. Collect the constants and the terms multiplying sleep:'),reg_lesson_equation_box(z$collect),shiny::p('The resulting equation is:'),reg_lesson_equation_box(z$equation),shiny::p(paste0('For participants in ',z$group,' of the same age, one extra hour of sleep corresponds to ',reg_lesson_num(abs(z$slope)),' ',if(z$slope>=0)'higher'else'lower',' predicted wellbeing points on average.')))),
  shiny::p('All three equations retain the same age coefficient. Their intercepts refer to zero sleep and zero age, outside the observed data; use them as equation components rather than realistic people. Displayed values are rounded; all results use the unrounded model.'),
  shiny::h4('4. Use an equation for a person’s profile'),reg_lesson_equation_box(paste0('For Walking, at s = 7 hours and a = 40 years:\ny = ',reg_lesson_num(w$steps[[2]]$intercept),reg_lesson_term(w$steps[[2]]$slope,'7'),reg_lesson_term(w$age,'40'),'\n  = ',reg_lesson_num(w$steps[[2]]$intercept+7*w$steps[[2]]$slope+40*w$age))),shiny::p('This is a predicted mean for that profile, not a guaranteed wellbeing score for an individual.'))
}
reg_interaction_text <- function(t){
 r<-t$result;ef<-reg_effects(r,'sleep_hours','programme',t$view$profile)$table;ct<-r$coefficients;f<-reg_lesson_num
 lines<-vapply(1:nrow(ef),function(i){a<-ef[i,];paste0(a$Stratum,': the estimated sleep slope is ',f(a$Estimate),' wellbeing points per hour (95% CI ',f(a$Lower),' to ',f(a$Upper),'; p ',if(a$p_value<.0001)reg_p(a$p_value)else paste0('= ',reg_p(a$p_value)),'). ',if(a$Lower<=0&&a$Upper>=0)'This interval includes zero; it also includes non-zero slopes, so it does not establish no association.'else'This interval excludes zero, giving evidence of a non-zero slope under the model.')},character(1))
 for(g in c('Walking','Workshop')){a<-ct[ct$Term==paste0('sleep_hours:programme',g),];lines<-c(lines,paste0(g,' versus Usual routine: the sleep slope differs by ',f(a$Estimate),' points per hour (95% CI ',f(a$Lower),' to ',f(a$Upper),'; p ',if(a$p_value<.0001)reg_p(a$p_value)else paste0('= ',reg_p(a$p_value)),'). ',if(a$Lower<=0&&a$Upper>=0)'These data do not clearly distinguish this pair of slopes; equality is not proven.'else paste0('This supports a ',if(a$Estimate<0)'shallower'else'steeper',' slope in ',g,' than in Usual routine.')))}
 pv<-r$interaction_tests$p_value[1]
 c(lines,paste0('Joint interaction test: p ',if(pv<.0001)reg_p(pv)else paste0('= ',reg_p(pv)),'. ',if(pv<.05)'At the 5% threshold, there is evidence that the sleep slope is not the same across all three programmes, after accounting for age.'else'At the 5% threshold, the data do not clearly establish different slopes across the three programmes. That does not prove the slopes are identical.'),
 'These comparisons concern slopes. A programme’s main coefficient instead compares its predicted mean with Usual routine at zero sleep, which is outside this example’s range. Use a realistic sleep value and the full equations to compare predicted means. Intervals and tests here are unadjusted; the fictional example does not establish a programme effect in real people.')
}
reg_lesson_student_script <- function(i,t){
 if(i==4)return(reg_confounding_student_script(t))
 code<-reg_student_script(t$meta,list(),t$spec,t$raw,t$view)
 if(i==3)code<-paste(code,'\n# See explicitly how R turns three categories into two dummy variables.\nanalysis$x_1 <- ifelse(analysis$programme == "Walking", 1, 0)\nanalysis$x_2 <- ifelse(analysis$programme == "Workshop", 1, 0)\nhead(analysis[c("programme", "x_1", "x_2")])\ndummy_model <- lm(wellbeing_score ~ x_1 + x_2, data = analysis)\ncoef(dummy_model)\n# These are the same three coefficients as the factor model above.\ncoding <- data.frame(x_1 = c(0, 1, 0), x_2 = c(0, 0, 1))\nrownames(coding) <- c("Usual routine", "Walking", "Workshop")\npredict(dummy_model, newdata = coding)',sep='\n')
 if(i==5)code<-paste(code,'\n# Derive the group-specific equations from the same full model.\nb <- coef(model)\n# The common age coefficient:\nb["age"]\n# Intercept and sleep slope for each programme:\nprogramme_equations <- data.frame(\n  programme = c("Usual routine", "Walking", "Workshop"),\n  intercept = b["(Intercept)"] + c(0, b["programmeWalking"], b["programmeWorkshop"]),\n  sleep_slope = b["sleep_hours"] + c(0, b["sleep_hours:programmeWalking"],\n    b["sleep_hours:programmeWorkshop"]), age_slope = b["age"])\nprogramme_equations\n# For Walking at 7 hours and 40 years:\nwith(programme_equations[2, ], intercept + 7 * sleep_slope + 40 * age_slope)',sep='\n')
 code
}

reg_confounding_student_script <- function(t){
 paste(c(learner_preamble(t$meta,list()),'',
 '# 2. Use exactly the same people in both models',learner_clean(c('wellbeing_score','sleep_hours','age')),
 'nrow(analysis)',
 '','# 3. Describe the associations with age',
 'cor(analysis$age, analysis$sleep_hours)',
 'cor(analysis$age, analysis$wellbeing_score)',
 '# These sample associations are clues, not proof of confounding.',
 '','# 4. First relate wellbeing to sleep alone',
 'unadjusted <- lm(wellbeing_score ~ sleep_hours, data = analysis)',
 'summary(unadjusted)','confint(unadjusted)',
 '','# 5. Add age to compare sleep values at the same age',
 'model <- lm(wellbeing_score ~ sleep_hours + age, data = analysis)',
 'summary(model)','confint(model)',
 '# ~ separates the outcome from its predictors; + adds age to the model.',
 '# coef() extracts the fitted equation coefficients.',
 'coef(unadjusted)','coef(model)',
 '','# 6. Put the two sleep estimates next to one another',
 'comparison <- data.frame(',
 '  model = c("Sleep alone", "Sleep adjusted for age"),',
 '  sleep_slope = c(coef(unadjusted)["sleep_hours"], coef(model)["sleep_hours"]),',
 '  lower = c(confint(unadjusted)["sleep_hours", 1], confint(model)["sleep_hours", 1]),',
 '  upper = c(confint(unadjusted)["sleep_hours", 2], confint(model)["sleep_hours", 2]))',
 'comparison',
 '','# 7. Compare the fitted lines over the observed sleep range',
 '# For the adjusted line, choose one age: the sample median.',
 'plot_data <- data.frame(sleep_hours = seq(min(analysis$sleep_hours),',
 '  max(analysis$sleep_hours), length.out = 80), age = median(analysis$age))',
 'plot_data$unadjusted <- predict(unadjusted, newdata = plot_data)',
 'plot_data$adjusted <- predict(model, newdata = plot_data)',
 'ggplot(plot_data, aes(x = sleep_hours)) +',
 '  geom_line(aes(y = unadjusted, colour = "Sleep alone")) +',
 '  geom_line(aes(y = adjusted, colour = "Adjusted for age")) +',
 '  labs(y = "Predicted mean wellbeing", colour = NULL) + theme_minimal()',
 '# The overlapping circles in the tutorial are a conceptual illustration.',
 '# They do not give numerical areas or calculate these coefficients.'),collapse='\n')
}
reg_lesson_exact_script <- function(i,t){
 code<-reg_exact_script(t$meta,list(),t$spec,t$view)
 fn<-c('reg_lesson_num','reg_lesson_term',switch(as.character(i),'3'='reg_dummy_worked','4'='reg_confounding_plot','5'='reg_interaction_worked',character()))
 body<-vapply(fn,function(n)paste0(n,' <- ',paste(deparse(get(n,mode='function')),collapse='\n')),character(1))
 paste(c(code,'# Additional worked tutorial calculation',body,'example <- list(result = result, view = view)',switch(as.character(i),'3'='print(reg_dummy_worked(example))','4'='print(reg_confounding_plot(example))','5'='print(reg_interaction_worked(example))',character())),collapse='\n\n')
}
