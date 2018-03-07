sub.dir<-paste0(rv$out.dir, '/', paste(sub.id(), time.id(), study.id(), 'Output/', sep = '_'))
dirList<-list.dirs(rv$out.dir)
sub.dir2<-paste0(rv$out.dir, '/', paste(sub.id(), time.id(), study.id(), 'Output', sep = '_'))
if(sum(dirList==sub.dir2)==0){dir.create(sub.dir)}
jpeg(paste0(sub.dir, sub.id(), time.id(), study.id(),round(time.min*100), '_', round(time.max*100), '.jpeg'), 
     res = 300, units = 'in', width = 8, height = 8)
traceplot(fit.stan, pars='HR')+
ggtitle(paste(sub.id(), time.id(), study.id(), "HR Traceplot GP Imputation:", 
              round(time.min, digits = 2), 'to', round(time.max, digits = 2)))
dev.off()

fit.summary<-summary(fit.stan, pars=pars.to.monitor[-3], probs=c(.01, .99))$summary
print(fit.summary)

sink(paste0(sub.dir, sub.id(), time.id(), study.id(),round(time.min*100), '_', round(time.max*100),'.txt'))
cat(paste0('ID:', '\t\t\t', id))
cat(paste0('T1:', '\t\t\t', round(time.min, digits = 2)))
cat(paste0('T2:', '\t\t\t', round(time.max, digits = 2)))
cat(paste0('Total Time', '\t\t\t', round(time.max, digits = 2)-round(time.min, digits = 2)))
cat(paste0('Run Time:', '\t\t\t', run_time=round(run_time, digits = 2)))
cat(paste0('MAP HR:', '\t\t\t', round(mu_HR2*60, digits = 2)))
cat(paste0('MAP R:', '\t\t\t',round(mu_R2*60, digits = 2)))
cat(paste0('adapt_delta:', '\t\t\t', rv$delta))
cat(paste0('Iterations:', '\t\t\t', rv$GP.iter))
cat(paste0('Warmup:', '\t\t\t', rv$GP.wrm))
cat('\n\nSystem Information:')
cat('-------------------------------------------------------------------------------------')
cat(paste0('Processor:', '\t\t\t', get_cpu()$model_name))
cat(paste0('Number of Cores:', '\t\t\t', detectCores(logical=F)))
cat(paste0('Number of Threads:', '\t\t\t', detectCores(logical=T)))
cat(paste0('RAM:', '\t\t\t', get_ram()))
cat('-------------------------------------------------------------------------------------')
cat('\nGP SUMMARY:', '\n',fit.summary)
sink()
