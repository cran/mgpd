bgpd_maxlik <-
function(param,dat,model="log",fixed=FALSE,...)
{
mlmax=1e+15
models      = c("log","psilog","neglog","psineglog","bilog","negbilog","mix","ct","taj")
if(!(model %in% models)) stop(paste("'",model,"' is not a valid model.",sep="")) else{
if(model=="log")        ml  = ml_log(        param=param,dat=dat,mlmax=mlmax,fixed=fixed)
if(model=="psilog")     ml  = ml_psilog(     param=param,dat=dat,mlmax=mlmax,fixed=fixed)
if(model=="neglog")     ml  = ml_neglog(     param=param,dat=dat,mlmax=mlmax,fixed=fixed)
if(model=="psineglog")  ml  = ml_psineglog(  param=param,dat=dat,mlmax=mlmax,fixed=fixed)
if(model=="bilog")      ml  = ml_bilog(      param=param,dat=dat,mlmax=mlmax,fixed=fixed)
if(model=="negbilog")   ml  = ml_negbilog(   param=param,dat=dat,mlmax=mlmax,fixed=fixed)
if(model=="mix")        ml  = ml_mix(        param=param,dat=dat,mlmax=mlmax,fixed=fixed)
if(model=="ct")         ml  = ml_ct(         param=param,dat=dat,mlmax=mlmax,fixed=fixed)
if(model=="taj")        ml  = ml_taj(        param=param,dat=dat,mlmax=mlmax,fixed=fixed)
ml
}
}

