bgpd_fit <-
function(initpar,dat,model="log",fixedmu=FALSE,control=list(maxit=50000),...)
{
est         = optim(initpar,bgpd_maxlik,dat=dat,model=model,control=control,fixed=fixedmu)
if(fixedmu==TRUE) est$par[1]=0
est
}

