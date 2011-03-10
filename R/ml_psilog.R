ml_psilog <-
function(param,dat,mlmax=1e+15,fixed=FALSE,...)
{
loglik        = mlmax
hxy           = NA
x             = dat[,1]
y             = dat[,2]
error         = FALSE
mux           = param[1]; muy    = param[4]
sigx          = param[2]; sigy   = param[5]
gamx          = param[3]; gamy   = param[6]
alpha         = param[7]; asy    = param[8]
p             = 2
A             = function(x,alpha) (x^alpha+(1-x)^alpha)^(1/alpha)
d1A           = function(x,alpha) (x^alpha+(1-x)^alpha)^(1/alpha)*(x^alpha*alpha/x-(1-x)^alpha*alpha/(1-x))/(alpha*(x^alpha+(1-x)^alpha))
d2A           = function(x,alpha) (x^alpha+(1-x)^alpha)^(1/alpha)*(x^alpha*alpha/x-(1-x)^alpha*alpha/(1-x))^2/
(alpha^2*(x^alpha+(1-x)^alpha)^2)+(x^alpha+(1-x)^alpha)^(1/alpha)*(x^alpha*alpha^2/
x^2-x^alpha*alpha/x^2+(1-x)^alpha*alpha^2/(1-x)^2-(1-x)^alpha*alpha/(1-x)^2)/
(alpha*(x^alpha+(1-x)^alpha))-(x^alpha+(1-x)^alpha)^(1/alpha)*(x^alpha*alpha/x-
(1-x)^alpha*alpha/(1-x))^2/(alpha*(x^alpha+(1-x)^alpha)^2)

fi            = function(t,c,a) c*t^a*(1-t)^a+t
d1fi          = function(t,c,a) c*t^a*a*(1-t)^a/t-c*t^a*(1-t)^a*a/(1-t)+1
d2fi          = function(t,c,a) c*t^a*a^2*(1-t)^a/t^2-c*t^a*a*(1-t)^a/t^2-2*c*t^a*a^2*(1-t)^a/(t*(1-t))+c*t^a*(1-t)^a*a^2/(1-t)^2-c*t^a*(1-t)^a*a/(1-t)^2
Afi           = function(t,alpha,c,a) A(fi(t,c,a),alpha)
d1Afi         = function(t,alpha,c,a) d1A(fi(t,c,a),alpha)*d1fi(t,c,a)
d2Afi         = function(t,alpha,c,a) d2A(fi(t,c,a),alpha)*(d1fi(t,c,a))^2+d1A(fi(t,c,a),alpha)*d2fi(t,c,a)
mu            = function(x,y,alpha,c,a) (1/x+1/y)*Afi(x/(x+y),alpha,c,a)

xx            = seq(0,1,0.01)
if(min(d1Afi(xx,alpha,asy,p),na.rm=TRUE)<(-1) | max(d1Afi(xx,alpha,asy,p),na.rm=TRUE)>(+1)) error=TRUE
if(sigx<0 | sigy<0 | alpha<1.5 ) error=TRUE
if(fixed==TRUE) {mux=0}
if(error) loglik = mlmax
if(!error)
{
tx            = (1+gamx*(x-mux)/sigx)^(1/gamx)
ty            = (1+gamy*(y-muy)/sigy)^(1/gamy)
tx0           = (1+gamx*(-mux)/sigx)^(1/gamx)
ty0           = (1+gamy*(-muy)/sigy)^(1/gamy)
dtx           = (1/sigx)*pmax((1+gamx*(x-mux)/sigx),0)^(1/gamx-1)
dty           = (1/sigy)*pmax((1+gamy*(y-muy)/sigy),0)^(1/gamy-1)
c0            = -mu(tx0,ty0,alpha,asy,p)
mu1           = tx/(tx+ty)
dxmu1         = ty/(tx+ty)^2
dymu1         = (-tx)/(tx+ty)^2
dxdymu1       = (tx-ty)/(tx+ty)^3
dxdymu        = (-1)*d1Afi(mu1,alpha,asy,p)*(dymu1/tx^2+dxmu1/ty^2)+(1/tx+1/ty)*(d2Afi(mu1,alpha,asy,p)*dxmu1*dymu1+d1Afi(mu1,alpha,asy,p)*dxdymu1)
hxy           = 1/c0*dxdymu*dtx*dty
hxy           = as.numeric(hxy*(1-((x<0)*(y<0))))
loglik        = -sum(log(hxy))
}
if(min(1+gamx*(x-mux)/sigx)<0) loglik=mlmax
if(min(1+gamy*(y-muy)/sigy)<0) loglik=mlmax
loglik
}

