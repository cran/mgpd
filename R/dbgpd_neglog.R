dbgpd_neglog <-
function(x,y,mar1=c(0,1,0.1),mar2=c(0,1,0.1),dep=2)
{
mu                = function(x,y) 1/x+1/y-(x^(alpha)+y^(alpha))^(-1/alpha)
dxdymu            = function(x,y) -(x^alpha+y^alpha)^(-1/alpha)*y^alpha*x^alpha/
(y*(x^alpha+y^alpha)^2*x)-(x^alpha+y^alpha)^(-1/alpha)*x^alpha*y^alpha*alpha/(x*(x^alpha+y^alpha)^2*y)

                  param         = as.numeric(c(mar1,mar2,dep))
                  mux           = param[1]; muy    = param[4]
                  sigx          = param[2]; sigy   = param[5]
                  gamx          = param[3]; gamy   = param[6]
                  alpha         = param[7]

hxy               = NULL
error             = FALSE 
if(sigx<0 | sigy<0 | alpha<0) error = TRUE
if(!error){
                  tx            = (1+gamx*(x-mux)/sigx)^(1/gamx)
                  ty            = (1+gamy*(y-muy)/sigy)^(1/gamy)
                  tx0           = (1+gamx*(-mux)/sigx)^(1/gamx)
                  ty0           = (1+gamy*(-muy)/sigy)^(1/gamy)
                  dtx           = (1/sigx)*pmax((1+gamx*(x-mux)/sigx),0)^(1/gamx-1)
                  dty           = (1/sigy)*pmax((1+gamy*(y-muy)/sigy),0)^(1/gamy-1)
                  c0            = -mu(tx0,ty0)
                  hxy           = 1/c0*dxdymu(tx,ty)*dtx*dty
                  hxy           = as.numeric(hxy*(1-((x<0)*(y<0))))
  }else stop("invalid parameter(s)")
hxy
}
