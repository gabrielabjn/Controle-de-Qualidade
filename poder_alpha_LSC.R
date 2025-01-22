

calcula_poder <-function(k,sigma,n){
  
  return(pnorm(-k-sigma*sqrt(n))+pnorm(-k+sigma*sqrt(n)))
  
  
}

calcula_poder(3,1,4)


calcula_LSC <- function(n) return(IQCC::d2(n)+IQCC::d3(n)*3)
calcula_LSC(2)


calcula_alpha <- function(LSC,n) return(1-ptukey(LSC,n,Inf))
calcula_alpha(calcula_LSC(2),2)

# ARL_0
1/calcula_alpha(calcula_LSC(3),2)

# forcando alpha a ser 0.0027 (calculo do LSC)
qtukey(1-0.0027,5,Inf)







