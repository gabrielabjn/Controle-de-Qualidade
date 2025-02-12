
# Poder da carta de X_barra ----------------------------------------------------

sigma1 <- 3.61  # desvio padrao do processo fora de controle
lambda<- sigma1/S_d  # sigma1/sigma0

mu1 <- 6 # media do processo fora de controle 
delta<- (mu1-X_barbar)/S_d  # (mu1 - mu0)/sigma0

Pdx<-function(delta,n,lambda,k){
  return(sum(pnorm(-(c(-1,1)*delta*sqrt(n)+k)/lambda)))
}

Pdx_(k,delta,lambda,n) # probabilidade de ocorrer um alerta verdadeiro

# ( o processo esta, de fato, fora de controle)

# Poder da carta de R ----------------------------------------------------------

Pdr<-function(n,lambda){
  return(1-ptukey(q=5.25/lambda,nmeans=n,df=Inf))
}
Pdr(4,2)


# Poder total ------------------------------------------------------------------
Pd<-function(Pdx,Pdr){
 return(Pdx + Pdr - Pdx*Pdr)
}
Pd(Pdx(0.5,4,2,3.24), Pdr(4,2))

# X_barra - R : alpha = 0.0027 e Pd = 0.3589

# ARL : numero de amostras feitas apos o shift ate ter sinal de alerta
# ARL: 1/ poder do teste
# Fazer pra L0 e L1

# Probabilidade de os graficos de X e R nao terem emitido sinal ate a i-esima
#amostra apos o desajuste
Pd<-0.3589
dgeom(x=5-1,prob = 1-Pd) # para delta = 0.5 e lambda=2
# x: quantidade de INsucessos antes do primeiro sucesso
