# Probabilidade de fora de controle --------------------------------------------
PFE <- function(LIC, LSC, mu, k=0 ,sigma){

  1- diff(pnorm(q=c(LIC,LSC),mean=mu,sd=sigma))

}
# adaptar formula acima para considerar delta (deslocamentos da media do processo)

10^6*PFE(LIC=73.950, LSC=74.050,mu=74.001,sigma=0.0099)
# nao sei se a conta acima esta correta, a das meninas deu mil e pouco

# Controle do processo ---------------------------------------------------------
sigma<- 2
Cp<-(LSE-LIE)/(6*sigma)

Cp_r<-min((LSE-mu)/(3*sigma), (mu-LIE)/(3*sigma))

CP_m<-(LSE-LIE)/(6*sqrt(sigma^2+(d-mu)^2))
# procurar saber quem eh d




