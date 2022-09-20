library(stringi)

# A função "digitoCpf" calcula os digitos do CPF. O resultado é uma string(2).
# O arqumento "strCpf9" deve receber os 9 primeiros digitos
# do CPF em formato string.

digitoCpf <- function(strCpf9){
  numbers_only <- function(x) !grepl("\\D", x)
  if ((nchar(strCpf9) != 9) | (!numbers_only(strCpf9))){return("--")}
  calcDig <- function(strCpf){
    multi <- nchar(strCpf) + 1
    somaDigitos <- 0
    for  (digitoStr in strsplit(strCpf, "")[[1]]){
      digito <- as.integer(strtoi(digitoStr) * multi)
      somaDigitos <- somaDigitos + digito
      multi <- multi - 1
    }
    calcMod11 <- 11 - (somaDigitos %% 11)
    if (calcMod11 > 9){calcMod11 <- 0}
    digitoVerificador = toString(calcMod11)
    return(digitoVerificador)
  }
  digitoVerificador1 <- calcDig(strCpf9)
  digitoVerificador2 <- calcDig(paste(strCpf9,digitoVerificador1,sep=""))
  return(paste(digitoVerificador1,digitoVerificador2,sep=""))
}
