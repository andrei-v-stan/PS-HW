
#Homework part E.   --- Stan Andrei-Vladut E13

#E3

z_test_means = function(alfa, n1, n2, sample1_mean, sample2_mean, sigma1, sigma2, m0,hypothesis_type){
  combined_sigma = sqrt(sigma1^2/n1 + sigma2^2/n2)
  z_score = (sample1_mean - sample2_mean - m0)/combined_sigma
  
  if(hypothesis_type=='l'){
    critical_z = qnorm(alfa);
    if(z_score < critical_z){cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");}
    else{cat("Nu se poate respinge ipoteza nula \n");}}
  
  if(hypothesis_type=='r'){
    critical_z = qnorm(1 - alfa);
    if(z_score > critical_z){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");}
    else{cat("Nu se poate respinge ipoteza nula \n");}}
  
  if(hypothesis_type=='s'){
    critical_z = qnorm(1 - alfa/2);
    if(abs(z_score) > abs(critical_z)){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");}
    else{cat("Nu se poate respinge ipoteza nula \n");}}
  cat("Scorul:", z_score," Valoarea critica: ", critical_z, "\n");}


z_test_means(0.01,21,22,76.56,72.23,2.95,3.12,0,'s')
z_test_means(0.05,21,22,76.56,72.23,2.95,3.12,0,'s')


#E5

F_test = function(alfa, n1, n2, s1, s2, hypothesis_type){

  F_score = s1^2/s2^2
  
  if(hypothesis_type=='s'){
    critical_F_s= qf(alfa/2, n1 - 1, n2 - 1);
    critical_F_d= qf(1-alfa/2, n1 - 1, n2 - 1);
    if(F_score < critical_F_s || F_score > critical_F_d){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");}
    else{cat("Nu se poate respinge ipoteza nula \n");}
    cat("Scorul:", F_score," Valorile critice: ", critical_F_s, ",", critical_F_d, "\n");}
  
  if(hypothesis_type=='r'){
    critical_F = qf(1 - alfa, n1 - 1, n2 - 1);
    if(abs(F_score) > abs(critical_F)){
      cat("Se respinge ipoteza nula si se accepta ipoteza alternativa \n");}
    else{cat("Nu se poate respinge ipoteza nula \n");}
    cat("Scorul:", F_score," Valoarea critica: ", critical_F, "\n");}}

F_test(0.01,235,197,1.83,2.11,'r')