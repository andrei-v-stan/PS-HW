
# Tema D --- Stan Andrei - Vladut (E13)



# z = critical value;   a = alpha;   s = sigma;  samp = sample mean;   zs = z score
# Am inlocuit direct a si b din c(a,b)
# Am inlocuit si  direct cu s/sqrt(n);


# D1

interval1=function(n,samp,a,s)
{z=qnorm(1-a/2,0,1);
 intr=c(samp-z*s/sqrt(n),samp+z*s/sqrt(n)); print(intr)}

# Exemplu:
# interval1(100,20,0.1,3)


# ----------------------------------------------------------------------------------------------------


#D2

interval2=function(n,samp,a,s)
{z=qt(1-a/2,n-1);
intr=c(samp-z*s/sqrt(n),samp+z*s/sqrt(n)); print(intr)}

# Exemplu:
# interval2(60,3.3,0.05,0.4)


# ----------------------------------------------------------------------------------------------------


#D3

test1=function(n, p0, suc, a, hyp)
{z=0; prim = suc/n;  zs = (prim - p0)/(sqrt(p0*(1 - p0)/n));


  if(hyp=='l')
  {z = qnorm(a);
   if(zs < z){cat("Se respinge H0 se accepta Ha \n");}
   else{cat("Nu se poate respinge Ha \n");}}
  
  if(hyp=='r')
    {z = qnorm(1 - a);
     if(zs > z){cat("H0 este respinsa si se accepta Ha \n");}
     else{cat("Nu se poate respinge Ha \n");}}  

  if(hyp=='s')
    {z = qnorm(1 - a/2);
    if(abs(zs) > abs(z)){cat("Se respinge H0 si se accepta Ha \n");}
    else{cat("Nu se poate respinge Ha \n");}}
    
  cat("Scorul:", zs," Valoarea critica: ", z, "\n");}

