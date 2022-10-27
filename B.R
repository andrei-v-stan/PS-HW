
#Tema B --- Stan Andrei Vladut (E13)



# B1.

con_vol=function(a)
  {N_C=0;  h=3;  r=2
  for(i in 1:a)
    {x = runif(1, -2, 2)
     y = runif(1, -2, 2)
     z = runif(1, 0, 3)
    if(x^2+y^2 <= (r^2/h^2)*z^2)
      {N_C=N_C + 1}}
  return(8*r*h*N_C/a)}

h=3; r=2


est_vol1=con_vol(10000)
abs_error1=abs(est_vol1-(pi*r^2*h)/3)
rel_error1=abs_error1/(pi*r^2*h)/3

print(c('aria estimata este',est_vol1), quote = FALSE)
print(c('eroarea absoluta este',abs_error1), quote = FALSE)
print(c('eroarea relativa este',rel_error1), quote = FALSE)



est_vol2=con_vol(20000)
abs_error2=abs(est_vol2-(pi*r^2*h)/3)
rel_error2=abs_error2/(pi*r^2*h)/3

print(c('aria estimata este',est_vol2), quote = FALSE)
print(c('eroarea absoluta este',abs_error2), quote = FALSE)
print(c('eroarea relativa este',rel_error2), quote = FALSE)



est_vol3=con_vol(50000)
abs_error3=abs(est_vol3-(pi*r^2*h)/3)
rel_error3=abs_error3/(pi*r^2*h)/3

print(c('aria estimata este',est_vol3), quote = FALSE)
print(c('eroarea absoluta este',abs_error3), quote = FALSE)
print(c('eroarea relativa este',rel_error3), quote = FALSE)


# ---------------------------------------------------------------------------------------------------- 


# B2.



area=function(N)
{N_C=0
for(i in 1:N)
  {x = runif(1,0,2)
   y = runif(1,0,2)
   if(y>=0 && 2*y<=x && x+y<=3)
     {N_C=N_C+1}}
return(2*N_C/N)}

area(15000)


# ---------------------------------------------------------------------------------------------------- 


# B3. a.

MC1 = function(a)
  {s = 0;
  for(i in 1:a)
    {u = runif(1, 0, 1);
     s = s + 2*sqrt(u)/(u+1);}
  return(1*s/a)}

est_int=MC1(100000)
abs_error=abs(est_int-4-pi)
rel_error=abs_error/(4-pi)

print(c('integrala estimata este',est_int), quote = FALSE)
print(c('eroarea absoluta este',abs_error), quote = FALSE)
print(c('eroarea relativa este',rel_error), quote = FALSE)




# B3. b.

MC2 = function(a)
  {s = 0;
  inf=10000
  for(i in 1:a)
    {u = runif(1, 0, inf);
    s = s + (1+2*u)*exp(-u);}
  return(inf*s/a)}

est_int=MC2(100000)
abs_error=abs(est_int-3)
rel_error=abs_error/(3)

print(c('integrala estimata este',est_int), quote = FALSE)
print(c('eroarea absoluta este',abs_error), quote = FALSE)
print(c('eroarea relativa este',rel_error), quote = FALSE)


# ---------------------------------------------------------------------------------------------------- 


# B4.

MC_retea=function(a)
{  s=0;
  for(i in 1:a)
  { x=rexp(1,1);
    g1=rgamma(1,shape=6,scale=4)
    g2=rgamma(1,shape=6,scale=2)
    g3=rgamma(1,shape=5,scale=3)
    u=runif(1,0,2);
    if(u<=0.35)        {s=s+g1}
    else if(u<=0.4)    {s=s+g2}
    else if(u<=0.25)   {s=s+g3}
    else               {s=s+x}}
  return (s/a);}
