
# Tema C --- Stan Andrei - Vladut (E13)



#C1. a.

search = function(x,a)
{bucla=0;
 while(!bucla)
    {i = sample(1:length(x),1)
     if(x[i] == a)  
         {return(i);}}}


# Exemplu :
# x=c(12,8,51,34,2,36,98,2,45,10,0,37,71,2)
# a=2
# search(x,a)

# ---------------------------------------------------------------------------------------------------- 


#C3.a.


f = function(x)     {return (2*x+3)}                   #Polinom f
g = function(x)     {return(4+3*x)}                    #Polinom g
h = function(x)     {return(x^2+9*x+25)}               #Polinom h


polinom=function(n)
{p = round(runif(1,1,3*n),0)
 if(f(p) * g(p) == h(p))
    print("True")
 else  print("False")}


# Exemplu :
# polinom(2)
# Pentru exemplul dat (de polinoame), daca p==1 va da true


# ---------------------------------------------------------------------------------------------------- 


#C3. b.


eroare=function()
{ct=0; error = 0.25;
 while(error>=0.0001)
{ct=ct+1; error=error*0.25}
 print(ct)}


# Exemplu :
# eroare()

