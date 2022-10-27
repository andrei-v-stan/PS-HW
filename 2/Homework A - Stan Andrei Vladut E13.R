

#Homework part A.   --- Stan Andrei-Vladut E13

# A1

P = function(k,l,lam)         # P poisson function
{barplot(dpois(k:(l),lam))}   # lam = ??

G= function(k,l,p)         # G geometric function
{barplot(dgeom(k:(l),p))}

# Example :
# P(3,5,2)
# G(5,10,0.3)


#------------------------------------------------------

# A2  

# a.)

F=function(x)           # F function
{print(mean(x))       # Prints the arithmetic mean
  print(median(x))      # Prints the median
  print(sd(x))          # Prints the standard deviation
  
  print(as.vector(quantile(x))[2])    #Prints the quantile 1
  print(as.vector(quantile(x))[4])}   #Prints the quantile 3

# Example :
# x=c(10,20,30)
# F(x)


#----------------

# b.)

ol = function(x)    # Outliers function
{a = vector()

am = mean(x)
sd = sd(x)

ct = 1
left = am - 2 * sd    # Left interval limit (closed); left-1 (open)
right = am + 2 * sd   # Right interval limit (closed); right-1 (open)

i=1
while(i<length(x))
{if(x[i]<=right && x[i]>=left)
{a[ct]=x[i]
ct=ct+1}
  i=i+1}

#print(a)
return (a)}            #Necessary for c.)

# Example :
# x=c(10,11,12,13,14,15,16,17,18,19,20,100,101)
# ol(x)


#----------------

# c.)

gr = function(x)    # graphic representation function
{a = ol(x)
interval=c(40,45,50,55,60,65,70,75,80,85,90,95)
hist(a, breaks=interval,right=T,freq=T)}

# Example :
# x = c(79,71, 89, 57, 76, 64, 82, 82, 67, 80, 81, 65, 73, 79, 79, 60, 58, 83, 74, 68, 78, 80, 78, 81, 76, 65, 70, 76, 58, 82, 59, 73, 72, 79, 87, 63, 74, 90, 69, 70, 83, 76, 61, 66, 71, 60, 57, 81, 57, 65, 81, 78, 77, 81, 81, 63, 71, 66, 56, 62, 75, 64, 74, 74, 70, 71, 56, 69, 63, 72, 81, 54, 72, 91, 92)
# gr(x)



