vol_sfera = function(N){# ex I.1
  N_C = 0;
  for(i in 1:N){
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    z = runif(1, -1, 1);
    if(x^2 + y^2 + z^2 <=1)
      N_C = N_C + 1;
  }
  volEstimat = 8*N_C/N;
  volExact = 4*pi/3;
  cat("Volumul estimat:", volEstimat," Volumul exact: ", volExact, "\n");
  cat("Eroarea absoluta:", abs(volExact- volEstimat), "\n");
  cat("Eroarea relativa:", abs(volExact- volEstimat)/volExact);
}

arie_peste_parabola = function(N){# ex I.2
  N_B = 0;
  for(i in 1:N){
    x = runif(1, 0, 2);
    y = runif(1, 0, 2);
    if(-2*x^2 + 5*x -2 >= y & x>=0.5)
      N_B = N_B + 1;
  }
  arieEstimata = 4*N_B/N;
  #ariaExacta = 27/24;#?
  cat("Aria estimata:", arieEstimata, "\n");
  #cat("Eroarea absoluta:", abs(ariaExacta - arieEstimata), "\n");
  #cat("Eroarea relativa:", abs(ariaExacta- arieEstimata)/ariaExacta);
}