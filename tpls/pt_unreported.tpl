DATA_SECTION
  init_int nyrs;
  init_vector index_dat(1,nyrs);
  init_vector catch_dat(1,nyrs);
  init_number z;
  init_int startphz;
  init_int eof;

  !!if(eof != 999){cout<<"Data read incorrectly "<<eof<<endl; exit(0);} 

PARAMETER_SECTION
  init_bounded_number logK(5.0,8.5); //between max catch (150) and 5*K (5000)
  init_bounded_number logMSY(0,5.6); //between 1 and 5*MSY (250)
  init_bounded_number P0(0.01,1,startphz); 
  number MSY;
  number K;
  number q;
  number sigma;

  vector B_pred(1,nyrs);
  vector index_pred(1,nyrs);
  vector dev(1,nyrs);
  vector ratio(1,nyrs);

  objective_function_value objn;

PROCEDURE_SECTION

  K = mfexp(logK);
  MSY = mfexp(logMSY);

  dvariable fpen=0.0; // penalty variable 
  
  B_pred(1) = P0*K;
  ratio(1) = index_dat(1)/B_pred(1);
  int i;
  for(i=2;i<=nyrs;i++){
  	B_pred(i) = B_pred(i-1) + (pow(z,(z/(z-1)))/(z-1))*MSY*((B_pred(i-1)/K)-pow((B_pred(i-1)/K),z)) - catch_dat(i-1);
    // penalties: constrain B_pred greater than 0
    B_pred(i) = posfun(B_pred(i), 1, fpen);
    ratio(i) = index_dat(i)/B_pred(i);
  }
  q = exp(mean(log(ratio)));

  int j;
  for(j=1;j<=nyrs;j++){
    index_pred(j) = B_pred(j)*q;
    dev(j) = log(index_dat(j)) - log(index_pred(j));
  }
  sigma = sqrt(sum(square(dev))/(nyrs-1));

  objn = nyrs*log(sqrt(sigma*sigma)) + sum(square(dev)/(2*sigma*sigma));
  objn += 1000*fpen;

  cout<<objn<<endl;

REPORT_SECTION
  report << "K\n"<< K <<endl;
  report << "q\n" << q <<endl;
  report << "sigma\n" <<sigma<<endl;
  report << "Bmsy\n" <<0.4*K<<endl;
  report << "MSY\n" << MSY <<endl;
  report << "B_pred\n" <<B_pred<<endl;
  report << "NLL\n" <<objn<<endl;
  report << "P0\n" <<P0<<endl;
  
