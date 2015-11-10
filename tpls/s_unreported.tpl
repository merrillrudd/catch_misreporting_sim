DATA_SECTION
  init_int nyrs;
  init_vector index_dat(1,nyrs);
  init_vector catch_dat(1,nyrs);
  init_int eof;

  !!if(eof != 999){cout<<"Data read incorrectly "<<eof<<endl; exit(0);} 

PARAMETER_SECTION
  init_bounded_number logK(3.9,10.8);
  init_bounded_number logr(-6.9,-0.1);
  number K;
  number r;
  number q;
  number sigma;

  vector B_pred(1,nyrs);
  vector index_pred(1,nyrs);
  vector dev(1,nyrs);
  vector ratio(1,nyrs);

  objective_function_value objn;

PROCEDURE_SECTION

  K = mfexp(logK);
  r = mfexp(logr);

  dvariable fpen=0.0;

  B_pred(1) = K;
  ratio(1) = index_dat(1)/B_pred(1);
  int i;
  for(i=2;i<=nyrs;i++){
  	B_pred(i) = B_pred(i-1) + r*B_pred(i-1)*(1-B_pred(i-1)/K) - catch_dat(i-1);
    B_pred(i) = posfun(B_pred(i), 1, fpen);
    ratio(i) = index_dat(i)/B_pred(i);
  }
  q = exp(mean(log(ratio)));

  int j;
  for(j=1;j<=nyrs;j++){
    index_pred(j) = B_pred(j)*q;
    dev(j) = index_dat(j) - index_pred(j);
  }
  sigma = sqrt(sum(square(dev))/(nyrs-1));

  objn = nyrs*log(sigma) + sum(square(dev)/(2*square(sigma)));
  objn += 1000*fpen;

  cout<<objn<<endl;

REPORT_SECTION
  report << "K\n"<< K <<endl;
  report << "r\n" << r <<endl;
  report << "q\n" << q <<endl;
  report << "sigma\n" <<sigma<<endl;
  report << "Bmsy\n" <<K/2<<endl;
  report << "MSY\n" << r*K/4 <<endl;
  report << "B_pred\n" <<B_pred<<endl;
  report << "NLL\n" <<objn<<endl;
  
