#include <admodel.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <pt_unreported.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
  nyrs.allocate("nyrs");
  index_dat.allocate(1,nyrs,"index_dat");
  catch_dat.allocate(1,nyrs,"catch_dat");
  z.allocate("z");
  startphz.allocate("startphz");
  eof.allocate("eof");
if(eof != 999){cout<<"Data read incorrectly "<<eof<<endl; exit(0);} 
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  logK.allocate(5.0,8.5,"logK");
  logMSY.allocate(0,5.6,"logMSY");
  P0.allocate(0.01,1,startphz,"P0");
  MSY.allocate("MSY");
  #ifndef NO_AD_INITIALIZE
  MSY.initialize();
  #endif
  K.allocate("K");
  #ifndef NO_AD_INITIALIZE
  K.initialize();
  #endif
  q.allocate("q");
  #ifndef NO_AD_INITIALIZE
  q.initialize();
  #endif
  sigma.allocate("sigma");
  #ifndef NO_AD_INITIALIZE
  sigma.initialize();
  #endif
  B_pred.allocate(1,nyrs,"B_pred");
  #ifndef NO_AD_INITIALIZE
    B_pred.initialize();
  #endif
  index_pred.allocate(1,nyrs,"index_pred");
  #ifndef NO_AD_INITIALIZE
    index_pred.initialize();
  #endif
  dev.allocate(1,nyrs,"dev");
  #ifndef NO_AD_INITIALIZE
    dev.initialize();
  #endif
  ratio.allocate(1,nyrs,"ratio");
  #ifndef NO_AD_INITIALIZE
    ratio.initialize();
  #endif
  objn.allocate("objn");
}

void model_parameters::userfunction(void)
{
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
}

void model_parameters::report()
{
 adstring ad_tmp=initial_params::get_reportfile_name();
  ofstream report((char*)(adprogram_name + ad_tmp));
  if (!report)
  {
    cerr << "error trying to open report file"  << adprogram_name << ".rep";
    return;
  }
  report << "K\n"<< K <<endl;
  report << "q\n" << q <<endl;
  report << "sigma\n" <<sigma<<endl;
  report << "Bmsy\n" <<0.4*K<<endl;
  report << "MSY\n" << MSY <<endl;
  report << "B_pred\n" <<B_pred<<endl;
  report << "NLL\n" <<objn<<endl;
  report << "P0\n" <<P0<<endl;
}

void model_parameters::preliminary_calculations(void){
  admaster_slave_variable_interface(*this);
}

model_data::~model_data()
{}

model_parameters::~model_parameters()
{}

void model_parameters::final_calcs(void){}

void model_parameters::set_runtime(void){}

#ifdef _BORLANDC_
  extern unsigned _stklen=10000U;
#endif


#ifdef __ZTC__
  extern unsigned int _stack=10000U;
#endif

  long int arrmblsize=0;

int main(int argc,char * argv[])
{
    ad_set_new_handler();
  ad_exit=&ad_boundf;
    gradient_structure::set_NO_DERIVATIVES();
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
  #if defined(__GNUDOS__) || defined(DOS386) || defined(__DPMI32__)  || \
     defined(__MSVC32__)
      if (!arrmblsize) arrmblsize=150000;
  #else
      if (!arrmblsize) arrmblsize=25000;
  #endif
    model_parameters mp(arrmblsize,argc,argv);
    mp.iprint=10;
    mp.preliminary_calculations();
    mp.computations(argc,argv);
    return 0;
}

extern "C"  {
  void ad_boundf(int i)
  {
    /* so we can stop here */
    exit(i);
  }
}
