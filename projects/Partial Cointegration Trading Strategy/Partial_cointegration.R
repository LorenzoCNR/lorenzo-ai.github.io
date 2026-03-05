############################################################
# PARTIAL COINTEGRATION vs COINTEGRATION TRADING STRATEGY
# Research prototype for statistical arbitrage
############################################################

############################
# INSTALL PACKAGES
############################

# For partialCI dependencies (when available)
install.packages("glmnet")
install.packages("FKF")

# If partialCI / partialAR are not on your CRAN version, install from Archive:
install.packages(
  "https://cran.r-project.org/src/contrib/Archive/partialAR/partialAR_1.0.tar.gz",
  repos=NULL,
  type="source"
)

install.packages(
  "https://cran.r-project.org/src/contrib/Archive/partialCI/partialCI_1.1.0.tar.gz",
  repos=NULL,
  type="source"
)

# Optional (quantstrat stack - often painful on Windows; keep if you want)
install.packages("remotes")
remotes::install_github("braverock/blotter")

# These from R-Forge sometimes fail; keep but don't block the script
# install.packages("quantstrat", repos="http://R-Forge.R-project.org")
# install.packages("blotter", repos="http://R-Forge.R-project.org")
# install.packages("FinancialInstrument", repos="http://R-Forge.R-project.org")


install.packages("remotes")
remotes::install_github("braverock/blotter")
install.packages("partialCI", dependencies = TRUE)
install.packages("TTR")
install.packages("egcm",dependencies = TRUE)
install.packages("quantmod")
install.packages("fImport")
install.packages("PerformanceAnalytics")
install.packages("kernlab")
install.packages("TSA")
install.packages("tseries")
install.packages("fUnitRoots")
install.packages("zoo")
install.packages("xts")

############################
# LIBRARIES
############################
install.packages("quantstrat", repos="http://R-Forge.R-project.org")
install.packages("blotter", repos="http://R-Forge.R-project.org")
install.packages("FinancialInstrument", repos="http://R-Forge.R-project.org")



#library(Rmetrics)
library(fImport)
library(zoo)
library(quantmod)
library(partialCI)
library(TTR)
library(egcm)
library(PerformanceAnalytics)
library(kernlab)
library(TSA)
library(tseries)
library(fUnitRoots)
############################
# PARAMETERS
############################

Money <- 10000
transaction_cost <- 0.002
slippage <- 0.001
total_cost <- transaction_cost + slippage

startdate="2013-01-01"
#endate=Sys.Date()
endate="2018-12-31"

#### partial cointegration vs cointegration alla Englr e Granger (Rif CI1 nel paper)
### per confrontare le strategie devo (dovrei) avere serie in cui si osservi cointegrazione totale e parziale 
###nelle diverse sotto finestre

tickers <- c('MMM','ABT','ABBV','ACN','ATVI','AYI','ADBE','AMD','AAP','AES',
             'AMG','AFL','A','APD','AKAM','ALK','ALB','ARE','ALXN','ALGN','ALLE',
             'AGN','ADS','LNT','ALL','GOOGL','GOOG','MO','AMZN','AEE','AAL','AEP',
             'AXP','AIG','AMT','AWK','AMP','ABC','AME','AMGN','APH','ANSS','ANTM',
             'AON','AOS','APA','AIV','AAPL','AMAT','APTV','ADM','ARNC',
             'AJG','AIZ','T','ADSK','ADP','AZO','AVB','AVY','BLL','BAC','BK',
             'BAX','BBY','BIIB','BLK','HRB','BA','BWA','BXP','BSX',
             'BHF','BMY','AVGO','CHRW','CA','COG','CDNS','CPB','COF','CAH','CBOE',
             'CHTR','CHK','CVX','CMG','CB','CHD','CI','XEC','CINF','CTAS','CSCO','C','CFG',
             'CTXS','CLX','CME','CMS','KO','CTSH','CL','CMCSA','CMA','CAG','CXO','COP',
             'ED','STZ','COO','GLW','COST','COTY','CCI','CSRA','CSX','CMI','CVS','DHI',
             'DHR','DRI','DVA','DE','DAL','XRAY','DVN','DLR','DFS','DISCA','DISCK','DISH',
             'DG','DLTR','D','DOV','DTE','DRE','DUK','DXC','ETFC','EMN','ETN',
             'EBAY','ECL','EIX','EW','EA','EMR','ETR','EVHC','EOG','EQT','EFX','EQIX','EQR',
             'ESS','EL','ES','RE','EXC','EXPE','EXPD','ESRX','EXR','XOM','FFIV','FB','FAST',
             'ANSS','ANTM','AON','AOS','APA','AIV','AAPL','AMAT','APTV','ADM','ARNC',
             'KMX','CCL','CAT','CBS','CNC','CNP','CTL','CERN','CF','SCHW',
	           'FRT','FDX','FIS','FITB','FE','FISV','FLIR','FLS','FLR','FMC','FL','F','FTV',
	           'FBHS','BEN','FCX','GPS','GRMN','IT','GD','GE','GIS','GM','GPC','GILD',
	           'GPN','GS','GT','GWW','HAL','HBI','HOG','HIG','HAS','HCA','HP','HSIC',
	           'HSY','HES','HPE','HLT','HOLX','HD','HON','HRL','HST','HPQ','HUM','HBAN','HII',
	           'IDXX','INFO','ITW','ILMN','IR','INTC','ICE','IBM','INCY','IP','IPG','IFF','INTU',
	           'ISRG','IVZ','IQV','IRM','JEC','JBHT','SJM','JNJ','JCI','JPM','JNPR','KSU','K','KEY',
	           'KMB','KIM','KMI','KLAC','KSS','KHC','KR','LB','LH','LRCX','LEG','LEN',
	           'LLY','LNC','LKQ','LMT','L','LOW','LYB','MTB','MAC','M','MRO','MPC','MAR','MMC','MLM',
	           'MAS','MA','MAT','MKC','MCD','MCK','MDT','MRK','MET','MTD','MGM','MCHP','MU',
	           'MSFT','MAA','MHK','TAP','MDLZ','MNST','MCO','MS','MOS','MSI','MYL','NDAQ',
	           'NOV','NAVI','NTAP','NFLX','NWL','NEM','NWSA','NWS','NEE','NLSN','NKE','NI',
	           'NBL','JWN','NSC','NTRS','NOC','NCLH','NRG','NUE','NVDA','ORLY','OXY','OMC','OKE',
	           'ORCL','PCAR','PKG','PH','PDCO','PAYX','PYPL','PNR','PBCT','PEP','PKI','PRGO','PFE',
	           'PCG','PM','PSX','PNW','PXD','PNC','RL','PPG','PPL','PFG','PG','PGR',
	           'PLD','PRU','PEG','PSA','PHM','PVH','QRVO','PWR','QCOM','DGX','RRC','RJF','RTN','O',  
	           'REG','REGN','RF','RSG','RMD','RHI','ROK','ROP','ROST','RCL','CRM','SBAC',
	           'SLB','SNI','STX','SEE','SRE','SHW','SIG','SPG','SWKS','SLG','SNA','SO','LUV',
	            'SPGI','SWK','SBUX','STT','SRCL','SYK','STI','SYF','SNPS','SYY','TROW','TPR',
	           'TGT','TEL','FTI','TXN','TXT','TMO','TIF','TWX','TJX','TSCO','TDG','TRV',
	           'TRIP','TSN','UDR','ULTA','USB','UAA','UA','UNP','UAL','UNH','UPS','URI',
	           'UTX','UHS','UNM','VFC','VLO','VAR','VTR','VRSN','VRSK','VZ','VRTX','V','VNO',
	           'VMC','WMT','WBA','DIS','WM','WAT','WEC','WFC','WDC','WU','WRK','WY','WHR','WMB',
	           'WLTW','WYNN','XEL','XRX','XLNX','XL','XYL','YUM','ZBH','ZION','ZTS')

####BBT,BDX, viab....
#tick<-paste(unique(tickers))
#tick<-sapply(strsplit(tick, '[, ]+'), function(x) toString(dQuote(x, FALSE)))
#View(tick)
#getSymbols("ENI.MI",from=startdate,to=endate)
#getSymbols("FP.PA",from=startdate,to=endate)
#eni<-ENI.MI$ENI.MI.Close
#total<-FP.PA$FP.PA.Close
#eni_adj<-ENI.MI$ENI.MI.Adjusted
#total_adj<-FP.PA$FP.PA.Adjusted
#data_close<-cbind(ENI.MI$ENI.MI.Close, FP.PA$FP.PA.Close)
#summary (data_close)
#colnames(data_close)<-c("eni", "total")
### ricorda di rimuovere gli na o va in errore il prosieguo
#data_close_naM<-na.omit(data_close)
#summary(data_close_naM)


# Download tickers one by one to avoid script crashes
# If a ticker fails, it will simply be removed

tickers_loaded <- c()
tickers_failed <- c()

for (tk in tickers) {
  
  ok <- tryCatch({
    
    getSymbols(tk,
               from=startdate,
               to=endate,
               periodicity="daily",
               auto.assign=TRUE)
    
    TRUE
    
  }, error=function(e) {
    FALSE
  })
  
  if (ok) {
    tickers_loaded <- c(tickers_loaded, tk)
  } else {
    tickers_failed <- c(tickers_failed, tk)
  }
}

cat("Loaded:",length(tickers_loaded),"\n")
cat("Failed:",length(tickers_failed),"\n")

############################################################
# BUILD PRICE MATRIX
############################################################

# Extract adjusted prices for all loaded tickers

adj_list <- lapply(tickers_loaded,function(x){
  
  tryCatch({
    Ad(get(x))
  }, error=function(e){
    NULL
  })
  
})

names(adj_list) <- tickers_loaded
adj_list <- adj_list[!sapply(adj_list,is.null)]

adj_p <- do.call(merge,adj_list)

# Remove columns completely missing
adj_p <- adj_p[,colSums(is.na(adj_p))<nrow(adj_p)]

# Remove rows with missing values
adj_p <- na.omit(adj_p)

# Clean column names
cc <- colnames(adj_p)
cc <- gsub("\\.Adjusted","",cc)
colnames(adj_p) <- cc

############################
# DATA CLEANING
############################


R<-dim(adj_p)[1]
C<-dim(adj_p)[2]
# Remove part of names
#cc=colnames(adj_p)
gsub("\\.Adjusted","",cc) 
colnames(adj_p)<-gsub("\\.Adjusted","",cc)
cc=colnames(adj_p)
#### Standardizzo i prezzi
R<-dim(adj_p)[1]
C<-dim(adj_p)[2]
std_Prices<-matrix(nrow = R, ncol = C,0)
for(i in 1:C){
  std_Prices[,i] <- (adj_p[,i]-mean(adj_p[,i]))/diff(range(adj_p[,i]))
}

ddate<-index(adj_p)

std_p<-xts(std_Prices,order.by=ddate)
colnames(std_p)<-cc
colnames(std_Prices)<-cc

############################################################
# CORRELATION SCREENING
############################################################

# Compute correlation matrix between standardized prices

std_p_df <- as.data.frame(std_p)

ccorr <- cor(std_p_df)

# Remove (almost) duplicates (too highly correlated) and diagonal

ccorr[lower.tri(ccorr,diag=TRUE)] <- NA
ccorr[ccorr>=0.95] <- NA

cccorr <- as.data.frame(as.table(ccorr))
cccorr <- na.omit(cccorr)

# Keep only highly correlated pairs

sign=0.75
cccorr <- subset(cccorr,abs(Freq)>sign)

print(head(cccorr))


#turn into a 3-column table
cccorr <- as.data.frame(as.table(ccorr))
#remove the NA values from above 

plot(std_p$ABT)
lines( std_p$GOOGL)
##opero sui closing prices anche se quelli adj hanno piu' info
#### prova con adjusted li richiami sostituendo ad Close, Adjusted

############################################################
# SELECT BEST PAIR
############################################################

# Choose the pair with highest correlation

best_pair <- cccorr[which.max(abs(cccorr$Freq)),]

tk1 <- as.character(best_pair$Var1)
tk2 <- as.character(best_pair$Var2)

cat("Selected pair:",tk1,tk2,"\n")

############################################################
# BUILD DATASET FOR PAIR
############################################################

data_close <- cbind(adj_p[,tk1],adj_p[,tk2])

# Maintain original naming convention used in earlier experiments
colnames(data_close) <- c("eni","total")

data_close_naM <- na.omit(data_close)

############################################################
# ENGLE-GRANGER COINTEGRATION TEST
############################################################
HAS_egcm <- require(egcm)
HAS_partialCI <- require(partialCI)
if(HAS_egcm){
  
  egcm_ENI_TOTAL <- egcm(data_close_naM$eni,
                         data_close_naM$total,
                         include.const=FALSE)
  
  summary(egcm_ENI_TOTAL)
  
  plot(egcm_ENI_TOTAL$residuals,
       type="l",
       main="Cointegration Residuals")
  
}

### PCI Eni Total ####
############################################################
# PARTIAL COINTEGRATION TEST
############################################################

if(HAS_partialCI){
  
  pci_I_e_t <- fit.pci(data_close_naM$eni,
                       data_close_naM$total)
  
  summary(pci_I_e_t)
  
  test_I <- test.pci(data_close_naM$eni,
                     data_close_naM$total)
  
  print(test_I)
  
}

#### PERFECT!!!! sono parzialmente cointegrate  (rho ed R^2 hanno valori che ci piacciono)

states_I<-statehistory.pci(pci_I_e_t)
states_I
plot(statehistory.pci(pci_I_e_t)[,4],type = "l",ylab = "", xlab = "")

#### aggiungi bande di confidenza re rossa.  

#sectorS&P<- c("XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XLY")
#prices <- multigetYahooPrices(c("SPY", sectorETFS), start=20060101)


#### PARTE 2
#### Portfolio Strategy
#### Trading strategy and CI PCI comparison #############
###
###

############################################################
# FORMATION / TRADING SPLIT
############################################################

# This function creates formation and trading windows

form_trade_spl <- function(data,form=12,trade=1,period="months"){
  
  ep <- endpoints(data,on=period)
  
  form_end <- ep[form+1]
  
  formset <- data[1:form_end,]
  
  trade_start <- form_end+1
  
  trade_end <- ep[form+trade+1]
  
  tradeset <- data[trade_start:trade_end,]
  
  return(list(form=formset,trade=tradeset))
  
}

############################################################
# MAIN MODEL FUNCTION
# Compare Cointegration (CI) vs Partial Cointegration (PCI)
# Strategy based on threshold trading of the spread
############################################################

models_CI_PCI <- function(data, formsz=12, tradesz=1, period='months')
{
  
  ##########################################################
  # 1. SPLIT DATA INTO FORMATION AND TRADING WINDOWS
  ##########################################################
  
  ft <- form_trade_spl(data, formsz, tradesz, period)
  
  formset <- ft[["form"]]     # formation sample eni/total
  tradeset <- ft[["trade"]]   # trading sample
  
  tradeX <- tradeset[,-1]     # Total
  tradeY <- tradeset[,1]      # Eni
  ### Total + En
  trade_both <- cbind(tradeX, tradeY)
  
  Money = 10000
  #### X=eni Y=Total Y=alpha+Beta_coint*X
  CI <- egcm(formset[,1],formset[,2],include.const=TRUE) 
  T_F<-is.cointegrated(CI)
  
  ############################################################
  # 1) TESTS: Engle-Granger CI and Partial CI
  ############################################################
  
  CI <- egcm(formset[,1], formset[,2], include.const=TRUE)
  T_F <- is.cointegrated(CI)
  print(T_F)
  
  Test_PCI <- test.pci(formset[,1], formset[,2],
                       alpha = 0.05,
                       null_hyp = c("rw", "ar1"),
                       robust = FALSE,
                       pci_opt_method = c("jp"))
  
  cat("p.value =", Test_PCI[[3]][[1]], "\n")
  ############################################################
  # Helper: transaction cost (use existing global if present)
  ############################################################
  ### CHANGE: avoid hardcoding 0.02 everywhere; if total_cost exists, use it
  if (exists("total_cost")) {
    tc <- total_cost
  } else {
    tc <- 0.02
  }
  
  
  ##########################################################
  # CASE 1 : BOTH CI AND PCI DETECTED
  ##########################################################
  
  if(Test_PCI[[3]][[3]] < 0.1 & T_F == "TRUE")
  {
    
    ########################################################
    # FIT PARTIAL COINTEGRATION MODEL  
    # and build combined param list with CI
    ########################################################
    
    PCI <- fit.pci(
      formset[,1],
      formset[,2],
      pci_opt_method = c("jp"),
      par_model = c("par"),
      lambda = 0,
      robust = FALSE,
      nu = 5,
      include_alpha = FALSE
    )
    
    PCI_LIST <- append(Test_PCI, PCI)
    PCI_CI_LIST <- append(PCI_LIST, CI)
    
    
    ########################################################
    # EXTRACT PARAMETERS
    ########################################################
    # CI params 
    beta_CI <- PCI_CI_LIST[[43]]
    alpha_CI <- PCI_CI_LIST[[41]]
    sd_res_CI <- PCI_CI_LIST[[58]]
    # PCI params 
    beta_pci <- PCI_CI_LIST[[12]]
    rho_pci <- PCI_CI_LIST[[13]]
    sigma_M_pci <- PCI_CI_LIST[[14]]
    sigma_R_pci <- PCI_CI_LIST[[15]]
    
    CI_PCI_parameters <- cbind(
      beta_CI,
      alpha_CI,
      sd_res_CI,
      beta_pci,
      rho_pci,
      sigma_M_pci,
      sigma_R_pci,
      beta_pci
    )
    
    
    ########################################################
    # SPREAD Z-SCORE USING CI
    ########################################################
    
    ci_pci_thr_up = 1
    ci_pci_thr_down = c(-0.5)
    ci_pci_thr_out = 0
    
    z_stat <- (trade_both[,1] -
                 CI_PCI_parameters[2] -
                 CI_PCI_parameters[1] * trade_both[,2]) /
      CI_PCI_parameters[3]
    
    z_stat[1] <- 0.9
    
    ########################################################
    # ENTRY ABOVE UPPER THRESHOLD (CI)
    ########################################################
    
    ### CHANGE: initialize with 0 
    in_shorts_up_ci  <- matrix(0, nrow=nrow(z_stat), ncol=1)
    in_longs_up_ci   <- matrix(0, nrow=nrow(z_stat), ncol=1)
    out_shorts_up_ci <- matrix(0, nrow=nrow(z_stat), ncol=1)
    out_longs_up_ci  <- matrix(0, nrow=nrow(z_stat), ncol=1)
    
    # ENTRY UP (CI)
    for(i in 2:(nrow(z_stat)-1)) {
      before <- z_stat[i-1]
      start  <- z_stat[i]
      post   <- z_stat[i+1]
      if(before < ci_pci_thr_up && start > ci_pci_thr_up && post > ci_pci_thr_up) {
        in_shorts_up_ci[i+1] <- (1/2*Money)/trade_both[i+1,1]
        in_longs_up_ci[i+1]  <- (1/2*Money)/trade_both[i+1,2]
      }
    }
    
    ########################################################
    # EXIT ABOVE THRESHOLD
    ########################################################
    
    for(i in 2:(nrow(z_stat)-1))
    {
      
      before <- z_stat[i-1]
      start  <- z_stat[i]
      post   <- z_stat[i+1]
      
      if(before > ci_pci_thr_up &&
         start  > ci_pci_thr_up &&
         post   < ci_pci_thr_up)
      {
        
        out_shorts_up_ci[i+1] <- (0.5 * Money) / trade_both[i+1,1]
        out_longs_up_ci[i+1]  <- (0.5 * Money) / trade_both[i+1,2]
        
      }
      
    }
    
    #   # 
    ########################################################
    # BUILD TRADE MATRIX
    ########################################################
    
    # ENTRY UP (CI)
    for(i in 2:(nrow(z_stat)-1)) {
      before <- z_stat[i-1]
      start  <- z_stat[i]
      post   <- z_stat[i+1]
      if(before < ci_pci_thr_up && start > ci_pci_thr_up && post > ci_pci_thr_up) {
        in_shorts_up_ci[i+1] <- (1/2*Money)/trade_both[i+1,1]
        in_longs_up_ci[i+1]  <- (1/2*Money)/trade_both[i+1,2]
      }
    }
    
    pluto_up_Ci <- cbind(in_shorts_up_ci, in_longs_up_ci, trade_both)
    
    # EXIT UP (CI)
    for(i in 2:(nrow(z_stat)-1)) {
      before <- z_stat[i-1]
      start  <- z_stat[i]
      post   <- z_stat[i+1]
      if(before > ci_pci_thr_up && start > ci_pci_thr_up && post < ci_pci_thr_up) {
        out_shorts_up_ci[i+1] <- (1/2*Money)/trade_both[i+1,1]
        out_longs_up_ci[i+1]  <- (1/2*Money)/trade_both[i+1,2]
      }
    }
    
    pippo_up_ci <- cbind(pluto_up_Ci, out_shorts_up_ci, out_longs_up_ci)
    
    
    
    ########################################################
    # COMPUTE PAYOFF
    ########################################################
    
    ll <- length(unique(pippo_up_ci[,c(1,2,5,6)]))
    if (ll <= 3){
      print("no trade up Ci")
      total_payoff_up_ci <- "No Trade up Ci"
    } else {
      patrick_up_ci <- lapply(pippo_up_ci, function(x){x[x!=1]})
      df_in_1  <- as.data.frame(patrick_up_ci[[1]])
      df_in_2  <- as.data.frame(patrick_up_ci[[2]])
      df_out_1 <- as.data.frame(patrick_up_ci[[5]])
      df_out_2 <- as.data.frame(patrick_up_ci[[6]])
      N <- nrow(df_out_1)
      finally_up_ci <- cbind(df_in_1[1:N,], df_in_2[1:N,], df_out_1, df_out_2)
      
      payoff_short_up_ci <- log((finally_up_ci[,3] - tc*finally_up_ci[,3]) / finally_up_ci[,1])
      payoff_long_up_ci  <- -log((finally_up_ci[,4] + tc*finally_up_ci[,4]) / finally_up_ci[,2])
      total_payoff_up_ci <- cumsum(payoff_short_up_ci) + cumsum(payoff_long_up_ci)
    }
    
    
        #   # #### TRADE down of THRESHOLD ####
    in_shorts_down_ci=matrix(nrow=nrow(z_stat),1)
    in_longs_down_ci=matrix(nrow=nrow(z_stat),1)
    out_shorts_down_ci=matrix(nrow=nrow(z_stat),1)
    out_longs_down_ci=matrix(nrow=nrow(z_stat),1) 
    #   # 
    #   # ##ciclo con cui si entra nel mercato in caso sotto soglia
    trade_both
    for(i in 2:nrow(z_stat)-1) {
      before<-z_stat[i-1]
      start<-z_stat[i]
      post<-z_stat[i+1]
      if(before>ci_pci_thr_down&& start<ci_pci_thr_down && post<ci_pci_thr_down) {
        in_shorts_down_ci[i+1]<-(1/2*Money)/trade_both[i+1,2]
        in_longs_down_ci[i+1]<-(1/2*Money)/trade_both[i+1,1]
      }
      else {
        in_shorts_down_ci<-in_shorts_down_ci
        in_longs_down_ci<-in_longs_down_ci
      }
    }
    #   # 
    pluto_down_ci<- cbind(in_shorts_down_ci, in_longs_down_ci,trade_both)
    #   # 
    #   # # 
    #   # # ### Ciclo con cui si esce in caso sotto soglia
    in_shorts_down_ci  <- matrix(0, nrow=nrow(z_stat), ncol=1)
    in_longs_down_ci   <- matrix(0, nrow=nrow(z_stat), ncol=1)
    out_shorts_down_ci <- matrix(0, nrow=nrow(z_stat), ncol=1)
    out_longs_down_ci  <- matrix(0, nrow=nrow(z_stat), ncol=1)
    
    # ENTRY DOWN (CI)
    for(i in 2:(nrow(z_stat)-1)) {
      before <- z_stat[i-1]
      start  <- z_stat[i]
      post   <- z_stat[i+1]
      if(before > ci_pci_thr_down && start < ci_pci_thr_down && post < ci_pci_thr_down) {
        in_shorts_down_ci[i+1] <- (1/2*Money)/trade_both[i+1,2]
        in_longs_down_ci[i+1]  <- (1/2*Money)/trade_both[i+1,1]
      }
    }
    
    pluto_down_ci <- cbind(in_shorts_down_ci, in_longs_down_ci, trade_both)
    
    # EXIT DOWN (CI)
    for(i in 2:(nrow(z_stat)-1)) {
      before <- z_stat[i-1]
      start  <- z_stat[i]
      post   <- z_stat[i+1]
      if(before < ci_pci_thr_down && start < ci_pci_thr_down && post > ci_pci_thr_down) {
        out_shorts_down_ci[i+1] <- (1/2*Money)/trade_both[i+1,2]
        out_longs_down_ci[i+1]  <- (1/2*Money)/trade_both[i+1,1]
      }
    }
    
    pippo_down_ci <- cbind(pluto_down_ci, out_shorts_down_ci, out_longs_down_ci)
    
    ll <- length(unique(pippo_down_ci[,c(1,2,5,6)]))
    if (ll <= 3){
      print("no trade down Ci")
      total_payoff_down_ci <- "No Trade down Ci"
    } else {
      patrick_down_ci <- lapply(pippo_down_ci, function(x){x[x!=1]})
      df_in_1  <- as.data.frame(patrick_down_ci[[1]])
      df_in_2  <- as.data.frame(patrick_down_ci[[2]])
      df_out_1 <- as.data.frame(patrick_down_ci[[5]])
      df_out_2 <- as.data.frame(patrick_down_ci[[6]])
      N <- nrow(df_out_1)
      finally_down_ci <- cbind(df_in_1[1:N,], df_in_2[1:N,], df_out_1, df_out_2)
      
      payoff_short_down_ci <- log((finally_down_ci[,3] - tc*finally_down_ci[,3]) / finally_down_ci[,1])
      payoff_long_down_ci  <- -log((finally_down_ci[,4] + tc*finally_down_ci[,4]) / finally_down_ci[,2])
      total_payoff_down_ci <- cumsum(payoff_short_down_ci) + cumsum(payoff_long_down_ci)
    }
    
    cointegration_payoff <- list("payoff up"=total_payoff_up_ci,
                                 "payoff_down"=total_payoff_down_ci)
    
    
    # # # ##### caso di cointegrazione parziale se sono valide entrambe ####
    # # # ### calcolo kalman gain...
   
    #   # # 
    ############################################################
    #  PCI KALMAN FILTER → z_stat_pci
    # use Clegg 2015 Closed formula
    ############################################################
    
    rho_pci  <- CI_PCI_parameters[,5]
    sig_m    <- CI_PCI_parameters[,6]
    sig_r    <- CI_PCI_parameters[,7]
    beta_pci <- CI_PCI_parameters[,8]
    
    k_1 <- 2*sig_m^2 / ((sig_r*(sqrt(((rho_pci+1)^2)*sig_r^2 + 4*sig_m^2)) + rho_pci*sig_r + sig_r) + 2*sig_m^2)
    k_2 <- 2*sig_r   / ((sig_r*(sqrt(((rho_pci+1)^2)*sig_r^2 + 4*sig_m^2)) + rho_pci*sig_r + sig_r))
    k_vec <- rbind(k_1, k_2)
    k <- min(k_vec)
    
    # In-sample (formation) filter
    W_1_f <- formset[1,1] - beta_pci*formset[1,2]
    W_f <- matrix(W_1_f, nrow=nrow(formset[,1]), ncol=1)
    E_f <- matrix(0,    nrow=nrow(formset[,1]), ncol=1)
    M_f <- matrix(0,    nrow=nrow(formset[,1]), ncol=1)
    R_f <- matrix(W_1_f, nrow=nrow(formset[,1]), ncol=1)
    
    for (i in 2:length(formset[,1])) {
      W_f[i] <- formset[i,1] - beta_pci*formset[i,2]
      E_f[i] <- W_f[i] - rho_pci*M_f[i-1] - R_f[i-1]
      M_f[i] <- rho_pci*M_f[i-1] - k*E_f[i]
      R_f[i] <- R_f[i-1] + k*E_f[i]
    }
    
    sigma_f <- sd(M_f)
    
    # Out-of-sample (trading) filter
    W_1_t <- trade_both[1,2] - beta_pci*trade_both[1,1]
    W_t <- matrix(W_1_t, nrow=nrow(trade_both[,1]), ncol=1)
    E_t <- matrix(0,     nrow=nrow(trade_both[,1]), ncol=1)
    M_t <- matrix(0,     nrow=nrow(trade_both[,1]), ncol=1)
    R_t <- matrix(W_1_t, nrow=nrow(trade_both[,1]), ncol=1)
    
    for (i in 2:length(trade_both[,1])) {
      W_t[i] <- trade_both[i,2] - beta_pci*trade_both[i,1]
      E_t[i] <- W_t[i] - rho_pci*M_t[i-1] - R_t[i-1]
      M_t[i] <- rho_pci*M_t[i-1] - k*E_t[i]
      R_t[i] <- R_t[i-1] + k*E_t[i]
    }
    
    sigma_t <- sd(M_t)
    last_pci <- list(W_t, E_t, M_t, R_t, sigma_t)
    
    z_stat_pci <- last_pci[[3]] / sigma_f
    z_stat_pci[1] <- 0.9
    
    z_stat_series <- cbind(z_stat_pci, trade_both)
    

    # # #   # #      
    # #   # #      
    # #   # #      # # ### TRADE OVER THRESHOLD  in CASO DI PCI e sono presenti entrambe####
    ############################################################
    # 5A) PCI trading (UP + DOWN)
    ############################################################
    
    in_shorts_up_pci  <- matrix(0, nrow=nrow(z_stat_pci), ncol=1)
    in_longs_up_pci   <- matrix(0, nrow=nrow(z_stat_pci), ncol=1)
    out_shorts_up_pci <- matrix(0, nrow=nrow(z_stat_pci), ncol=1)
    out_longs_up_pci  <- matrix(0, nrow=nrow(z_stat_pci), ncol=1)
    
    # ENTRY UP (PCI)
    for(i in 2:(nrow(z_stat_pci)-1)) {
      before_pci <- z_stat_pci[i-1]
      start_pci  <- z_stat_pci[i]
      post_pci   <- z_stat_pci[i+1]
      if(before_pci < ci_pci_thr_up && start_pci > ci_pci_thr_up && post_pci > ci_pci_thr_up) {
        in_shorts_up_pci[i+1] <- (1/2*Money)/trade_both[i+1,1]
        in_longs_up_pci[i+1]  <- (1/2*Money)/trade_both[i+1,2]
      }
    }
    
    pluto_up_pci <- cbind(in_shorts_up_pci, in_longs_up_pci, trade_both)
    
    # EXIT UP (PCI)
    for(i in 2:(nrow(z_stat_pci)-1)) {
      before_pci <- z_stat_pci[i-1]
      start_pci  <- z_stat_pci[i]
      post_pci   <- z_stat_pci[i+1]
      if(before_pci > ci_pci_thr_up && start_pci > ci_pci_thr_up && post_pci < ci_pci_thr_up) {
        out_shorts_up_pci[i+1] <- (1/2*Money)/trade_both[i+1,1]
        out_longs_up_pci[i+1]  <- (1/2*Money)/trade_both[i+1,2]
      }
    }
    
    pippo_up_pci <- cbind(pluto_up_pci, out_shorts_up_pci, out_longs_up_pci)
    
    mm <- length(unique(pippo_up_pci[,c(1,2,5,6)]))
    if (mm <= 3){
      print("no trade up PCi")
      total_payoff_up_pci <- "No Trade up PCi"
    } else {
      patrick_up_pci <- lapply(pippo_up_pci, function(x){x[x!=1]})
      df_in_1  <- as.data.frame(patrick_up_pci[[1]])
      df_in_2  <- as.data.frame(patrick_up_pci[[2]])
      df_out_1 <- as.data.frame(patrick_up_pci[[5]])
      df_out_2 <- as.data.frame(patrick_up_pci[[6]])
      N <- nrow(df_out_1)
      finally_up_pci <- cbind(df_in_1[1:N,], df_in_2[1:N,], df_out_1, df_out_2)
      
      payoff_short_up_pci <- log((finally_up_pci[,3] - tc*finally_up_pci[,3]) / finally_up_pci[,1])
      payoff_long_up_pci  <- -log((finally_up_pci[,4] + tc*finally_up_pci[,4]) / finally_up_pci[,2])
      total_payoff_up_pci <- cumsum(payoff_short_up_pci) + cumsum(payoff_long_up_pci)
    }
    
    # DOWN THRESHOLD (PCI)
    in_shorts_down_pci  <- matrix(0, nrow=nrow(z_stat_pci), ncol=1)
    in_longs_down_pci   <- matrix(0, nrow=nrow(z_stat_pci), ncol=1)
    out_shorts_down_pci <- matrix(0, nrow=nrow(z_stat_pci), ncol=1)
    out_longs_down_pci  <- matrix(0, nrow=nrow(z_stat_pci), ncol=1)
    
    # ENTRY DOWN (PCI)
    for(i in 2:(nrow(z_stat_pci)-1)) {
      before_pci <- z_stat_pci[i-1]
      start_pci  <- z_stat_pci[i]
      post_pci   <- z_stat_pci[i+1]
      if(before_pci > ci_pci_thr_down && start_pci < ci_pci_thr_down && post_pci < ci_pci_thr_down) {
        in_shorts_down_pci[i+1] <- (1/2*Money)/trade_both[i+1,2]
        in_longs_down_pci[i+1]  <- (1/2*Money)/trade_both[i+1,1]
      }
    }
    
    pluto_down_pci <- cbind(in_shorts_down_pci, in_longs_down_pci, trade_both)
    
    # EXIT DOWN (PCI)
    for(i in 2:(nrow(z_stat_pci)-1)) {
      before_pci <- z_stat_pci[i-1]
      start_pci  <- z_stat_pci[i]
      post_pci   <- z_stat_pci[i+1]
      if(before_pci < ci_pci_thr_down && start_pci < ci_pci_thr_down && post_pci > ci_pci_thr_down) {
        out_shorts_down_pci[i+1] <- (1/2*Money)/trade_both[i+1,2]
        out_longs_down_pci[i+1]  <- (1/2*Money)/trade_both[i+1,1]
      }
    }
    
    pippo_down_pci <- cbind(pluto_down_pci, out_shorts_down_pci, out_longs_down_pci)
    
    mm <- length(unique(pippo_down_pci[,c(1,2,5,6)]))
    if (mm <= 3){
      print("no trade down PCi")
      total_payoff_down_pci <- "No Trade down PCi"
    } else {
      patrick_down_pci <- lapply(pippo_down_pci, function(x){x[x!=1]})
      df_in_1  <- as.data.frame(patrick_down_pci[[1]])
      df_in_2  <- as.data.frame(patrick_down_pci[[2]])
      df_out_1 <- as.data.frame(patrick_down_pci[[5]])
      df_out_2 <- as.data.frame(patrick_down_pci[[6]])
      N <- nrow(df_out_1)
      finally_down_pci <- cbind(df_in_1[1:N,], df_in_2[1:N,], df_out_1, df_out_2)
      
      payoff_short_down_pci <- log((finally_down_pci[,3] - tc*finally_down_pci[,3]) / finally_down_pci[,1])
      payoff_long_down_pci  <- -log((finally_down_pci[,4] + tc*finally_down_pci[,4]) / finally_down_pci[,2])
      total_payoff_down_pci <- cumsum(payoff_short_down_pci) + cumsum(payoff_long_down_pci)
    }
    
    partial_cointegration_only_payoff<-list("payoff up pci"=total_payoff_up_pci, "payoff_down pci"=total_payoff_down_pci, "z_stat_pci_only"=z_stat_series[,1],"pval"=Test_PCI[[3]][[1]])    # beta_CI<-PCI_LIST(42)
    # alpha_CI<-PCI_LIST(40)
    ############################################################
    #  Return combined results
    ############################################################
    
    results_pci_vs_ci <- list(
      "payoff up pci"  = total_payoff_up_pci,
      "payoff_down pci"= total_payoff_down_pci,
      "payoff up ci"   = total_payoff_up_ci,
      "payoff down ci" = total_payoff_down_ci,
      "Z Stat pci"     = z_stat_series[,1],
      "Z Stat ci"      = z_stat,
      "pval"           = Test_PCI[[3]][[1]]
    )
    
    ### CHANGE: explicit return (your original computed but did not reliably return)
    return(results_pci_vs_ci)
  }
  
  ############################################################
  # CASE B: PCI only (significant PCI, NOT CI)
  ####################################################
  ############################################################
  # CASE B: COINTEGRATION ONLY (CI TRUE, PCI NOT SIGNIFICANT)
  ############################################################
  
  else if (Test_PCI[[3]][[3]] >= 0.1 & T_F == "TRUE") {
    
    ############################################################
    # Prepare trading dataset
    ############################################################
    
    trade_both <- cbind(tradeX, tradeY)
    
    ci_thr_up   <- c(1)
    ci_thr_down <- c(-0.5)
    ci_thr_out  <- 0
    
    
    ############################################################
    # Extract CI parameters from egcm object
    ############################################################
    
    CI_LIST <- append(T_F, CI)
    
    beta_CI   <- CI_LIST[[14]]
    alpha_CI  <- CI_LIST[[12]]
    sd_res_CI <- CI_LIST[[29]]
    
    CI_parameters <- cbind(beta_CI, alpha_CI, sd_res_CI)
    
    
    ############################################################
    # Compute CI Z-score
    ############################################################
    
    z_stat <- (trade_both[,1] -
                 CI_parameters[2] -
                 CI_parameters[1] * trade_both[,2]) /
      CI_parameters[3]
    
    z_stat[1] <- 0.9
    
    
    ############################################################
    # TRADE ABOVE THRESHOLD
    ############################################################
    
    in_shorts_up_ci  <- matrix(nrow=nrow(z_stat_pci),1)
    in_longs_up_ci   <- matrix(nrow=nrow(z_stat_pci),1)
    
    out_shorts_up_ci <- matrix(nrow=nrow(z_stat_pci),1)
    out_longs_up_ci  <- matrix(nrow=nrow(z_stat_pci),1)
    
    
    ############################################################
    # Entry condition (spread above threshold)
    ############################################################
    
    for(i in 2:(nrow(z_stat)-1)) {
      
      before <- z_stat[i-1]
      start  <- z_stat[i]
      post   <- z_stat[i+1]
      
      if(before < ci_thr_up && start > ci_thr_up && post > ci_thr_up) {
        
        in_shorts_up_ci[i+1] <- (1/2 * Money) / trade_both[i+1,1]
        in_longs_up_ci[i+1]  <- (1/2 * Money) / trade_both[i+1,2]
        
      }
    }
    
    
    ############################################################
    # Build trade matrix
    ############################################################
    
    pluto_up_ci <- cbind(in_shorts_up_ci,
                         in_longs_up_ci,
                         trade_both)
    
    
    ############################################################
    # Exit condition
    ############################################################
    
    for(i in 2:(nrow(z_stat)-1)) {
      
      before <- z_stat[i-1]
      start  <- z_stat[i]
      post   <- z_stat[i+1]
      
      if(before > ci_thr_up && start > ci_thr_up && post < ci_thr_up) {
        
        out_shorts_up_ci[i+1] <- (1/2 * Money) / trade_both[i+1,1]
        out_longs_up_ci[i+1]  <- (1/2 * Money) / trade_both[i+1,2]
        
      }
    }
    
    
    pippo_up_ci <- cbind(pluto_up_ci,
                         out_shorts_up_ci,
                         out_longs_up_ci)
    
    
    ############################################################
    # Compute payoff (UP trades)
    ############################################################
    
    ll <- length(unique(pippo_up_ci[,c(1,2,5,6)]))
    
    if (ll <= 3) {
      
      print("no trade up Ci")
      total_payoff_down_ci <- "No Trade up Ci"
      
    } else {
      
      patrick_up_ci <- lapply(pippo_up_ci,
                              function(x){x[x!=1]})
      
      df_in_1  <- as.data.frame(patrick_up_ci[[1]])
      df_in_2  <- as.data.frame(patrick_up_ci[[2]])
      
      df_out_1 <- as.data.frame(patrick_up_ci[[5]])
      df_out_2 <- as.data.frame(patrick_up_ci[[6]])
      
      N <- nrow(df_out_1)
      
      finally_up_ci <- cbind(df_in_1[1:N,],
                             df_in_2[1:N,],
                             df_out_1,
                             df_out_2)
      
      payoff_short_up_ci <- log(
        (finally_up_ci[,3] - 0.02 * finally_up_ci[,3]) /
          finally_up_ci[,1]
      )
      
      payoff_long_up_ci <- -log(
        (finally_up_ci[,4] + 0.02 * finally_up_ci[,4]) /
          finally_up_ci[,2]
      )
      
      total_payoff_up_ci <- cumsum(payoff_short_up_ci) +
        cumsum(payoff_long_up_ci)
      
    }
    
    
    ############################################################
    # TRADE BELOW THRESHOLD
    ############################################################
    
    in_shorts_down_ci  <- matrix(nrow=nrow(z_stat),1)
    in_longs_down_ci   <- matrix(nrow=nrow(z_stat),1)
    
    out_shorts_down_ci <- matrix(nrow=nrow(z_stat),1)
    out_longs_down_ci  <- matrix(nrow=nrow(z_stat),1)
    
    
    ############################################################
    # Entry below threshold
    ############################################################
    
    for(i in 2:(nrow(z_stat)-1)) {
      
      before <- z_stat[i-1]
      start  <- z_stat[i]
      post   <- z_stat[i+1]
      
      if(before > ci_thr_down &&
         start  < ci_thr_down &&
         post   < ci_thr_down) {
        
        in_shorts_down_ci[i+1] <- (1/2 * Money) / trade_both[i+1,2]
        in_longs_down_ci[i+1]  <- (1/2 * Money) / trade_both[i+1,1]
        
      }
    }
    
    
    pluto_down_ci <- cbind(in_shorts_down_ci,
                           in_longs_down_ci,
                           trade_both)
    
    
    ############################################################
    # Exit below threshold
    ############################################################
    
    for(i in 2:(nrow(z_stat)-1)) {
      
      before <- z_stat[i-1]
      start  <- z_stat[i]
      post   <- z_stat[i+1]
      
      if(before < ci_thr_down &&
         start  < ci_thr_down &&
         post   > ci_thr_down) {
        
        out_shorts_down_ci[i+1] <- (1/2 * Money) / trade_both[i+1,2]
        out_longs_down_ci[i+1]  <- (1/2 * Money) / trade_both[i+1,1]
        
      }
    }
    
    
    pippo_down_ci <- cbind(pluto_down_ci,
                           out_shorts_down_ci,
                           out_longs_down_ci)
    
    
    ############################################################
    # Compute payoff (DOWN trades)
    ############################################################
    
    ll <- length(unique(pippo_down_ci[,c(1,2,5,6)]))
    
    if (ll <= 3) {
      
      print("no trade down Ci")
      total_payoff_down_ci <- "No Trade down Ci"
      
    } else {
      
      patrick_down_ci <- lapply(pippo_down_ci,
                                function(x){x[x!=1]})
      
      df_in_1  <- as.data.frame(patrick_down_ci[[1]])
      df_in_2  <- as.data.frame(patrick_down_ci[[2]])
      
      df_out_1 <- as.data.frame(patrick_down_ci[[5]])
      df_out_2 <- as.data.frame(patrick_down_ci[[6]])
      
      N <- nrow(df_out_1)
      
      finally_down_ci <- cbind(df_in_1[1:N,],
                               df_in_2[1:N,],
                               df_out_1,
                               df_out_2)
      
      payoff_short_down_ci <- log(
        (finally_down_ci[,3] - 0.02 * finally_down_ci[,3]) /
          finally_down_ci[,1]
      )
      
      payoff_long_down_ci <- -log(
        (finally_down_ci[,4] + 0.02 * finally_down_ci[,4]) /
          finally_down_ci[,2]
      )
      
      total_payoff_down_ci <- cumsum(payoff_short_down_ci) +
        cumsum(payoff_long_down_ci)
      
    }
    spread_ci <- trade_both[,1] -
      CI_parameters[2] -
      CI_parameters[1]*trade_both[,2]
    
    plot(spread_ci,
         type="l",
         main="Cointegration Spread")
    
    abline(h=mean(spread_ci), col="blue")
    
    ############################################################
    # Return results
    ############################################################
    
    cointegration_only_payoff <- list(
      
      "payoff up ci"   = total_payoff_up_ci,
      "payoff down ci" = total_payoff_down_ci,
      
      "z_stat_ci_only" = z_stat_series[,1],
      
      "pval" = Test_PCI[[3]][[1]]
      
    )
    
    Test_PCI[[3]][[1]]
    
  }

#rm(models_CI_PCI )

#rm(prova_fiale)
#unlist(prova_m, recursive=FALSE, use.names=TRUE)
prova_fix12<-models_CI_PCI(data_close_naM,formsz=48, tradesz=12,period="months")
#rm(list=ls())

results <- models_CI_PCI(data_close_naM)
############################################################
# PERFORMANCE ANALYSIS
############################################################

library(PerformanceAnalytics)

ret_pci_up   <- results[["payoff up pci"]]
ret_pci_down <- results[["payoff_down pci"]]

ret_ci_up    <- results[["payoff up ci"]]
ret_ci_down  <- results[["payoff down ci"]]

ret_pci <- na.omit(c(ret_pci_up, ret_pci_down))
ret_ci  <- na.omit(c(ret_ci_up,  ret_ci_down))

############################################################
# EQUITY CURVE
############################################################

equity_pci <- cumsum(ret_pci)
equity_ci  <- cumsum(ret_ci)

plot(equity_pci,
     type="l",
     col="blue",
     main="PCI Equity Curve",
     ylab="Cumulative Return")

plot(equity_ci,
     type="l",
     col="red",
     main="CI Equity Curve",
     ylab="Cumulative Return")


############################################################
# SHARPE RATIO
############################################################

SharpeRatio(ret_pci)
SharpeRatio(ret_ci)

############################################################
# MAX DRAWDOWN
############################################################

maxDrawdown(ret_pci)
maxDrawdown(ret_ci)

############################################################
# Z SCORE CI
############################################################

plot(results[["Z Stat ci"]],
     type="l",
     main="CI Z Score")

abline(h=c(-1,1), col="red")
abline(h=0, col="black")

############################################################
# Z SCORE PCI
############################################################

plot(results[["Z Stat pci"]],
     type="l",
     main="PCI Z Score")

abline(h=c(-1,1), col="blue")
abline(h=0, col="black")

############################################################
# CI vs PCI
############################################################

plot(equity_ci,
     type="l",
     col="red",
     lwd=2,
     main="CI vs PCI",
     ylab="Cumulative Return")

lines(equity_pci,
      col="blue",
      lwd=2)

legend("topleft",
       legend=c("CI","PCI"),
       col=c("red","blue"),
       lwd=2)

############################################################
# TRADE STATISTICS
############################################################

num_trades <- length(ret_pci)

win_rate <- sum(ret_pci > 0) / length(ret_pci)

avg_trade <- mean(ret_pci)

profit_factor <- sum(ret_pci[ret_pci>0]) /
  abs(sum(ret_pci[ret_pci<0]))

cat("Number of trades:",num_trades,"\n")
cat("Win rate:",win_rate,"\n")
cat("Average trade:",avg_trade,"\n")
cat("Profit factor:",profit_factor,"\n")


############################################################
# SPREAD VISUALIZATION
############################################################



############################################################
# RETURN DISTRIBUTION
############################################################

hist(ret_pci,
     breaks=30,
     main="Distribution of PCI strategy returns")


############################################################
# TRADING SIGNAL VISUALIZATION
############################################################

plot(results[["Z Stat pci"]],
     type="l",
     main="Trading signals")

abline(h=c(-1,1), col="red")
abline(h=0)

############################################################
# PAIR STABILITY ANALYSIS
############################################################
# A robustness check consists in verifying whether the
# cointegration relationship remains stable through time.
#
# Possible diagnostics include:
# - rolling ADF test
# - parameter stability tests
# - rolling hedge ratio estimation
############################################################


############################################################
# ROLLING BACKTEST (RESEARCH EXTENSION)
############################################################
# The current implementation uses a single formation and trading
# window. A natural extension is a rolling estimation procedure
# where the CI / PCI parameters are re-estimated through time.
#
# This avoids look-ahead bias and produces a realistic
# out-of-sample evaluation.

############################################################
# MULTI-PAIR PORTFOLIO EXTENSION
############################################################
# In production systems the strategy is typically deployed
# across multiple pairs simultaneously in order to diversify
# idiosyncratic spread risk.
############################################################


#####
####???
##### Formazione coppie
##### Normalizzazione prezzo
#### Mettici commodities ETF futures e opzioni
### Ranking corr
####etc etc per tirare fuori dalla lista e vedere  cosa abbiamo ottenuto....
###qualche grafico....quali vuoi? qui avevo fatto la statistica con le bande
# z_stat<-cbind(prova_m,1,-0.5)
###stat test....mmmhmhmhmhm...a 9-10 lags ? staz...Acf  e Pacf non sono il max
# ####mmhmhmhm....considera anche type
# acf(z_stat[,1])
# pacf(z_stat[,1])
# 
# adfTest(z_stat[,1],lags=10, type = c("c"), title = NULL,description = NULL)
# ###Critical Values
# #adfTable(trend = c("nc", "c", "ct"), statistic = c("t", "n"))
# 
#### Cosa manca....
### Rolling
### Valutazione performance con misure che non siano solo il rendimento secco
### Stop Loss
### Bid Ask...costi di transazione....
### what else??
### Grafici ma capiamo quali....oltre alle serie e alle due statistiche 
###l posso plottare serie e volume di trading
### bande di bolliger

