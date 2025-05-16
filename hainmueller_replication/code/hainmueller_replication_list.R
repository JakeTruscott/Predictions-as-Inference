
  rep_adams_2006 = list(Y="pshift2", # change in ideological shift
                        D="idparty", # niche party
                        X="vshift", # public opinion shift
                        Z=c("pshiftt12","votec1","pvoteshift","Italy","Britain","Greece","Luxembourg","Denmark","Netherlands","Spain"))


  rep_aklin_2013 = list(Y="drenew_capacity_nh_share",
                        D="oilcrude_price2007dollar_bp",
                        X="lrenewpc",
                        Z=c("left_to_right","right_to_left","left_executive",
                            "right_executive","election_year",
                            "renewablecapacity_3yr_average","hydronuclear_3yr","year",
                            "traditional_electricity_share"))

  rep_banks_2012 = list(Y="racpolicy",
                        D="anger",
                        X="racresent1",
                        Z=c("fearres1","fear" ,"disgust", "disgusres1", "ideology" ,"education" ,"income1", "age", "south", "openissuejo1" ))

  rep_bodea_io_2015 = list(Y="logdm2",
                           D="lvaw",
                           X="polity2_cen",
                           Z=c("L_logdm2","L_lngdp","L_dgdp_k","L_openness","xrdum",
                               "L_fiscal_balance","pres_only","leg_only","pres_leg"))

  rep_bodea_2015 = list(Y="fdiinflow_res",
                        D="llvaw",
                        X="polity2_cen",
                        Z=c("l_dffrus","l_dfxreserves","l_openness","l_dgdp_k",
                            "l_lngdppc","l_fiscal_balance","l_capital_controls","l_lninfl",
                            "l_signyearfill","xrdum","year"))

  rep_carpenter_2014 = list(Y="totnamesper1000",
                            D="pctpetwomen",
                            X="pctfocgag",
                            Z=c("pctfocdc", "pctpetsep", "pctfocterr", "pctfocnewst" ,"pctfocslavetrade"))

  rep_chapman_2009 = list(Y="rally",
                          D="unauth",
                          X="S",
                          Z=c("priorpop","bipart","statemnt","nextelec","nytcov","buscon","revorg","wardumk","majopp","allies","war","SCappeal","regorgact","ordsever","hostlvl"))

  rep_clark_2006 = list(Y="enep1",
                        D="eneg",
                        X="logmag",
                        Z=c("uppertier" ,"enpres" ,"proximity1", "uppertier_eneg" ,"proximity1_enpres" ))

  rep_clark_2014 = list(Y="votechgtt1",
                        D="cgscn2tt1",
                        X="partydispuw",
                        Z=c("votechgt1t2","ingovt_mc"))

  rep_hellwig_2007 = list(Y="incvotet",
                          D="dgdp",
                          X="tradeshr",
                          Z=c("incvotet1", "electype" ,"gdpxelectype" ,"presrun", "enlp" ,"income" ,"regafrica", "regasia" ,"regcee" ,"reglatam"))



  rep_hicken_2008 = list(Y="log_illit_all",
                         D="educgdp",
                         X="parindex",
                         Z=c("log_capgdpppp","poprural","lamerica","psoviet","sasia2","easia2","mideast"))



  rep_malesky_2012 = list(Y="d_question_count",
                          D="t2",
                          X="internet_users100",
                          Z=c("centralnominated","fulltime","retirement","city","ln_gdpcap","ln_pop","transfer","south","unweighted"))

  rep_neblo_2010 = list(Y="willing",
                        D="treatcong2",
                        X="stealth2_ct",
                        Z=c( "needjud_ct", "educ_ct", "income_ct", "interest_ct" ,"chur_ct",
                             "gentrust_ct", "intxcon", "treathour2", "treatint" ,"treattop" ,
                             "treatinc" , "gender2" ,"age_ct",  "conflict_ct" ,"needcog_ct" ,
                             "sunshine_ct", "efficacy_ct" ,"pidcoll_ct" ,"empfull", "white" ))

  rep_pelc_2011 = list(Y="depth3",
                       D="polity3",
                       X="logfullimports",
                       Z=c("logGDPconst", "logfullexports" , "accessionperiod", "loggdpcap" ,
                           "MFNpre", "chinadum" , "membershiptiming","polityXexp"))

  rep_peterson_2013 = list(Y="tougher",
                           D="lazy",
                           X="imagine",
                           Z=NULL)

  rep_somer_2009 = list(Y="absch1",
                        D="votech2",
                        X="monthstoprevelect",
                        Z=c("absch2"))

  rep_tavits_2008 = list(Y="voteslost",
                         D="neighbor",
                         X="importance",
                         Z=c("yearsdem" ,"turn_ch" ,"govt", "unemp", "gdp" ,"ln_mdm", "ln_infl", "votes_last" ,"c1" ,"c2", "c3" ,"c4", "c5" ,"c6" ,"c7" ,"c8", "c9"))

  rep_truex_2014 = list(Y="roa",
                        D="npc",
                        X="so_portion",
                        Z=NULL)


  rep_william_2011 = list(Y="change",
                          D="opp_conf_party_elecdate",
                          X="eff_par",
                          Z=c("opp_conf_elecdate","majority", "gparties", "lag_pervote" ,"rgdppc_growth" ,"eoc"))




  hainmueller_list <- list()

  hainmueller_list$rep_adams_2006 <- rep_adams_2006
  hainmueller_list$rep_aklin_2013 <- rep_aklin_2013
  hainmueller_list$rep_banks_2012 <- rep_banks_2012
  hainmueller_list$rep_bodea_io_2015 <- rep_bodea_io_2015
  hainmueller_list$rep_bodea_2015 <- rep_bodea_2015
  hainmueller_list$rep_carpenter_2014 <- rep_carpenter_2014
  hainmueller_list$rep_chapman_2009 <- rep_chapman_2009
  hainmueller_list$rep_clark_2006 <- rep_clark_2006
  hainmueller_list$rep_clark_2014 <- rep_clark_2014
  hainmueller_list$rep_hellwig_2007 <- rep_hellwig_2007
  hainmueller_list$rep_hicken_2008 <- rep_hicken_2008
  hainmueller_list$rep_malesky_2012 <- rep_malesky_2012
  hainmueller_list$rep_neblo_2010 <- rep_neblo_2010
  hainmueller_list$rep_pelc_2011 <- rep_pelc_2011
  hainmueller_list$rep_peterson_2013 <- rep_peterson_2013
  hainmueller_list$rep_somer_2009 <- rep_somer_2009
  hainmueller_list$rep_tavits_2008 <- rep_tavits_2008
  hainmueller_list$rep_truex_2014 <- rep_truex_2014
  hainmueller_list$rep_william_2011 <- rep_william_2011


  save(hainmueller_list, file = 'hainmueller_replication/data/hainmueller_list.rdata')
