
  Adams_et_al_AJPS_2006 = list()
  Adams_et_al_AJPS_2006[['rep_adams_2006']] = list(Y="pshift2", # change in ideological shift
                        D="idparty", # niche party
                        X="vshift", # public opinion shift
                        Z=c("pshiftt12","votec1","pvoteshift","Italy","Britain","Greece","Luxembourg","Denmark","Netherlands","Spain"),
                        data = 'rep_adams_2006.dta')

  ##############################################################################


  Aklin_AJPS_2013 = list()
  Aklin_AJPS_2013[['rep_aklin_2013a']] = list(Y="drenew_capacity_nh_share",
                        D="oilcrude_price2007dollar_bp",
                        X="lrenewpc",
                        Z=c("left_to_right","right_to_left","left_executive",
                            "right_executive","election_year",
                            "renewablecapacity_3yr_average","hydronuclear_3yr","year",
                            "traditional_electricity_share"),
                        data = 'rep_aklin_2013.dta')

  Aklin_AJPS_2013[['rep_aklin_2013b']] = list(Y="drenew_capacity_nh_share",
                         X="oilcrude_price2007dollar_bp",
                         D="lrenewpc",
                         Z=c("left_to_right","right_to_left","left_executive","right_executive","election_year","renewablecapacity_3yr_average","hydronuclear_3yr","year","traditional_electricity_share"),
                         data = 'rep_aklin_2013.dta') # Switches X and D from Aklin A

  ##############################################################################


  Banks_Valentino_AJPS_2012 = list()
  Banks_Valentino_AJPS_2012[['rep_banks_2012a']] = list(Y="racpolicy",
                        D="anger",
                        X="racresent1",
                        Z=c("fearres1","fear" ,"disgust", "disgusres1", "ideology" ,"education" ,"income1", "age", "south", "openissuejo1" ),
                        data = 'rep_banks_2012a.dta')

  Banks_Valentino_AJPS_2012[['rep_banks_2012b']] = list(Y="racpolicy",
                         D="anger",
                         X="jimcrow13",
                         Z=c("disgusjc13" ,"disgust" ,"fear" ,"fearjc13" ,"ideology", "education" ,"income1" ,"age" ,"south", "openissuejo1"),
                         data = 'rep_banks_2012b.dta')


  ##############################################################################


  Bodea_IO_2015 = list()
  Bodea_IO_2015[['bodea_io_2015a']] = list(Y="logdm2",
                           D="lvaw",
                           X="polity2_cen",
                           Z=c("L_logdm2","L_lngdp","L_dgdp_k","L_openness","xrdum",
                               "L_fiscal_balance","pres_only","leg_only","pres_leg"),
                           FE = c('decade', 'cowcode'),
                           data = 'rep_bodea_io_2015a.dta')

  Bodea_IO_2015[['bodea_io_2015b']] = list(Y="logdm2",
                                               D="lvaw",
                                               X="FH_trans",
                                               Z=c("L_logdm2","L_lngdp","L_dgdp_k","L_openness","xrdum","L_fiscal_balance","pres_only","leg_only","pres_leg"),
                                           FE = c("region","decade","cowcode"),
                                               data = 'rep_bodea_io_2015b.dta')

  Bodea_IO_2015[['bodea_io_2015c']] = list(Y="lninfl",
                                               D="lvaw",
                                               X="polity2_cen",
                                               Z=c("L_lninfl","L_logdm2","L_lngdp","L_dgdp_k","L_openness","xrdum","L_fiscal_balance","pres_only","leg_only","pres_leg","L_wdgdpdefl"),
                                               FE = c('region', 'cowcode'),
                                               data = 'rep_bodea_io_2015c.dta')

  Bodea_IO_2015[['bodea_io_2015d']] = list(Y="lninfl",
                                               D="lvaw",
                                               X="FH_trans",
                                               Z=c("L_lninfl","L_logdm2","L_lngdp","L_dgdp_k","L_openness","xrdum","L_fiscal_balance","pres_only","leg_only","pres_leg","L_wdgdpdefl"),
                                           FE = c('region', 'cowcode'),
                                               data = 'rep_bodea_io_2015d.dta')


  ##############################################################################


  Bodea_JOP_2015 = list()
  Bodea_JOP_2015[['rep_bodea_2015a']] = list(Y="fdiinflow_res",
                                             D="llvaw",
                                             X="polity2_cen",
                                             Z=c("l_dffrus","l_dfxreserves","l_openness","l_dgdp_k",
                                                 "l_lngdppc","l_fiscal_balance","l_capital_controls","l_lninfl",
                                                 "l_signyearfill","xrdum","year"),
                                             FE = c('cowcode'),
                                             data = 'rep_bodea_2015a.dta')

  Bodea_JOP_2015[['rep_bodea_2015b']] = list(Y="real_10yrate_res",
                                             D="llvaw",
                                             X="polity2_cen",
                                             Z=c("l_dffrus","l_dfxreserves","l_openness","l_wdgdpdefl","l_dgdp_k","l_lngdppc",
                                                  "l_fiscal_balance","l_capital_controls","l_lngdp","l_lninfl","xrdum","year"),
                                             FE=c('cowcode'),
                                             data = 'rep_bodea_2015b.dta')


  ##############################################################################


  Carpenter_Moore_APSR_2014 = list()
  Carpenter_Moore_APSR_2014[['rep_carpenter_2014']] = list(Y="totnamesper1000",
                            D="pctpetwomen",
                            X="pctfocgag",
                            FE = c("countyid_number", 'congress'),
                            Z=c("pctfocdc", "pctpetsep", "pctfocterr", "pctfocnewst" ,"pctfocslavetrade"),
                            data = 'rep_carpenter_2014.dta')


  ##############################################################################


  Chapman_IO_2009 = list()
  Chapman_IO_2009[['rep_chapman_2009']] = list(Y="rally",
                          D="unauth",
                          X="S",
                          Z=c("priorpop","bipart","statemnt","nextelec","nytcov","buscon","revorg","wardumk","majopp","allies","war","SCappeal","regorgact","ordsever","hostlvl"),
                          data = 'rep_chapman_2009.dta')

  ##############################################################################


  Clark_Golder_CPS_2006 = list()
  Clark_Golder_CPS_2006[['rep_clark_2006a']] = list(Y="enep1",
                        D="eneg",
                        X="logmag",
                        Z=c("uppertier" ,"enpres" ,"proximity1", "uppertier_eneg" ,"proximity1_enpres"),
                        data = 'rep_clark_2006a.dta')

  Clark_Golder_CPS_2006[['rep_clark_2006b']] = list(Y="enpv",
                                                    D="eneth",
                                                    X="lnml",
                                                    Z=c("upper" ,"enpres" ,"proximit", "proxpres" ,"uppereneth"),
                                                    data = 'rep_clark_2006b.dta')

  Clark_Golder_CPS_2006[['rep_clark_2006c']] = list(Y="enep1",
                                                    D="eneg",
                                                    X="logmag",
                                                    Z=c("uppertier" ,"enpres" ,"proximity1", "uppertier_eneg" ,"proximity1_enpres"),
                                                    data = 'rep_clark_2006c.dta')

  Clark_Golder_CPS_2006[['rep_clark_2006d']] = list(Y="enep1",
                                                    D="proximity1",
                                                    X="enpres",
                                                    Z=c("uppertier" ,"logmag" , "uppertier_eneg" ,"eneg", "logmag_eneg"),
                                                    data = 'rep_clark_2006a.dta')

  ##############################################################################


  Clark_Leiter_CPS_2014 = list()
  Clark_Leiter_CPS_2014[['rep_clark_2014']] = list(Y="votechgtt1",
                        D = "cgscn2tt1",
                        X = "partydispuw",
                        Z =c ("votechgt1t2","ingovt_mc"),
                        FE = c('partnum'),
                        data = 'rep_clark_2014.dta')


  ##############################################################################


  Hellwig_Samuels_CPS_2007 = list()
  Hellwig_Samuels_CPS_2007[['rep_hellwig_2007a']] = list(Y="incvotet",
                          D="dgdp",
                          X="tradeshr",
                          Z=c("incvotet1", "electype" ,"gdpxelectype" ,"presrun", "enlp" ,"income" ,"regafrica", "regasia" ,"regcee" ,"reglatam"),
                          data = 'rep_hellwig_2007a.dta')

  Hellwig_Samuels_CPS_2007[['rep_hellwig_2007b']] = list(Y="incvotet",
                                                         D="dgdp",
                                                         X="grosscap",
                                                         Z=c("incvotet1", "electype" ,"gdpxelectype" ,"presrun", "enlp" ,"income" ,"regafrica", "regasia" ,"regcee" ,"reglatam"),
                                                         data = 'rep_hellwig_2007b.dta')

  Hicken_Simmons_ajps_2008 = list()
  Hicken_Simmons_ajps_2008[['rep_hicken_2008']] = list(Y="log_illit_all",
                         D="educgdp",
                         X="parindex",
                         Z=c("log_capgdpppp","poprural","lamerica","psoviet","sasia2","easia2","mideast"),
                         data = 'rep_hicken_2008a.dta')


  ##############################################################################


  Huddy_APSR_2015 = list()
  Huddy_APSR_2015[['rep_huddy_2015a1']] = list(Y = 'totangry',
                                               D = 'threat',
                                               X = 'pidentity',
                                               Z = c("issuestr2", "pidstr2", "pidstr2_threat" ,"issuestr2_threat", "knowledge" , "educ" , "male" , "age10"),
                                               data = 'rep_huddy_2015a.dta',
                                               custom = c('data$pidstr2_threat<-data$pidstr2 * data$threat', 'data$issuestr2_threat<-data$issuestr2 * data$threat'))

  Huddy_APSR_2015[['rep_huddy_2015a2']] = list(Y = 'totangry',
                                              D = 'threat',
                                              X = 'pidentity',
                                              Z = c("issuestr2", "pidstr2", "knowledge" , "educ" , "male" , "age10"),
                                              data = 'rep_huddy_2015a.dta',
                                              custom = c('data$pidstr2_threat<-data$pidstr2 * data$threat', 'data$issuestr2_threat<-data$issuestr2 * data$threat'))

  Huddy_APSR_2015[['rep_huddy_2015b1']] = list(Y = 'totpos',
                                               D = 'support',
                                               X = 'pidentity',
                                               Z = c("pidstr2", "issuestr2","issuestr2Xsupport", "pidstr2Xsupport", "knowledge" , "educ","male" , "age10"),
                                               data = 'rep_huddy_2015b.dta',
                                               custom = c('data$issuestr2Xsupport=data$issuestr2*data$support', 'data$pidstr2Xsupport=data$pidstr2*data$support'))

  Huddy_APSR_2015[['rep_huddy_2015b2']] = list(Y = 'totpos',
                                               D = 'support',
                                               X = 'pidentity',
                                               Z = c("pidstr2", "issuestr2", "knowledge" , "educ","male" , "age10"),
                                               data = 'rep_huddy_2015b.dta',
                                               custom = c('data$issuestr2Xsupport=data$issuestr2*data$support', 'data$pidstr2Xsupport=data$pidstr2*data$support'))

  ##############################################################################

  Kim_LeVeck_2013 = list()
  Kim_LeVeck_2013[['rep_kim_2013a']] = list(Y = 'infadjownexp1k',
                                            D = 'partysd',
                                            X = 'opres',
                                            Z = c("absmeandist","partysdXabsmeandist","opresXabsmeandist", "infadjoppexp1k", "quality","frosh"),
                                            FE = c('year', 'stcd_redist'),
                                            data = 'rep_kim_2013.dta',
                                            custom = c('data = na.omit(data[data$incumbent2==1 &  !is.na(data$marginprev),
              c("infadjownexp1k" ,"partysd", "opres",
                "partysdXopres", "absmeandist" ,"partysdXabsmeandist",
                "opresXabsmeandist" , "infadjoppexp1k" ,"quality"   ,
                "frosh",  "year" , "stcd_redist" , "stcd",
                "marginprev","incumbent2","stcd")])'))

  Kim_LeVeck_2013[['rep_kim_2013b']] = list(Y = 'infadjownexp1k',
                                            D = 'absmeandist',
                                            X = 'opres',
                                            Z = c("partysd","partysdXopres","partysdXabsmeandist","infadjoppexp1k", "quality","frosh"),
                                            FE = c('year', 'stcd_redist'),
                                            data = 'rep_kim_2013.dta',
                                            custom = c('data = na.omit(data[data$incumbent2==1 &  !is.na(data$marginprev),
              c("infadjownexp1k" ,"partysd", "opres",
                "partysdXopres", "absmeandist" ,"partysdXabsmeandist",
                "opresXabsmeandist" , "infadjoppexp1k" ,"quality"   ,
                "frosh",  "year" , "stcd_redist" , "stcd",
                "marginprev","incumbent2","stcd")])'))


  Kim_LeVeck_2013[['rep_kim_2013c']] = list(Y = 'infadjownexp1k',
                                            D = 'absmeandist',
                                            X = 'partysd',
                                            Z = c("partysdXopres","opresXabsmeandist","infadjoppexp1k", "opres","quality","frosh"),
                                            FE = c('year', 'stcd_redist'),
                                            data = 'rep_kim_2013.dta',
                                            custom = c('data = na.omit(data[data$incumbent2==1 &  !is.na(data$marginprev),
              c("infadjownexp1k" ,"partysd", "opres",
                "partysdXopres", "absmeandist" ,"partysdXabsmeandist",
                "opresXabsmeandist" , "infadjoppexp1k" ,"quality"   ,
                "frosh",  "year" , "stcd_redist" , "stcd",
                "marginprev","incumbent2","stcd")])'))



  ##############################################################################

  Malesky_APSR_2012 = list()

  Malesky_APSR_2012[['rep_malesky_2012a']] = list(Y="d_question_count",
                          D="t2",
                          X="internet_users100",
                          Z=c("centralnominated","fulltime","retirement","city","ln_gdpcap","ln_pop","transfer","south","unweighted"),
                          data = 'rep_malesky_2012.dta')

  Malesky_APSR_2012[['rep_malesky_2012b']] = list(Y="d_criticize_total_per",
                                                  D="t2",
                                                  X="internet_users100",
                                                  Z=c("centralnominated","fulltime","retirement","city","ln_gdpcap","ln_pop","transfer","south","unweighted"),
                                                  data = 'rep_malesky_2012.dta')

  Malesky_APSR_2012[['rep_malesky_2012c']] = list(Y="diff_quest",
                                                  D="t2",
                                                  X="internet_users100",
                                                  Z=c("centralnominated","fulltime","retirement","city","ln_gdpcap","ln_pop","transfer","south","unweighted"),
                                                  data = 'rep_malesky_2012.dta')

  Malesky_APSR_2012[['rep_malesky_2012d']] = list(Y="diff_crit",
                                                  D="t2",
                                                  X="internet_users100",
                                                  Z=c("centralnominated","fulltime","retirement","city","ln_gdpcap","ln_pop","transfer","south","unweighted"),
                                                  data = 'rep_malesky_2012.dta')

  ##############################################################################

  Neblo_et_al_APSR_2010 = list()
  Neblo_et_al_APSR_2010[['rep_neblo_2010']] = list(Y="willing",
                        D="treatcong2",
                        X="stealth2_ct",
                        Z=c( "needjud_ct", "educ_ct", "income_ct", "interest_ct" ,"chur_ct",
                             "gentrust_ct", "intxcon", "treathour2", "treatint" ,"treattop" ,
                             "treatinc" , "gender2" ,"age_ct",  "conflict_ct" ,"needcog_ct" ,
                             "sunshine_ct", "efficacy_ct" ,"pidcoll_ct" ,"empfull", "white" ),
                        data = 'rep_neblo_2010.dta')

  ##############################################################################


  Pelc_IO_2011 = list()
  Pelc_IO_2011[['rep_pelc_2011a']] = list(Y="depth3",
                       D="polity3",
                       X="logfullimports",
                       Z=c("logGDPconst", "logfullexports" , "accessionperiod", "loggdpcap" ,
                           "MFNpre", "chinadum" , "membershiptiming","polityXexp"),
                       data = 'rep_pelc_2011a.dta')

  Pelc_IO_2011[['rep_pelc_2011b']] = list(Y="overhang",
                                          D="polity3",
                                          X="logfullimports",
                                          Z=c("logGDPconst", "logfullexports" , "accessionperiod", "loggdpcap" ,"MFNpre", "chinadum" , "membershiptiming","polityXexp"),
                                          data = 'rep_pelc_2011b.dta')

  ##############################################################################

  Petersen_APSR_2014 = list()
  Petersen_APSR_2014[['rep_peterson_2013a']] = list(Y="tougher",
                           D="lazy",
                           X="imagine",
                           Z=NULL,
                           data = 'rep_petersen_2013a.dta')

  Petersen_APSR_2014[['rep_peterson_2013a']] = list(Y="dictator",
                                                    D="attitude",
                                                    X="imagine",
                                                    Z=NULL,
                                                    data = 'rep_petersen_2013b.dta',
                                                    custom = 'data$im<-data$imagine01')

  ##############################################################################

  Somer_Topcu_JOP_2009 = list()
  Somer_Topcu_JOP_2009[['rep_somer_2009']] = list(Y="absch1",
                        D="votech2",
                        X="monthstoprevelect",
                        Z=c("absch2"),
                        data = 'rep_somer_2009.dta')

  ##############################################################################

  Tavits_CPS_2007 = list()
  Tavits_CPS_2007[['rep_tavits_2008']] = list(Y="voteslost",
                         D="neighbor",
                         X="importance",
                         Z=c("yearsdem" ,"turn_ch" ,"govt", "unemp", "gdp" ,"ln_mdm", "ln_infl", "votes_last" ,"c1" ,"c2", "c3" ,"c4", "c5" ,"c6" ,"c7" ,"c8", "c9"),
                         data = 'rep_tavits_2008.dta')

  ##############################################################################

  Truex_APSR_2014 = list()
  Truex_APSR_2014[['rep_truex_2014a']] = list(Y="roa",
                        D="npc",
                        X="so_portion",
                        Z=NULL,
                        FE = c('gvkey', 'fyear'),
                        data = 'rep_truex_2014a.dta')

  Truex_APSR_2014[['rep_truex_2014b']] = list(Y="roa",
                                              D="npc",
                                              X="rev2007",
                                              Z=NULL,
                                              FE = c('gvkey', 'fyear'),
                                              data = 'rep_truex_2014a.dta')

  Truex_APSR_2014[['rep_truex_2014c']] = list(Y="margin",
                                              D="npc",
                                              X="so_portion",
                                              Z=NULL,
                                              FE = c('gvkey', 'fyear'),
                                              data = 'rep_truex_2014b.dta')

  Truex_APSR_2014[['rep_truex_2014d']] = list(Y="margin",
                                              D="npc",
                                              X="rev2007",
                                              Z=NULL,
                                              FE = c('gvkey', 'fyear'),
                                              data = 'rep_truex_2014b.dta')


  ##############################################################################

  Vernby_AJPS_2013 = list()
  Vernby_AJPS_2013[['rep_vernby_2013a']] = list(Y="school.diff",
                                                D="noncitvotsh",
                                                X="noncit15",
                                                Z=c("Taxbase2" ,  "Taxbase2.2" , "pop" , "pop.2" ,   "manu" , "manu.2"),
                                                data = 'AJPSReplicationData.dta')


  Vernby_AJPS_2013[['rep_vernby_2013b']] = list(Y="socialvard.diff",
                                                D="noncitvotsh",
                                                X="noncit5",
                                                Z=c("Taxbase2" ,  "Taxbase2.2" , "pop" , "pop.2" ,   "manu" , "manu.2"),
                                                data = 'AJPSReplicationData.dta')

  ##############################################################################

  Williams_CPS_2011 = list()
  Williams_CPS_2011[['rep_williams_2011a']]  = list(Y="change",
                          D="opp_conf_party_elecdate",
                          X="eff_par",
                          Z=c("opp_conf_elecdate","majority", "gparties", "lag_pervote" ,"rgdppc_growth" ,"eoc"),
                          data = 'rep_williams_2011a.dta')

  Williams_CPS_2011[['rep_williams_2011b']]  = list(Y="change",
                                                    D="opp_conf_party_elecdate",
                                                    X="abs_rile",
                                                    Z=c("majority" ,"gparties", "lag_pervote", "rgdppc_growth" ,"ncm_all_abs_rile","opp_conf_elecdate"),
                                                    data = 'rep_williams_2011b.dta')



  ##############################################################################

  rep_names <- ls(envir = .GlobalEnv)
  hainmueller_list <- mget(rep_names[!rep_names == 'object_names'], envir = .GlobalEnv)

  save(hainmueller_list, file = 'hainmueller_replication/data/hainmueller_list.rdata')
