Replication Materials for "How Much Should We Trust Estimates from Multiplicative Interaction Models?" by Jens Hainmueller, Jonathan Mummolo, and Yiqing Xu. Political Analysis. Forthcoming. 

The replication files include 4 folders:
1. In the "Code" folder are 28 R files that replicate the results of this paper
2. In the "Data" folder are replication data from 22 papers we replicated
3. The "Graphs" folder stores the graphics created by the code files (figures to be used in the main text are stored in "Graphs/main" folder)
4. In the "Tex" folder are two LaTex files that replicate Figure 8 in the main text and Online Supplementary Information (Appendix B)

** Replicating all results takes about 3 hours on a 2017 Macbook Pro with 4 cores and 8G of RAMs. 


+-------------------------
+ Replication Procedure 
+-------------------------

1. Install "interflex" package from Github. In R, type:
install.packages('devtools', repos = 'http://cran.us.r-project.org') # if not already installed
devtools::install_github('xuyiqing/interflex')

install.packages("interflex", type = "source")
** Note **
Mac users who encounter “-lgfortran” or “-lquadmath” error during installation due to the lack of a correct complier, please check out this link: https://thecoatlessprofessor.com/programming/rcpp-rcpparmadillo-and-os-x-mavericks--lgfortran-and--lquadmath-error/
Typing the following two lines of code in your Terminal should solve this problem.

** Other required R packages:
1) ggplot2
2) lmtest
3) sandwich
4) foreign
5) lfe

curl -O http://r.research.att.com/libs/gfortran-4.8.2-darwin13.tar.bz2
sudo tar fvxz gfortran-4.8.2-darwin13.tar.bz2 -C /

2. Execute the following R files in order:
1) 1_simulated.R --> replicates Figures 1-3 in the main text and Figure A1 in the appendix
2) 2_figure4.R --> replicates Figure 4
3) 3_figure5.R --> replicates Figure 5
4) 4_figure6.R --> replicates Figure 6
5) 5_figure7.R --> replicates Figure 7
** Note **
Please change "path" to the root directory of the replication materials

3. The rest 23 files replicates Figure 8 as well as results in the Online Appendix
** Notes ***
a. "hmx.R" needs to be sourced such that "replicate()" and "plot.all()" functions can be used.
b. These replication files do not have to be run in a particular order. 
c. Once all code files are executed, all figures appearing in the paper or in the Online Appendix will be reproduced. 
d. Each replication code file (named "rep_firstauthor_year.R")  produces 5 graphs for each of interaction cases we investigated in that paper:
* Used in Figure 8 in the paper *
1). A plot of marginal effects with binning estimates -- "firstauthor_year_est.pdf"
* Used in the Online Appendix *
2). A raw data plot -- firstauthor_year_raw.pdf
3). A GAM plot of GAM  -- "firstauthor_year_gam.pdf"
4). A plot of marginal effects with binning estimates (without title) -- "firstauthor_year_est0.pdf"
5). A plot of marginal effects using the kernel estimator -- "firstauthor_year_smooth.pdf"

4. Once all the graphs are in place, "figure8.tex" will reproduce Figure 8 in the main text and "Online_Appendix.tex" will reproduce Online Supplementary Information (Appendix B). Both files are in the "Tex" folder.