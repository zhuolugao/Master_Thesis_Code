======================
A1_compustat_monthly_data.R
======================
1. Require, clean, renamre data from CRSP database, save it as "compustat_monthly.rds"

2. Calculate cost of debt. Since the # of cost of debt is small, we can choose to use the interest rate to replace it.

3. Span the quarterly data to monthly data, save it as "span_compustat.rds".

4. Visulization of the real cost of debt.

5. Require the interest rate data from CRSP, merge the datasets into "span_compustat.rds".
279 lines

======================
B1_crsp_data.R
======================
1. Require the return data from CRSP database, save in "crsp_full_data.rds"

2. Require the market data from Kennth library. save in "market.rds"

3. Make the summary table in Appendix, Table 2. Count how many banks in each group within each month
172 lines

======================
B2_crsp_beta_compute.R
======================
1. Merge the return data "crsp_full_data.rds" and the market data "market.rds".

2. Calculate the backward beta, save in "backward_betas.rds". (a double check is done by calculating cov/var rather than OLS regression)

3. Based on the dataset "backward_betas.rds", calculate the forward beta, save in "forward_beta.rds". 
231 lines

======================
B3_crsp_look.R
======================
1. Based on the dataset "forward_beta.rds", make a table to summary the backward and forward beta, use the function KABLE directly to LATEX

2. Calculate the VW and EW average of backward beta, and plot it, see the figure in Section 4.1.

3. Plotting the VW and EW average of three portfolios, using "backward_beta.rds", figures in Section 4
94 lines

======================
B5_BW_replicating.R
======================
1. Reproduce the BW regression, same time period, same grouping procedure.

2. Update the low risk anomaly regression, regression the portfolio's alpha on beta, with 10, 20 portfolios. and with 30%, 40%, 30% portfolios.

3. Plotting all the regressions; Report all the regression results.
700 lines

======================
C1_combine.R
======================
1. Combine the datasets of Compustat "span_compustat.rds" and CRSP return data "forward_beta.rds" with a link table "linktable.rds" (obtained in A2, not important)

2. Define the ratios: 
inverse.tier1 = 1/tier1 ratio
equity.ratio = common.equity / asset
inverse.equity.ratio = 1/ equity.ratio

3. Save in "C1_combine_crsp_comp.rds"
50 lines

======================
C2_combine_tables.R
======================
1. Use "C1_combine_crsp_comp.rds", make the portfolio table in Section 5.1
50 lines

======================
C3_combine_analysis.R
======================
1. The BW regression of beta on inverse tier 1 ratio, but with data from Compustat database

2. Piecewise linear regression

3. IV regression of realized return on inverse Tier 1 ratio.
143 lines

======================
G1_permco_rssid.R
======================
1. Combine the dataset "forward_beta.rds" with the data from the Bank Regulatory database.

2. Reproduce the regression of BW

3. Reproduce the kernel regression

======================
D1--D3: calculate the ICC
======================
1. Save in "icc_final.rds"

======================
D4_ICC_analysis.R
======================



































