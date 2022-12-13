# hedge-fund-activism

Fast approach to hedge fund activism (HFA) data collection

The data collection process for HFA has been found challenging, due to the following difficaulties: 

- Downloading of SC 13D forms, which is large in volume and varies in file types;
- Extraction of relevant information from SC 13D filing, which requires text mining;
- Matching subject names from SC 13D forms to ticker symbols to perform stock analysis;
- Acquiring stock data for all target firms. 

We collect HFA data in the following way: 
- SC 13D forms are downloaded from SEC, parsed, and stored in local database. 
We adapted the code from Schwartz-Ziv & Volkova (2021).
- Then ticker symbols are matched to CIK on 13D filings with `sec-cik-mapper`.
- Next, identify *hedge fund* filers using information from WhaleWisdom.com.
- Stock prices are retrieved from Yahoo Finance using R package `quantmod`.
- S&P 500 returns are used for calculation of abnormal returns.

## Reference 

1. Schwartz-Ziv, M., & Volkova, E. (2021). Is Blockholder Diversity Detrimental? 
Available at SSRN: https://ssrn.com/abstract=3621939 or http://dx.doi.org/10.2139/ssrn.3621939
GitHub:https://github.com/volkovacodes/Block_Codes

2. `sec-cik-mapper`: https://github.com/jadchaar/sec-cik-mapper

3. `quantmod`: https://github.com/joshuaulrich/quantmod

4. Brav, A., Jiang, W., Partnoy, F., & Thomas, R. S. (2008b). Hedge Fund Activism, Corporate Governance, and Firm Performance. The Journal of Finance (New York), 63(4), 1729–1775. 
