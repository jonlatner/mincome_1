# mincome_calnitsky_latner_2017

[![DOI:10.7910/DVN/YQDAEC](http://img.shields.io/badge/DOI-10.7910/DVN/YQDAEC.425840-B31B1B.svg)](https://doi.org/10.7910/DVN/YQDAEC)

## Introduction

Repository for the codes used for the analyses of David Calnitsky &amp; Jonathan Latner (2017), "Basic Income in a Small Town: Understanding the Elusive Effects on Work."

All analyses in the article were conducted using R. 

Complete R codes used are in separate .do -files.  The distinct files download the data, clean it, analyze it, and reproduce various tables and graphs.

The full article can be found at: https://doi.org/10.1093/socpro/spx024

There are two small "howevers" that users should know about:

1) there was 1 small coding error in do_files/02_clean.R in 
line 284/285:

replace earnings_73 = 0 if earnings_73 == . & (mearnings74 == 0 | fearnings74 == 0 | totalearnings74 == 0)

As one can see, it should be:

replace earnings_73 = 0 if earnings_73 == . & (mearnings73 == 0 | fearnings73 == 0 | totalearnings73 == 0)

The result is that one Dauphin family which was excluded in the original paper is now included (famno == 35723).  This very slightly changes the coefficients in Table 3 (i.e. the diff in diff table), but does not qualitatively alter the findings.

2) I didn't like table 4, the baseline characteristics.  In the paper, the numbers don’t really add up the way they should.  Further, table 4 refers to "married", but figure 5 refers to dual headed, which is different.  Also, I didn't like how high school and no high school was created in the original paper.  The original code allowed for a family to have both a high school degree and not a high school degree.  In the end, the changes make little difference.
