# Conjoint Analysis

A sample conjoint analysis based on a paper by [Aizaki & Nishimura (2008)](https://www.jstage.jst.go.jp/article/air/17/2/17_2_86/_pdf). Tweaked to incorporate `dplyr` syntax/functions and generate data from scratch.

## Workflow
* Create a full factorial design and two partial designs for the different combinations of variables affecting people's choices of which brand of milk to buy
* Simulate data
* Apply a conjoint analysis to determine which independent variables have a significant effect on the dependent variable
* Extract log likelihoods (at 0 and at convergence) from the model