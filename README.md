---
title: "PHP2560 Final Project: Retirement Plan Simulator"
author: Yingxi Kong, Yunan Chen, Yingqiu Huang
output:
  bookdown::pdf_document2:
    toc: false
    number_sections: false
---

# Retirement Plan Simulator Description

## Background

When planning for retirement investments, investors face numerous uncertainties, such as fluctuating inflation rates and varying interest rates across different investment plans. The success of a retirement plan also depends on factors like the initial down payment and the annual contributions made to the plan. To address these uncertainties and assist investors in finding the optimal retirement strategy, we have developed a simulator. This tool projects the value of an investor's portfolio from the start of their investment journey until they reach the age of 100.

## Simulation Generating Process

### Formula for Portfolio Value before Expected Retirement Year

This formula calculates the annual value of the investor's portfolio before retirement, taking into account the following parameters:

-   D (Down Payment): The initial amount contributed by the investor in the beginning of the plan.
-   r (Inflation Rate): The annual inflation rate, varying from 1% to 7%.
-   x (Annual Investment): The amount invested by the investor into the plan each year.
-   i (Interest Rate): The annual interest rate applied to the investment, varying from 1% to 10%.
-   n (Number of Years): The duration from the commencement of the investment until the anticipated retirement year.
-   p (Annual Spending): The yearly expenditure the investor expects to incur.
-   s (Social Security Income): The annual income from government social security benefits received post-retirement.

```{=tex}
\begin{equation} 
Profile Value = D \cdot (1 + \frac{i-r}{1+r})^n + x \cdot \frac{(1 + \frac{i-r}{1+r})^n - 1}{\frac{i-r}{1+r}}
(\#eq:mod1)
\end{equation}
```
### Formula for Profile Value after Expected Retirement Year

This formula calculates the yearly value of an investor's portfolio following retirement, incorporating **p** for their yearly expenditures and **s** for their social security benefits.

```{=tex}
\begin{equation} 
ProfileValue_{k+1}=ProfileValue_{k} \cdot (1 + \frac{i-r}{1+r}) - p + s, \ \text{k= number of years after retirement}
(\#eq:mod2)
\end{equation}
```
### Simulation Functions

``` r
retire_plan(D, x, age, current_year, retire_year, s, i, r, p)
```

This function uses formula \@ref(eq:mod1) and formula \@ref(eq:mod2) and returns a data frame including columns for the year, the age of an investor, profile value, and simulation iteration.

``` r
sim_retirement(D, x, age, current_year, retire_year, s, p, interest_list, infla_list, num_simulations)
```

This function generates random samples for the interest and inflation rates, and then it proceeds to compute the retirement plan using these varying rates. It outputs a data frame that includes columns for the year, the investor's age, portfolio value, simulation iteration, interest rate, and inflation rate, as well as an 'event' status indicating 'success' or 'failure.' A 'success' event signifies that the investor's portfolio value remains above zero at the age of 100, indicating financial longevity. Conversely, a 'failure' event occurs if the portfolio value drops below zero before reaching 100 years old, reflecting insufficient funds for the investor's lifespan.

## In-App

This Shiny application features an input section that takes in user data including Down Payment, Annual Investment, Social Security Income, Annual Spending, Age, Current Year, and Retirement Year.

The application is divided into two tabs:

1.  'Visualization': This tab presents two graphical representations. The first is an interactive line graph that displays all simulation outcomes, with 'year' on the x-axis and 'portfolio value' on the y-axis. Successful events are marked in green, while failures are indicated in red. The second graph illustrates the probability of success, defining survival as instances where this probability exceeds 80%.

2.  'Summary Table': This tab generates a table summarizing the simulated portfolio value distribution. It identifies the optimal, 75th, median (50th), 25th percentile, and lowest portfolio values across different years. An additional column reveals the average year when the portfolio value depletes to zero.
