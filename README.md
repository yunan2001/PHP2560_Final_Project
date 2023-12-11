---
title: "Retirement Plan Simulator"
author: Yingxi Kong, Yunan Chen, Yingqiu Huang
output:
  bookdown::pdf_document2:
    toc: false
---

# Retirement Plan Simulator Description

## Background

When it comes to investment planning in retirement, there are a lot of uncertainties, such as unpredictable inflation rate and different interest rate for different investment plan. The retirement plan also relies on the amount of down payment and the amount of money the investor put in the plan each year. To account for these uncertainties and to better help investors to find the optimal planning for retirement, we created a simulator that can generate the the investor's profile value from the start year of their investment to the year when they turn 100.

## Simulation Process

### Formula for Profile Value before Expected Retirement Year {#sec-before}

The formula here calculates the investor's profile value of each year based on parameters given above.

-   D (Down Payment): Initial Payment for the investor

-   r (Inflation Rate): Inflation rate is between 1% - 7%

-   x (Annual Investment): The amount of money the investor put in the plan each year

-   i (Interest Rate): Interest rate is between 1% - 10%

-   n (Number of Years): The number of years from the start of the investment to the expected retirement year

-   p (Annual Spending): The expected amount of money the investor spends each year

-   s (Social Security Income): The amount of money government issues each year after the investor's retirement

$$
Profile Value = D \cdot (1 + \frac{i-r}{1+r})^n + x \cdot \frac{(1 + \frac{i-r}{1+r})^n - 1}{\frac{i-r}{1+r}}
$$

### Formula for Profile Value after Expected Retirement Year {#sec-after}

The formula calculates the investor's profile value of each year after their retirement, given that p is their annual spending and s is social security income.

$$
ProfileValue_{k+1}=ProfileValue_{k} * (1 + \frac{i-r}{1+r}) - p + s, \text{k= number of years after retirement}
$$

### Simulation

-   Retirement Plan Calculation Function: This function uses formulas mentioned in \@ref(sec-before) and \@ref(sec-after). This function will output the a data frame containing year, age, profile value, simulation number.

-   Simulation Function: First, we sample the interest rate and the inflation rate. Second, we simulate the retirement plan calculation function using the sampled interest rate and inflation rate. Finally, we construct a data frame containing year, age, profile value, simulation number, interest rate, inflation rate, event (success or failure). Success means that the investor's profile value is not 0 when they turn 100 years old. (i.e. they survive) Failure means that the investor's profile value becomes less than 0 before they turn 100 years old. (i.e. they do not have enough money to survive)

## Outcomes

### Shiny App

This Shiny app takes in user's inputs (Down Payment, Annual Investment, Social Security Income, Annual Spending, Age, Current Year, Retire Year), and output two plots and one table.

### Interpretation of Plots and Table

-   Plots: The first plot is an interactive line plot showing all simulation results. The x-axis is year, and the y-axis is the profile value. The event is coded as green (success) and red (failure). The second plot displays the overall probability of success. We consider an investor survived if the probability of success is greater than 80%.

-   Table: The table shows the distribution of profile value under the simulation. We found the best, 75th percentile, 50th percentile, 25th percentile, worst profile values at different years. We also added a column showing the average year where profile value goes to 0.
