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

### i) Formula for Profile Value before Expected Retirement Year

**D (Down Payment)**: Initial Payment for the investor

**r (Inflation Rate)**: Inflation rate is between 1% - 7%

**x (Annual Investment)**: The amount of money the investor put in the plan each year

**i (Interest Rate)**: Interest rate is between 1% - 10%

**n (Number of Years)**: The number of years from the start of the investment to the expected retirement year

**p (Annual Spending)**: The expected amount of money the investor spends each year

**s (Social Security Income)**: The amount of money government issues each year after the investor's retirement

The formula here calculates the investor's profile value of each year based on parameters given above.

$$Profile Value = D \cdot (1 + \frac{i-r}{1+r})^n + x \cdot \frac{(1 + \frac{i-r}{1+r})^n - 1}{\frac{i-r}{1+r}}$$

### ii) Formula for Profile Value after Expected Retirement Year

The formula calculates the investor's profile value of each year after their retirement, given that p is their annual spending and s is social security income. 

$$ProfileValue_{k+1}=ProfileValue_{k} * (1 + \frac{i-r}{1+r}) - p + s, \text{k= number of years after retirement}$$

### iii) Simulation


