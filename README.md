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

### i) Retirement Plan Formula
$$D * (1 + \frac{(i-r)}{(1+r)})^j + x * (\frac{((1 + \frac{(i-r)}/{(1+r)})^j - 1)}{\frac{(i-r)}{(1+r)})$$
