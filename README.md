# RPT5000 — Advanced Year-To-Date Sales & Branch Reporting

The **RPT5000** program is an enhanced COBOL reporting utility designed to process complex financial data. It serves as an upgraded data processing tool that reads customer records from a master file (`CUSTMAST`) and generates a highly structured, multi-level Year-To-Date (YTD) Sales Report.

## What it does 
* **Multi-Level Control Breaks:** The program identifies changes in `Branch Number` and `SalesRep Number` to trigger intermediate total calculations automatically.
* **Financial Comparisons:** It calculates the variance between current YTD sales and last year's YTD sales, including both the raw dollar amount and the percentage of change.
* **Hierarchical Reporting:** It provides three distinct levels of summation:
    1.  **SalesRep Totals:** Summary of all customers under a specific representative.
    2.  **Branch Totals:** Summary of all sales within a specific branch.
    3.  **Grand Totals:** Final company-wide summation of all processed data.
* **Automated Formatting:** Handles page headers, timestamps, page numbering, and clean columnar alignment for professional printing.

## New COBOL Concepts Implemented
Compared to previous iterations like the **RPT3000**, this version introduces:
* **Complex Evaluation Logic:** Uses `EVALUATE TRUE` to manage the state of the data stream (End-of-File, First Record, or Control Breaks).
* **Control Field Tracking:** Uses `OLD-SALESREP-NUMBER` and `OLD-BRANCH-NUMBER` in `WORKING-STORAGE` to track and trigger logic when the data group changes.
* **Arithmetic Precision:** Uses `COMPUTE` with `ROUNDED` and `ON SIZE ERROR` handling to ensure financial calculations remain accurate and do not crash the program on overflow.
* **Condition Names:** Implementation of `88` level condition names for cleaner switch management (`CUSTMAST-EOF` and `FIRST-RECORD`).
  
## Program Output

Below is a screenshot of the RPT5000 execution, showing the clear separation between SalesRep totals, Branch Totals and the final grand Totals:

![Program Output](Assets/RPT5000Output.png) 

---

### Author Profiles
* [Tristan Joubert - GitHub](https://github.com/TJoubert004)
