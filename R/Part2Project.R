# Function for linear regression and hypotheses
#' Checking Assumptions and Display Relevant Graphs for Linear Regression
#'
#' @param file_name a character string of the name of the raw file containing the data that is going to be tested.
#' @param x_var column name in the file containing the data of the independent variable
#' @param y_var column name in the file containing the data of the dependent variable
#'
#' @return assumption confirmation and result of the test
#' @export
#'
#' @examples
#' linear_regression_and_hypotheses("project.csv","height","weight")
linear_regression_and_hypotheses <- function(file_name, x_var, y_var) {
  cat("HYPOTHESIS\n")
  cat("Null Hypothesis (H0): Beta = 0 (No relationship between", x_var, "and", y_var, ")\n")
  cat("Alternative Hypothesis (H1): Beta is not equal to 0 (There is a relationship between", x_var, "and", y_var, ")\n\n")

  data <- readr::read_csv(file_name)
  fit <- stats::lm(data[[y_var]] ~ data[[x_var]])

  cat("ASSUMPTIONS\nPlease check the plots.\n")
  plot_scatter <- ggplot2::ggplot(data, ggplot2::aes(x = x_var, y = y_var)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle(paste("Scatterplot of", y_var, "vs", x_var)) +
    ggplot2::xlab(x_var) + ggplot2::ylab(y_var)

  lm_resid <- broom::augment(fit)
  plot_resid <- ggplot2::ggplot(lm_resid, ggplot2::aes(x = .fitted, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Scatterplot of fitted values vs residuals") +
    ggplot2::xlab("Fitted values") + ggplot2::ylab("Residuals")

  plot_hist <- ggplot2::ggplot(lm_resid) +
    ggplot2::geom_histogram(ggplot2::aes(x = .resid)) +
    ggplot2::ggtitle("Histogram of residuals") +
    ggplot2::xlab("Residuals")

  patchwork::wrap_plots(plot_scatter, plot_resid, plot_hist, ncol = 1)

  cat("FIT - Linear regression\n")
  cat("Estimated Slope (Beta): ", stats::coef(fit)[2], "\n")
  cat("95% Confidence Intervals for Beta: [", stats::confint(fit)[2, 1], ", ", stats::confint(fit)[2, 2], "]\n")
  cat("Value of t: ", summary(fit)$coefficients[, "t value"][2], "\n")
  cat("Degrees of Freedom (df): ", summary(fit)$df[2], "\n")
  cat("P-value: ", summary(fit)$coefficients[, "Pr(>|t|)"][2], "\n")

  return(fit)
}

# Call the function for Question 1
lm_result_1 <- linear_regression_and_hypotheses("project.csv", "height", "weight")

# Question 2 (T-Test for Mean Height)
# Function to perform a t-test for mean height
#'Display t-test result between mean and height
#'
#' @param data_file a dataset that contains the information to be tested
#'
#' @return t-test result
#' @export
#'
#' @examples
#' t_test_mean_height("project.csv")
t_test_mean_height <- function(data_file) {
  data <- readr::read_csv(data_file)

  t_test_result <- stats::t.test(data$height[data$gender == "Male"], data$height[data$gender == "Female"])

  cat("Question 2 Results:\n")
  cat("t-value:", t_test_result$statistic, "\n")
  cat("P-value:", t_test_result$p.value, "\n")
  alpha <- 0.05  # Set your significance level
  cat("Decision:", ifelse(t_test_result$p.value < alpha, "Reject Null Hypothesis", "Do Not Reject Null Hypothesis"), "\n")
  cat("Conclusion:", ifelse(t_test_result$p.value < alpha, "The mean height of males and females is different.", "The mean height of males and females is the same."), "\n")
}

# Usage:
t_test_mean_height("project.csv")

# Question 3(Chi-squared Test for Association between Gender and physical activity)
# Function to perform a chi-squared test for association between gender and physical activity
#' Display relevant table for chi-squared test and perform chi-square test
#'
#' @param data_file a character string of the name of the raw file containing the data that is going to be tested.
#'
#' @return result of the test
#' @export
#'
#' @examples
#' chi_squared_test_gender_activity("project.csv")
chi_squared_test_gender_activity <- function(data_file) {
  data <- readr::read_csv(data_file)

  contingency_table <- table(data$gender, data$phys)
  chi_squared_result <- stats::chisq.test(contingency_table)

  cat("Question 3 Results:\n")
  cat("Chi-squared value:", chi_squared_result$statistic, "\n")
  cat("P-value:", chi_squared_result$p.value, "\n")
  alpha <- 0.05  # Set your significance level
  cat("Decision:", ifelse(chi_squared_result$p.value < alpha, "Reject Null Hypothesis", "Do Not Reject Null Hypothesis"), "\n")
  cat("Conclusion:", ifelse(chi_squared_result$p.value < alpha, "There is an association between gender and physical activity.", "There is no association between gender and physical activity."), "\n")
}
