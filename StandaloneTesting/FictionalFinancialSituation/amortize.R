
MonthlyAmortization  <- function(loan, apr_IN_percent, months) {
    rate <- 1 + apr_IN_percent / 12
    
    # http://en.wikipedia.org/wiki/Amortization_calculator
    # A = P * [i(1+i)^n / (1+i)^n -1]
    # loan * rate^months * (rate - 1) / (rate^months - 1)
    # i = rate-1
    # A = P * [ i+ (i/ ((1+i)^n -1) ) ] as last presented in Wikipedia
    loan * ((rate-1)+((rate-1)/((rate^months)-1) ))
}
