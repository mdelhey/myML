data(iris)

# Define variables
X_con <- as.matrix(iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")])
X_cat <- cbind(X_con, iris[, "Species"])
y <- iris[, "Sepal.Length"]

myRegression <- function(X, y) {
    # Input: X (matrix); y (vector)
    # Catagorical variables must be integers
    stopifnot(is.matrix(X) && is.numeric(y))
        
    # Check to see if catagorical varibles
    check <- as.vector(do.call("cbind", lapply(1:ncol(X), function(c) all(X[, c] %% 1 == 0))))
    if (any(check)) { X <- codeVar(X, check) }

    # Add column of 1's
    X_design <- cbind(1, X)

    # Calculate beta hat
    B_hat <- solve(t(X_design) %*% X_design) %*% t(X_design) %*% y

    # Name the vector
    names(B_hat) <- c("Intercept", colnames(X))

    return(t(B_hat))
}

codeVar <- function(X, index) {
    # Baseline catagorical varible coding
    # Assumes that lowest int is baseline
    stopifnot(is.matrix(X), is.vector(index))
    
    cols <- lapply(1:ncol(X), function(c) {
        if (index[c]) {
            # Get factor levels
            lvls <- sort(unique(X[,c]))

            # Create baseline
            baseline <- min(lvls)
            
            # Create new variables
            lapply(lvls[-1], function(l) ifelse(X[,c] == l, 1, 0))
        }
    })

    # Remove original variable
    X <- X[, !index]
   
    # Combine with X
    cbind(X, do.call("cbind", unlist(cols, recursive=FALSE)))
}

coefs <- myRegression(X_cat, y)

# Test vs. R implimentation
fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, data = iris)
all.equal(as.vector(coefs), as.vector(fit$coefficients))
