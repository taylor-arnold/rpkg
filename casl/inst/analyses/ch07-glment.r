library(tokenizers)
library(smodels) # devtools::install_github("statsmaths/smodels")
devtools::document() #library(akl)
library(dplyr)
library(ggplot2)
library(stopwords)
library(Matrix)
library(glmnet)

# Load the reviews.
data(reviews)

# Philosopher's Stone
# Chamber of Secrets.
# Prisoner of Azkaban
# Goblet of Fire
#harry_potter_isbn <- c(
#"043920352X", 
#"0439064864", 
#"0439136350",
#"0439139597") 
#reviews <- reviews[reviews$asin %in% isbns,]

#num_book_reviews <- table(reviews$asin)
#reviews <- reviews[reviews$asin %in% names(num_book_reviews > 140),]

# Get the review helpfulness.
helpful <- reviews$helpful %>%
  strsplit(",") %>%
  unlist %>%
  as.numeric %>%
  matrix(byrow=TRUE, ncol=2)

# Keep the reviews where at least 100 people 
# characterized the review.
reviews <- reviews[ helpful[,2] >= 100,]
helpful <- helpful[ helpful[,2] >= 100 ,]

# Get the tokens for each of the reviews.
X <- tokenize_words(reviews$reviewText) %>% 
  term_list_to_df %>%
  term_df_to_matrix 

# Get rid of the stopwords.
X <- X[, !(colnames(X) %in% stopwords())]

prop_helpful <- drop(helpful[,1] / helpful[,2])

ggplot(as_tibble(prop_helpful), aes(x=value)) +
  geom_histogram() + theme_minimal() + xlab("Count") + 
  ylab("Helpfulness")

ggsave("helpfulness.pdf", height = 5, width = 8)

# What's the average proportion of helpfulness?
intercept <- mean(prop_helpful)
print(intercept)

# Scale the helpfulness proportion.
scaled_prop_helpful <- drop(scale(prop_helpful, scale=FALSE))

# Calculate a min and max lambda value and
# create a sequence of penalties.
max_lambda <- max(abs(colSums(scaled_prop_helpful * scale(X)))) / 
  nrow(X)
min_lambda <- 0.1 * max_lambda
lambda <- exp(seq(log(max_lambda), log(min_lambda), 
  length.out=10))

# fit the cross validated elastic net model.
cv_fit <- cv_lenet_screen(X, prop_helpful, lambda)

gfit <- cv.glmnet(X, prop_helpful, lambda=lambda)
max(abs(gfit$glmnet.fit$beta - cv_fit$fit$b))

# Plot the out-of-sample mean square errors.
resid_est <- as_tibble(cv_fit[c("cvm", "cvup", "cvlo", "lambda")]) %>%
  mutate(llambda = log(lambda)) %>%
  arrange(llambda)

ggplot(resid_est, aes(x=llambda, y=cvm, ymin=cvlo, ymax=cvup)) + 
  geom_errorbar() + theme_minimal() + geom_point(aes(color="red")) +
  scale_colour_discrete(guide = FALSE) + ylab("Mean-Squared Error") +
  xlab(expression("log"~(lambda)))

ggsave("lenet.pdf", height = 5, width = 8)

j <- which.min(cv_fit$cvm)
sort(cv_fit$fit$b[,j][cv_fit$fit$b[,j] != 0], decreasing=TRUE)

eta <- rep(0, nrow(X))
g <- binomial()$linkinv(eta)
gprime <- binomial()$mu.eta(eta)
z <- eta + (prop_helpful - g) / gprime
W <- as.vector(gprime^2 / binomial()$variance(g)) / nrow(X)
max_bin_lambda <- max(abs(colSums(z * W * scale(X)))) 
bin_lambda <- max_bin_lambda * (lambda[j] / max_lambda)

bfit <- glenet(X, prop_helpful, bin_lambda, 1)
sort(exp(bfit$b[bfit$b != 0, 1]), decreasing=TRUE)

