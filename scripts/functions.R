# decay function
png(filename = 'slides_ol/figures/decay_function.png',
    width = 10, height = 12, units = 'cm', res = 300)

# define distances
d <- 1:100

# define a decay function
decay_func <- function(x) exp(-(0.5 + (4 * x/100)))

# plot decay over distances
plot(d,
     decay_func(d),
     pch = 19,
     xlab = 'd = 1:100',
     ylab = 'exp(-(0.5 + 4 * d/100))')

dev.off()

# aggregate function
png(filename = 'slides_ol/figures/aggregate_functions.png',
    width = 10, height = 12, units = 'cm', res = 300)
# define distances
d <- 1:100

# sample distances to assign genes
set.seed(123)
g <- sample(1:10, 100, replace = TRUE)

# count the number of instances
n <- as.numeric(table(g))

# aggregate the distances for each instance
s <- aggregate(d, list(g), sum)$x

# plot aggregate distances over the number of instances
plot(n,
     s,
     pch = 19,
     xlab = 'table(g)',
     ylab = 'aggregate(d, g, sum)')

dev.off()

# distance_stat
png(filename = 'slides_ol/figures/distance_stat.png',
    width = 10, height = 12, units = 'cm', res = 300)

# define distances
d <- 1:100

# make a random stat variabl
set.seed(12345)
t <- rnorm(200, 0, 5)

# calculate rank product
rp <- rank(abs(c(-d, d))) * rank(abs(t))

# get min and max ranks
pmax <- which(rp == max(rp))
pmin <- which(rp == min(rp))

# plot randam stats over distances
plot(c(-d, d),
     t,
     pch = 19,
     col = 'gray',
     xlab = 'c(-d, d)',
     ylab = 'rnorm(200, 0, 5)')

# highlight the max rank
points(c(-d, d)[pmax],
       t[pmax],
       col = 'blue',
       pch = 19)

text(c(-d, d)[pmax]+10,
       t[pmax] + 1,
       col = 'blue',
       labels = 'Max')

# highlight the min rank
points(c(-d, d)[pmin],
       t[pmin],
       col = 'green',
       pch = 19)

text(c(-d, d)[pmin] + 10,
       t[pmin] + 1,
       col = 'green',
       label = 'Min')

# make quadrant lines
abline(h = 0, col = 'red')
abline(v = 0, col = 'red')

dev.off()

# stat_stat
png(filename = 'slides_ol/figures/stat_stat.png',
    width = 10, height = 12, units = 'cm', res = 300)

# define a second random stat variable
set.seed(321)
t2 <- rnorm(200, 0, 5)

# plot the two random stat variables
plot(t, t2,
     ylim = c(-10, 10),
     xlim = c(-10, 10),
     pch = 19,
     col = 'gray',
     xlab = 'rnorm(200, 0, 5)',
     ylab = 'rnorm(200, 0, 5)')

# highlight four quadrant by sign
text(5,5, '(+, +)', col = 'blue')
text(-5,-5, '(-, -)', col = 'blue')
text(-5,5, '(-, +)', col = 'green')
text(5,-5, '(+, -)', col = 'green')

# make quadrant lines
abline(h = 0, col = 'red')
abline(v = 0, col = 'red')

dev.off()

# ecdf
png(filename = 'slides_ol/figures/ecdf.png',
    width = 10, height = 12, units = 'cm', res = 300)

# make a random stat variabl
set.seed(12345)
t <- rnorm(200, 0, 5)

# calculate the cumultive distribution function
ecdf_fun <- ecdf(t)

# plot ecdf
plot(ecdf_fun,
     main = '',
     xlab = 'rnorm(200, 0, 5)',
     ylab = 'ECDF')

dev.off()
