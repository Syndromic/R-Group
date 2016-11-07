

###########################################################################################################################
###
### Advanced R topics
### 
### 1. Data types
### 2. Functions


###########################################################################################################################
###
### 1. Datatypes
### 
###
### vectors, matrices, arrays
### lists
### data.frames
###

### vectors, matrices, arrays must all be of same type (all characters or all numeric etc)

set.seed(42)

# vector example
rbinom(100,1,0.5)

# matrix example
matrix(rbinom(100,1,0.5), ncol = 10)

# array example
array(rbinom(100,1,0.5), dim = c(10,5,2))

### Note: R auto collapses dimensions
### e.g.
test<- array(rbinom(100,1,0.5), dim = c(10,5,2))
test[,1,]
class(test[,1,])    # matrix, not array!

# can prevent this using 'drop'
test[,1, ,drop  = FALSE] ## array


#### lists

# lists are collections of arbitrary objects

list1<- list(a = "a", 
             b = 1:5)
             
list2<- list(list1 = list1, rnorm(5))

# Useful for returning complex results fro functions
     
#### data frames

# like a list where each item is a vector and has the same length


df1<- data.frame(a = "a", 
                 b = 1:5)

         
df2<- data.frame(df1 = df1, rnorm(5))

class(df1[,"a"])

# Note, data.frame auto converts strings to factors :( 
#  use "stringsAsFactors = FALSE" to stop


df1<- data.frame(a = "a", 
                 b = 1:5, 
                 stringsAsFactors = FALSE)

class(df1[,"a"])

###########################################################################################################################
###
### 2. Functions
### 
### a. Writing a function example
### b. Using elipses "..."
### c. Using functions as parameters
### d. Anonymous functions
### e. R class system(s)
### f. Functions that make functions
### g. Functions as first class objects
###


############
###
### a. Writing a function example
###

 scanin<- scan(file = "Data\\Session22 output.txt", 
			sep = "\n", 
			what = character(),
			na.string = NULL)
			
plot(scanin)
# R does not know what to do with this


### Create a function to produce a plot of the results of using scan



plot.scan<- function(x)
{
	nchars<- nchar(x)
	nlines<- length(x)
	plot(c(0, max(nchars)), c(1, nlines), type = 'n', axes = "FALSE", xlab = '', ylab= '')
	for (i in 1:nlines)
		for (j in 1:nchar(x[i]))
			rect(j, nlines - i - 1, j+1, nlines - i,
			     lty = 0.5, 
			     col = c('black', 'lightgrey')[1 + (substring(x[i], j, j) == " ")])
}

plot.scan(scanin)



############
###
### b. Using elipses "..."
###


### What if we want to allow the user to add in axes labels?
### Could try:

plot.scan<- function(x, xlab = '', ylab = '')
{
	nchars<- nchar(x)
	nlines<- length(x)
	plot(c(0, max(nchars)), c(1, nlines), type = 'n', axes = "FALSE", xlab = xlab, ylab= ylab)
	for (i in 1:nlines)
		for (j in 1:nchar(x[i]))
			rect(j, nlines - i - 1, j+1, nlines - i,
			     lty = 0.5, 
			      col = c('black', 'lightgrey')[1 + (substring(x[i], j, j) == " ")])
}

plot.scan(scanin, xlab = 'column', ylab = 'row')

### However it would be very tedious to add in every possible parameter to plot as an option
### Instead can use elipses

plot.scan<- function(x,  ...)
{
	nchars<- nchar(x)
	nlines<- length(x)
	plot(c(0, max(nchars)), c(1, nlines), axes = FALSE, type = 'n',  ...)
	for (i in 1:nlines)
		for (j in 1:nchar(x[i]))
			rect(j, nlines - i - 1, j+1, nlines - i,
			     lty = 0.5, 
			      col = c('black', 'lightgrey')[1 + (substring(x[i], j, j) == " ")])
}

plot.scan(scanin, xlab = 'column', ylab = 'row', main = 'My Scan')


############
###
### c. Using functions as parameters
###
### Functions can be parameters to other functions
### For example, in the 'apply' and 'aggregate' functions


mat<- matrix(rnorm(100), ncol = 10)

# row means
apply(mat, 1, mean)
sillymean<- function(x) mean(sin(x))

apply(mat, 1, sillymean)


# shinyServer(function(input, output) {
#  .
#  .
#  .
#})

# what does this do?

library(shiny)
shiny:::shinyServer



############
###
### d. Anonyous functions
###
### Functions do not have to have names!

function(x) x+1 

(function(x) x+1 )(1)

# This is called anonymous functions, and can be used anywhere a named function is used

apply(mat, 1, function(x) mean(sin(x)))


############
###
### e. R class system(s)
###
### R has two main systems of class dispatching
### S3 and S4
### S3 is simpler but less powerful
### S4 is more complicated and more powerful

# S3 example
#
# How doess the plot function work?

dat<- data.frame( x1 = rnorm(100),
		  x2 = rnorm(100))
dat$y<- dat$x1 + rnorm(100)/2

# create a simple scatter plot
plot(dat$x1, dat$y)

# run a regression	  
m1<- lm( y ~ x1 + x2, data = dat)

# show a diagnostic plot
plot(m1)

# how did R know to produce a diagnostic plot of m1 when plot was called?
# Look at plot function

plot

# "UseMethod" tells R that plot is a generic function
# Decides which plot function to use based on class of object

class(m1)

stats:::plot.lm
# When plot function is called with an object of class "lm", R knows to call plot.lm
# ":::" forces R to show the function if it is in the "stats" namespace

# If no method for class exists, uses plot.default
plot.default


### We can make our own version of plot for the results of a scan

 scanin<- scan(file = "Data\\Session22 output.txt", 
			sep = "\n", 
			what = character(),
			na.string = NULL)
			
class(scanin)
class(scanin)<- c(class(scanin), "scan")
class(scanin)

plot(scanin)

# plot.scan uses the S3 method for classes; When plot(scanin) is called, R looks first
# for a function plot.scan since this is the right most item in class(scanin)

# S4 dispatch system is much more complicated (I've never needed to use it)


############
###
### f. Functions that make functions
###

# Function to create a function that sets any values outside
# of a range to NA, where the range is specified by another vector when
# the function is created

createfun<- function(x)
{
	xrange<- range(x)
	function(y)
	{
		low<- y < xrange[1]
		high<- y > xrange[2]
		y[low | high]<- NA
		y
	}
}

truncfun<- createfun(runif(100))
truncfun
truncfun(rnorm(100))

# Note that xrange is not (easily) visible to us
xrange
eval(quote(xrange), envir =  environment(truncfun))

# The scope for funding variables in R is different than for most non functional languages

xrange<- c(0.4, 0.5)
truncfun(rnorm(100))
# still using old xrange
# Scope goes to environment that a function was created in, not the environment that a function was called from

# This can lead to interesting functions:




make_counter<- function()
{
	current<- 0
	function()
	{
		current<<- current + 1
		current
	}
}

test<- make_counter()
test
test()
test()
test()
test()

test2<- make_counter()
test2()
test()







############
###
### g. Functions as first class objects
### R is a functional language - this just means that R treats functions as just another object type
###
### This also means you can do things with functions that you cannot do with non functional languages


testfun<- function(x) x+1
testlist<- as.list(testfun)
testlist
test2fun<- as.function(testlist)(1)
test2fun

### Statements in R are also objects

class(testlist[[2]])
testcall<- as.list(testlist[[2]])
testcall

as.call(testcall)

x<- 1
eval(as.call(testcall))



