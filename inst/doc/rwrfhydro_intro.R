#' ---
#' title: rwrfhydro introduction
#' author: James McCreight, Aubrey Dugger, Logan Karsten
#' ---
#' 
#' 
#' #### What is rwrfhydro?
#' A community-contributed tool box for managing, analyzing, and visualizing WRF Hydro input and output files in R (everything but the model and the GIS).
#' 
#' Intentionally, “rwrfhydro” can be read as “our wrf hydro”. The purpose of this R package is to focus **community development** of tools for working with and analyzing data related to the WRF Hydro model. These tools are both free and open-source, just like R, which should help make them accessible and popular. 
#' 
#' The git hub repository is found here: [https://github.com/mccreigh/rwrfhydro/](https://github.com/mccreigh/rwrfhydro/).
#' 
#' The [README.Rmd](https://github.com/mccreigh/rwrfhydro/blob/master/README.Rmd) provides instructions on installing, using, and contributing. Additional resouces for learning R are listed there as well.
#' 
#' 
#' # Purpose
#' This vignette 
#' 1) Provides an a list of current vignettes,
#' 2) Covers basic R installation and usage. 
#' 
#' The ultimate goal is endocterination so that you will help contribute to the rwrfhydro source code.
#' 
#' # Vignettes
#' ----------------------------------- -------------------------------------------------------------------------------
#' R Basics                            [ [html](#RBasics) - [R](rwrfhydro_intro.R) ]
#' 
#' rwrfhydro overview                  [ [html](overview.html) - [R](overview.R) ]
#' 
#' Domain visualization                [ [html](domainChannelVis.html) - [R](domainChannelVis.R) ]
#' 
#' Streamflow evaluation               [ [html](streamflowEval.html) - [R](streamflowEval.R) ]
#' 
#' Water balance                       [  [html](waterBudget.html) - [R](waterBudget.R) ]
#' 
#' GetMultiNcdf                        [ [html](getMultiNetcdf.html) - [R](getMultiNetcdf.R) ]
#' 
#' ET                                  [ [html](evapotranspirationEval.html) - [R](evapotranspirationEval.R) ]
#' 
#' **-The following require optional**  **libraries or compiled code.-** 
#' 
#' Regridding (devCompile branch)      [ [html](foo.html) - [R](foo.R) ]
#' 
#' Geospatial Tools (rgdal)            [ [html](geoSpatialTools.html) - [R](geospatialTools.R) ]
#' 
#' Precipitation evalation (rgdal)     [ [html](precipitationEvaluation.html) - [R](precipitationEvaluation.R) ]
#' 
#' MODIS (modis)                       [ [html](modisProcessing.html) - [R](modisProcessing.R) ]
#' 
#' ----------------------------------- -------------------------------------------------------------------------------
#' 
#' 
#' *** 
#' <a id="Install"></a>
#' 
#' # Installation Tips
#' The [installation instructions](https://github.com/mccreigh/rwrfhydro/blob/master/README.Rmd#installing) require a minor detour for 2 reasons: 1) ncdf4 in a non-standard location, 2) there is no local R library yet. First we'll install `devtools`.
#' 
#' 
#' In the terminal:
## ---- eval=FALSE, engine="bash"------------------------------------------
## R
## install.packages("devtools", repos="http://cran.at.r-project.org/")
## q()

#' On a clean R install, the above will ask the user to create a new, writable Library path. This will be the first in the R_LIBS path (in R: `.libPath()`).
#' 
#' 
#' Next, we'll return to the shell and install the `ncdf4` package against the correct-for-R (gcc) build on the system. 
#' 
#' For Windows, users can download the appropriate binary ncdf4 file from the following link: [http://cirrus.ucsd.edu/~pierce/ncdf/](http://cirrus.ucsd.edu/~pierce/ncdf/). Then the following command, it will open a window for you to choose the downloaded zip file and will install it:
## ---- eval=FALSE, engine="bash"------------------------------------------
## install.packages(file.choose(), repos=NULL, type = "binary")

#' 
#' For linux and OSX, in the terminal:
## ---- eval=FALSE, engine="bash"------------------------------------------
## wget http://cran.r-project.org/src/contrib/ncdf4_1.13.tar.gz
## R CMD INSTALL \\
## --configure-args="--with-nc-config=/usr/local/netcdf-4.3.2-gcc/bin/nc-config" ncdf4_1.13.tar.gz

#' The path to the ncdf4 source can be found by googling "cran ncdf4" and using the information at the package's home page. The config-args are specific to the machines in the lab. Note that if you wish to also install the `rgdal` package, you'll also have to install gdal and proj4 and several other pieces of external software on your machine and then build `rgdal` against their locations in a similar way.
#' 
#' Now we will install `rwrfhydro`. The remainder of its dependencies install without issue. Again in the terminal:
## ---- eval=FALSE, engine="bash"------------------------------------------
## R
## library(ncdf4)  ## check if the install was successful.
## devtools::install_github('mccreigh/rwrfhydro')  ## one time and for updates.
## library(rwrfhydro) ## for each new R session where you want to use rwrfhydro.s

#' 
#' Several of the `rwrfhydro` vignettes depend on external testcase data to run. This data is external because WRF-Hydro output is voluminous! These test cases will be available separately on our website at some time inthe near future. They should be downloaded and unzipped in a folder `~/wrfHydroTestCases/`, or the unzipped tar balls can be symlinked into this directory. For the examples included here we will need both `~/wrfHydroTestCases/Fourmile_Creek` and `~/wrfHydroTestCases/usgsDB`. The `usgsDB` directory is included in the class tar ball and can be symlinked by `cd ~/wrfHydroTestCases ; ln -s ~/pathTo/rwrfhydro_class/usgsDB .`.
#' 
#' ***
#' <a id="RBasics"></a>
#' 
#' # R Basics
#' 
#' 
#' ## Table of Contents
#' * [High-level View](#HighLevelView)
#' * [Setup](#Setup)
#' * [Functions](#Functions)
#' * [Lists](#Lists)
#' * [Data Frames](#DataFrames)
#' * [Serious List Example](#SeriousListEx)
#' * [Methods & Classes](#MethodsClasses)
#' * [Scoping](#Scoping)
#' * [Getting Help](#Help)
#' * [Packages & Namespaces](#PackagesNamespaces)
#' * [Bonus: Computing on the language](#COL)
#' 
#' <a id="HighLevelView"></a>
#' 
#' ## High-level View
#' The goal of this overview is to **demistify** R and help new users feel less confused when using R. The examples here are not deep at all (don't believe that they are) but they give a view into some basic ways that R might be different than whatever languages you are used to. Once you absorb this, you can graduate to more sophisticated resources like [Advanced R](http://adv-r.had.co.nz/) or [R Packages](http://r-pkgs.had.co.nz/) with a solid foundation. Please see the rwrfhydro [home page](https://github.com/mccreigh/rwrfhydro/blob/master/README.Rmd) for more references and the [R cheat sheet](https://nex.nasa.gov/nex/static/media/other/R-refcard_2.pdf) for a broad list of functionality.
#' 
#' One note is that R was developed as an open source project which did not apparently impose many coding style standards at the outset. Hence, there is really no standard code style (e.g. `function.name` or `function_name` or `functionName` or `FunctionName` or `functionname` might all be used for different functions). Dont read any other meaning into this. In contrast, we adopt Google's [coding style for `rwrfhydro` development](https://github.com/mccreigh/rwrfhydro/blob/master/README.Rmd#r-code-style) with one modification. So, style is meaningful when you are reading `rwrfhydro` code and using it's objects (functions and otherwise).
#' 
#' If you are used to including a library of functions in your path in another language, this notion is replaced by the "package" in R. This is a somewhat advanced way to use R but it is well worth learning the fundamentals. Contributing to `rwfhydro` can help learn some parts of package development, though we handle most of these. Notably, R possesses advanced and powerful documentation and markup features which are worth learning. Note that most of the course materials documents are provided in three formats generated from source rmarkdown (.Rmd) documents: .html are generated by **running** the code and the .R files turn the text into comments, leaving the code and not running it.
#' 
#' None of this is to mention the availablilty of packages and interfaces to other open-source software which extend the power of the R language.
#' The [Comprehensive R Archive Network (CRAN)](http://cran.r-project.org/) is the (distributed) nexus for both the R software and community contributed packages which meet the CRAN standards. There are currently over 6500 contributed packages! It is useful to look at the homepage of packages on CRAN, e.g. that for the [`plyr` package](http://cran.r-project.org/web/packages/plyr/index.html). Though `rwfhydro` is not on CRAN, we emulate this package overview at the top of our [README](https://github.com/mccreigh/rwrfhydro/blob/master/README.Rmd). `Imports` describes which packages are required to run, `Suggests` describes optional packages for extended functionality. The `Bug Reports` URL may be the most important piece of information on the page. In the downloads section, installation is described, a pdf manual is offered, and the vignettes are listed. The pdf manual is simply a compilation of the help available within R. The vignettes provide **working** examples with annotation which also show the output and make the example self-contained.
#' 
#' 
#' <a id="Setup"></a>
#' 
#' ## Setup
#' 
#' While intefacing with R on command line is an option, using a more integrated editor will help you be more efficient. [Rstudio](http://www.rstudio.com/) provides a very good and free IDE (Integraged Development Environment) for R. The only competitor is [ESS for Emacs](http://ess.r-project.org/). Both are highly recommended with RStudio having several advantages, particularly for package development. 
#' 
#' 
#' When running R, you might benefit from setting `options(warn=2)` which turns all warnings into errors. It's useful to stop execution and discover unintended mis-use of R as it starts (great for those new to R). Debugging in R can be turned on by `options(error=utils::recover)`, for example. Debugging is a more advanced usage but good to be aware of, see [this resource](http://www.biostat.jhsph.edu/~rpeng/docs/R-debug-tools.pdf) for more information.
#' 
#' My "startup" file contains both of the above suggestions. (In fact, these are the only things which remain of all the various things I've tried to date.) Here's how that startup file is configured by use of the ~/.Renviron file.
## ---- eval=FALSE, engine="bash"------------------------------------------
## james@orographic:~> more .Renviron
## R_LIBS=~/R/Libraries/R3.2/
## R_PROFILE=/Users/james/R/startup_jlm.r
## 
## james@orographic:~> more /Users/james/R/startup_jlm.r
## options(warn=1)
## options(error=utils::recover)

#' 
#' The first line of .Renviron specifies the R libraries path (`R_LIBS`), which is unix-like in searching for read and write. The second line specifies the `R_PROFILE` or startup file location. The contents of the `R_PROFILE` are R commands and my only commands are the `options()` mentioned above. 
#' 
#' <a id="Functions"></a>
#' 
#' ## Functions
#' A basic R function looks like this:
## ------------------------------------------------------------------------
BasicFunc <- function(x) {   ## define the function 
  y=x^2                      ## square the argument
  y                          ## return the square
}
xx <- 3
yy <- BasicFunc(xx)
print(yy)
yy

#' Note that the last line in the function is the return value. Also, the print method is invoked on an object when that object is entered by itself, as in the very last line. 
#' 
#' Remember that nearly everything in R is a function object. Indeed, functions are "first-class". A more complicated example illustrates passing a named function to another function. (In the [scoping](#Scoping) section below, we illustrate a function returning a function, which is then called a closure).
## ------------------------------------------------------------------------
## Note named arguments. Args can be passed by name or position.
Function1 <- function(x=x,f=f) list(arg=x, result=f(x), func=f)
result1 <- Function1(xx, BasicFunc)
result2 <- Function1(f=BasicFunc, x=xx)
identical(result1,result2)
result1
str(result1)

#' The result of this function is not a scalar or vector, it is a `list` object which contains a possibly arbitrary collection of things. The `list` object is the fundamental way to get more than a single object out of a function. The `str()` function reveals the structure of objects, including `lists`. It helps structure complicated, hierarchical items as we'll see later. It also provides object type and class information.
#' 
#' <a id="Lists"></a>
#' 
#' ## Lists
#' Above we saw that `lists` are used to return multiple items from a function. Then, how do we get the contents out of a returned list? For simple lists, this can be done, by hand. Note that `str()` is wrapping many of the commands below to give a more informative view what the output actually is.
## ------------------------------------------------------------------------
names(result1)          ## returns a character vector, hence the [1]
str(result1['arg'])     ## returns a sub-list
str(result1[['arg']])   ## returns the item requested, a numeric scalar 
str(result1$arg)        ## dollar acts like the "[[" function.
str(result1$func(2))    ## function evaluation.
result1[['func']](2)    ## function evaluation, same as previous.
## The easiest way to rename is by name using plyr::rename
renames <- paste0('names.',c("b","(c)"))  ## the new names are the vector entries
names(renames) <- c('result','func')      ## the old names are the names of the new names
## This kind of translation vector is very handy, note that
## value=rename[name] and name=names(rename)[which(rename == value)]
str(renames)
print(renames)  ## the top line is the names
result1 <- plyr::rename(result1, renames)
str(result1)
result1$names.b
result1$`names.(c)`(2)   ## backquote can handle illegal names 

#' 
#' This demonstrates a basic principle in R that you can forget about indices and **call it by name**. NO indices were used above, the number two is a value passed to a function.
#' 
#' <a id="DataFrames"></a>
#' 
#' ## Data frames (regular lists)
#' Before showing complicated lists, lets look at data frames. Data frames are special kinds of lists that are "regular". 
## ------------------------------------------------------------------------
mtcars             ## data packaged with R typically comes in data frames.
str(mtcars)         
rownames(mtcars)
colnames(mtcars)
?mtcars
## subset is on rows
ecoCars<-subset(mtcars, mpg > 25)  ## non-standard evaluation of the column name
ecoCars      
ecoCars$hp    ## $ gives columns by name
mtcars[mtcars$wt<3,c('wt','mpg','cyl','disp')]  ## rows and cols can be referenced 
mtcars$names <- rownames(mtcars)           ## new col with names, mixed types in df
mtcars$names <- NULL                       ## remove a column

#' The regular collation of data allows many special operations to be performed on data frames to summarize, subset, and etc the data. We dont have time to cover this important aspect directly. 
#' 
#' <a id="SeriousListEx"></a>
#' 
#' ## Serious list example
#' One example of a very complicated list is all the netcdf meta data. 
## ------------------------------------------------------------------------
ncFile <- '~/wrfHydroTestCases/Fourmile_Creek/RUN.RTTESTS/OUTPUT_CHRT_DAILY/2013051600.LDASOUT_DOMAIN1'
library(ncdf4)
ncid <- nc_open(ncFile)

#' 
#' (Click on the color-boxed section below to skip to its end.)
#' <br>
#' <div style="border:2px solid; border-color:blue; border-radius: 25px; padding: 12px 25px;">
#' <a href="#endofblue" style="text-decoration: none;">
## ------------------------------------------------------------------------
ncid

#' </a>
#' <a id="endofblue"></a>
#' </div>
#' 
#' <br>
#' 
## ------------------------------------------------------------------------
nc_close(ncid)

#' 
#' Note that the print method for `ncid` (which has `class(ncid)=='ncdf4'`) does something like an ncdump - but you almost need to stand on your head. In `rwrfhydro` we modified this to look more like what you expect if you use ncdump on command line. 
#' 
#' (Again, click on the color-boxed section below to skip to its end.)
#' <br>
#' <div style="border:2px solid; border-color:green; border-radius: 25px; padding: 12px 25px;">
#' <a href="#endofgreen" style="text-decoration: none;">
## ------------------------------------------------------------------------
rwrfhydro::ncdump(ncFile)  ## something more visually akin to unix ncdump

#' </a>
#' <a id="endofgreen"></a>
#' </div>
#' <br>
#' 
#' Detailed inspection of the ncid object makes it obvious that automating information extraction is going to be key.
#' 
#' 
#' (Again, click on the color-boxed section below to skip to its end.)
#' <br>
#' <div style="border:2px solid; border-color:red; border-radius: 25px; padding: 12px 25px;">
#' <a href="#endofred" style="text-decoration: none;">
## ------------------------------------------------------------------------
str(ncid)

#' </a>
#' <a id="endofred"></a>
#' </div>
#' <br>
#' 
#' Whoa!! How in the world would you, for example, get the dimensions of each variable?
## ------------------------------------------------------------------------
## drill down by name into the object, var looks like it describes variables.
names(ncid) 

## what are the objects named contained in ncid$var?
names(ncid$var)  

## each variables has these named objects
names(ncid$var$SOIL_M)  

## NamedList simply names a list named by its entries
varList <- rwrfhydro::NamedList(names(ncid$var))  
varList   

## Pass all the variable names to the function which then
## looks up the size information for that variable.
str(plyr::llply(varList, function(vv) ncid$var[[vv]]$size )) 

## or if you want the units
str(plyr::llply(varList, function(vv) ncid$var[[vv]]$units )) 

#' In the above, the `llply` function from the `plyr` package applies a function (anonymously specified in-line) to a list (`varList`) and returns a list. The characters in `VarList` are passed to the function as `vv` and these characters are used to select the variable from `ncid$var`. Note that the names on the returned list are the *names on the list which were iterated over*. To illustrate this, 
#' 
## ------------------------------------------------------------------------
names(varList) <- NULL
str(plyr::llply(varList, function(vv) ncid$var[[vv]]$size )) 

#' 
#' That's fairly useless. So it's important to understand that the result is collated against the name of the input list and the value of that list object is what is passed to the function. They just happen to be identical in this case.
#' 
#' <a id="MethodsClasses"></a>
#' 
#' ## Methods and classes
#' It's also good to have a basic awareness of methods and classes in R, this can be particularly mystifying to new users. This is how the same generic function can appear to give a variety of different behaviors, these are methods conditioned on the class of the input. It's also worth noting that the common S3 object system used in R is very "lightweight" (e.g. see setting the class below) and easy to use. For those who shudder at such an informal system there are [other object approaches in R](http://adv-r.had.co.nz/OO-essentials.html).
#' 
## ------------------------------------------------------------------------
class(mtcars)
print(head(mtcars))
print.data.frame(head(mtcars))
print.default(head(mtcars))

print.foo <- function(x){
  print(names(x))
  print('foo!!!!!!!!!!!')  ## really not helpful!
}
class(mtcars) <- append('foo',class(mtcars))
print(mtcars)

#' <a id="Scoping"></a>
#' 
#' ## Scoping
#' Scoping: what variables are available where? R has **Lexical scoping**: variables in enclosing environments/functions are available to a given environment/function. For assignment: when assigning to a name the name is searched for in enclosing functions until it is found, if not found in the global environment it is created there. Lastly, functions can be returned with their own environment, these are called closures. Here are some examples. 
#' 
#' The variable `a` is in the global environment and is found by the function `f`
#' because `f` is enclosed in the global environment.
## ------------------------------------------------------------------------
a <- 10  
f <- function(x) a*x   
f(2)

#' 
#' Now `g` contains a variable `b` which `f2` cannot find because `g` does not enclose `f2`. I wrap failing evaluations in `print(try())` so that this document compiles. Note that `try()` is a very vaulable function for fault tolerance. Also, examination of `try()` shows that it returns it's agument wrapped in `invisible()`, to see that object `print()` has to be explicitly called on `try()`.
## ------------------------------------------------------------------------
options(warn=1)
g <- function(x) {b <- 10; 1/x}   
f2 <- function(x)  b*x 
print(try(f2(.1)))

## the following lexical assignment is frowned upon by most guRus.
g2 <- function(x) {b <<- 100; 1/x}   
print(try(b)) # whoa, isnt it supposed to be assigned?
g2(2)
b # R is lazy, so b isnt assigned until the function is called.
f2(.1)

#' 
#' A closure is a function with data, that data is arbitrary. 
## ------------------------------------------------------------------------
fOuter <- function(x) {qqq <- 2*x; function() qqq }
fInner <- fOuter(4)
fInner()
print(try(qqq))
get('qqq',envir = environment(fInner))

#' 
#' Or maybe more informatively:
## ------------------------------------------------------------------------
vvv <- 123
gOuter <- function(x) {
  ttt <- 2*x
  junk <- rwrfhydro::NamedList(letters[1:4])
  function(getVar) get(getVar) 
}
gInner <- gOuter(4)
print(try(ttt))
gInner('junk')
gOuter(c(1,2,4))('ttt')
gInner('vvv')

#' 
#' The caveat emptor with scoping is that, while functions which reference variables in enclosing envrionments are plug-and-play, unintended changes to variables in enclosing environments are vunerabilities/liabilities to function accuracy! Be careful and declare all variables in a function when saftey is needed.
#' 
#' <a id="Help"></a>
#' 
#' ## Getting help
#' Getting information on functions is key. Getting the source code of a function is as easy as calling a function without parentheses (i.e., invoking the print method on the function!) and reading functions is a great way to pickup R programming tips:
## ------------------------------------------------------------------------
lm

#' Beyond reading the body of the text, note that the arguments are listed at the top with default values following `=`. The `...` argument is for passing arguments to "low-level" regression functions (described in `?lm`). We also see that the function belongs to the `stats` namespace. 
#' 
#' See also (not run here):
## ---- eval=FALSE---------------------------------------------------------
## formals(lm)
## ?lm
## ?'%in%'
## ?'['
## ## the following are help on help
## ?help
## ?`?`
## ?`??`

#' 
#' <a id="PackagesNamespaces"></a>
#' 
#' ## Packages and namespaces
#' R packages are like toolboxes for specific purposes.Namespaces for packages make their functions available and managing namespaces is how one can open an entire toolbox or just pull a specific tool out of a specific toolbox. Namespaces can be made entirely available by attaching a package using `library()`, e.g. the whole toolbox is available in the global environment:
## ------------------------------------------------------------------------
sessionInfo()
library(rwrfhydro)  ## this adds rwrfhydro to the 'other attached packages' list
sessionInfo()
PlotFdc  ## this function is available in the global environment.

#' Now all the functions in the rwrfhydro namespace are available because it is attached. 
#' 
#' Now we grab a specific tool (`melt`) from the "reshape2" package, but this dosent make the whole toolbox available, though we see it's namespace is 
## ------------------------------------------------------------------------
mtcars$model <- rownames(mtcars)
head(reshape2::melt(mtcars[,c("mpg","cyl","disp","model")], id='model')) ## use a function in plyr without attaching it
sessionInfo() ## attaches plyr, reshape2, Rcpp,  & stringr (some already attached.)
print(try(melt))  ## since plyr is not attached, the melt function is not available.

#' 
#' The namespace of a package is the set of exported objects. Internal (non-exported objects) in a package can be accessed via `:::` though this is often frowned upon.
#' 
#' <a id="COL"></a>
#' 
#' ## Bonus: Computing on the language
#' R is an extremely flexible language. The non-standard evaluation mechanisms allow for a variety of powerful behaviours. These can be confusing to new users. Here's one simple example showing a few useful functions.
## ------------------------------------------------------------------------
assign('one',1)
two <- eval(parse(text='one+one'))
get('two')

