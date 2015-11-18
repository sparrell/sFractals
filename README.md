sFractals
=====

An OTP application to make and return fractals. 
This is more an exercise to learn erlang, OTP, rebar,
common test,  git, github but hopefuly a useful program comes drom it

Build
-----

    $ rebar3 compile


Build and Test
--------------

    $ rebar3 ct

Module Relationships
--------------

![modules] (https://raw.githubusercontent.com/sparrell/sFractals/data-file-server/src/simpleFractalModules.jpg)


Plan of Attack
--------------

1. set up app structure using rebar3 **Done**
1. setup git and github and use github desktop **Done**
1. add complex number library **Done**
1. first order learn 'common test' and add tests for complex number math **done**
1. function to render color data into image **Done**
1. make data for simple fractal **Done** (and refactored several times)
1. function to colorize iter data **Done**

...
not sure of order of rest of these
* function to generate math data
  + initially simple (mandelbrot? julia?) "fixed" function
+ generalize passing potentially different functions to common core
  + add in a bunch of functions (incl Dynamical Plane and Parameter Plane)
* function to generate color data from math data
* turn functions into servers and establish work flow
* supervision tree
  + put graphic of tree here
* webserver
* html form for input on webserver
* creation of config data
  + to feed data generation
  + for user to save for re-creating

...

endstate:
* webserver with page that accepts inputs and then creates image
* data generation server and image render servers
* supervision trees
    
