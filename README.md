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

Examples
--------------

A number of example escripts to show how it works. Look at readme in example directory for more info

Program Structure
--------------
This is still a work in progress. 
It grew over time so there are multiple methods of creating a fractal image as I tried various ways.

Method A: As shown in examples 01a,02a,03a,04a,...:
- put configuration if a file and read it into a record for use by programs
- simpleFractal:computeFractalData computes the fractal data
- simpleFractal:makePngFromData creates the image

Method B: As shown in examples 06b1,06b2,08b1,08b2:
- similar to MethodA but stores data in a file so it can be read multiple times for different palettes.
- put configuration if a file and read it into a record for use by programs
- simpleFractal:computeFractalDataIntoFile computes the fractal data
- simpleFractal:makePngFromDataFile creates the image

Method C: As shown in examples 01c,02c,08c:
- Refactored MethodA to put routines in fractalHelpers.erl, and structured so computes 1 row of data and then adds that data to png (instead of computing all the data and then starting the png)
- put configuration if a file and read it into a record for use by programs
- fractalHelpers:makePng computes the data by row and creates png as it goes

Method D: As shown in examples 01d,05d,08d:
- fill in - workerpool in separate processes
- similar to method C but it creates a pool of workers to create the rows of data so they can be created in parralell. A server is started to receive the rows of data and to write to png (queing when necessary when they arrive out of order)
- fractalHelpers:makePngUsingPool

Module Relationships
--------------

This needs updating

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
1. explore processes and worker pools for creating data **Done**

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
    

