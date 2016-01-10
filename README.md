
=====

An OTP application to make and return fractals.
This is more an exercise to learn erlang, OTP, rebar,
common test,  git, github but hopefuly a useful program comes drom it

Build
-----

    $ rebar3 compile


Build and Test
--------------

    $ rebar3 do compile, ct

Examples
--------------

A number of example escripts to show how it works. Look at readme in example directory for more info

Program Structure
--------------
This is still a work in progress.

Dependencies:
- rebar3 is used to compile & build this application
- png is used for creating png image
- epocxy is used to distribute the fractal processing across many processes
- cowboy is used as the web&API server
- jiffy is used for parsing user input into json
- lager is used for logging
- eper & recon are used for debugging
- shotgun is used for test
- relx is used for creating releases
- worker_pool_ is deprecated in this application



Quick overview of files:
- for rebar3 make/build/install
   - rebar.config => for rebar3 to make/build/install application
- for erlang OTP application
   - src/sFractals.app.src
   - src/sFractals_app.erl
   - src/sFractals_sup.erl
- Cowboy web and REST API server
   - the sFractals application starts a cowboy webserver and routes:
      - static to index.hmtl
      - /status to status_handler.erl
      - /sFractal to fractal_handler.erl
- Fractal Creation
   - fractal_handler.erl is called by cowboy when a post is made to /sFractal
   - fractal_handler.erl
      - uses routines in config_utils convert html to JSON and validate input
      - calls sf_controller:make_data to make the fractal
   - config_utils.erl is called by fractal_handler.erl and any other routines that need config input validation. Uses jiffy for json conversions
   - sf_controller.erl controls the flow of making a fractal.
      - It creates an ets table to hold the fractal data,
      - uses epocxy to spawn one process per row of computation.
      - Calls compute_points:compute_iteration_value to do the actual fractal computation for each point.
      - Calls make_png:make_png_from_data to create the image from the data
   - compute_points.erl contains actual fractal computation algorithms
   - make_png.erl converts fractal data into image using png and imagelib.erl
   - imagelib.erl has routines for converts fractal data into image using png
- Misc
   - status_handler.erl is called by Cowboy on /status and returns status about the application (still a work in process)
   - cplx.erl - routines for complex (ie imaginary number) math (not used yet)
   - data_file_svr.erl - not used yet. hope to make a storage server to save recomputation


#Use Cases
--------------

The use case of creating this fractal:
- put pic here
consists of:
- user inputting configuration parameters
- creating fractal data based on config parameters
- mapping fractal data into colors and creating png

## Inputting Config
--------------
Starting the sFractals app

## Creating Fractal Data
--------------


## Colors and Png
--------------


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
1. modify current code base into proper otp and supervision trees **Done**
1. add in cowboy webserver **Done**
1. add in input form, web handler, restful api between them? **Done**
1. creation of config data to feed data generation **Done**

...

Next steps:

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
* config data for user to save for re-creating

...

endstate:
* webserver with page that accepts inputs and then creates image
* data generation server and image render servers
* supervision trees


