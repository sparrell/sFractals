
=====

PastFuture is a directory of code that might be used in future.
It either is still a work in progress or was deprecated but being saved for resurrection.

cmpl.erl is a set of complex math (ie imaginary numbers) routines. 
Useful for certain fractals (that aren't implemented yet)

fractal_server.erl is a gen_server to process fractals that was replaced by cowboy. 
Leaving around in case split the processing from the webserver

compute_fractal_data was replaced by the epocxy in sf_controller.

data_file_svr.erl is for storing data in a file. Might be refactored in sf_controller but leaving around until it is.

databox.erl is for defining the 'box' (x,y values) of the area of the fractal

