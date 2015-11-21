sFractals examples dir
=====

The "examples" directory contains several example erlang scripts to create some simple fractals. The differences between the examples (eg resoultion, time to run,...) are explaing below.

Times
-----

For calibration purposes, 
here are the unscientifically measured run times for the various examples 
(eg so you'll not freak if when example 7 takes much longer.

	Example 1 = 1s
	Example 2 = 13s
	Example 3 = 25s
	Example 4 = 27s
	Example 5 = 25s
	Example 6 = 1s
	Example 6b = 1s
	Example 7 = 67s
	Example 7b = 29s
	Example 8 = wip
	Example 9 = wip

Example 1
-----

A really simple Julian fractal. The configuration is in example1.ecfg. 
It is only 10 pixels by 10 pixels with a depth of 12.
The x/y range (ie range of Z in the Julian Z-squared+C equation) is set wide enough to catch the entire Julian region.

Here is the image - note it's pretty small so you may need to magnify:

![modules] (https://raw.githubusercontent.com/sparrell/sFractals/master/examples/example1.png)


Example 2
--------------

The same x/y range as example1 with the same depth of 12, but with a finer resolution of 4Kx4K.

![modules] (https://raw.githubusercontent.com/sparrell/sFractals/master/examples/example2.png)

Example 3
-----

The same resolution (4kx4k) as example2 with the same depth of 12, but with a smaller range of Z. 
The x axis is the middle third of example 2 and the y axis is also one third but starting at the middle (ie 0-2 whereas example 2 was -3to+3).

![modules] (https://raw.githubusercontent.com/sparrell/sFractals/master/examples/example3.png)

Example 4
--------------

Example 4 has the same parameters as example3 except it has a depth of 16 instead of 12.
Notice the black (reached iteration bound) of example3 fills in with more detail in example4.

![modules] (https://raw.githubusercontent.com/sparrell/sFractals/master/examples/example4.png)

Example 5
-----

Example 5 has the same parameters as example4 except it has a depth of 32 instead of 16.

![modules] (https://raw.githubusercontent.com/sparrell/sFractals/master/examples/example5.png)

Example 6
--------------

Example 6 is the same as example 1 but uses different code that first stores the fractal data in a file.
This allows for different color maps without having to recreate all the fractal data.

![modules] (https://raw.githubusercontent.com/sparrell/sFractals/master/examples/example6.png)

Example 6b
--------------

Example 6b uses the data from example 6 but a differnt color map (same depth, just different colors).
If it was large with hi-resolution then it would save significant time.
In this trivial example it runs too fast to notice the time savings.

![modules] (https://raw.githubusercontent.com/sparrell/sFractals/master/examples/example6b.png)

Example 7
-----

Example 7 is like example 6 in that it writes out the data to file and then reads the file. 
Example 7 uses the same parameters as example 5.
Note Example 7 takes over twice as long as example5 (but it is creating a 31M data file).

![modules] (https://raw.githubusercontent.com/sparrell/sFractals/master/examples/example7.png)

Example 7b
-----

Using the 31M data file from example 7, creates image using different color map.

![modules] (https://raw.githubusercontent.com/sparrell/sFractals/master/examples/example7b.png)

Example 8
--------------

still a work in progress



Example 9
--------------

still a work in progress

