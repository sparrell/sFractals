curl -vX POST localhost:8080/sFractal -H'content-type:application/json' -d '{
   "fractalAlg":"julian",
   "imageFileName":"../examples/Output/example09.png",
   "colorAlg":"simplest",
   "width":10,
   "height":10,
   "cReal":0.5,
   "cImaginary":-0.5,
   "zReal":-0.1,
   "zImaginary":-0.1,
   "xRealRight":3.0,
   "xRealLeft":-3.0,
   "yImaginaryLow":-3.0,
   "yImaginaryHigh":3.0,
   "bailoutThreshold":4,
   "maxIterationThreshold":11 
}'
