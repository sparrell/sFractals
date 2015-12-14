curl -vX POST localhost:8080/sFractal -H'content-type:application/json' -d '{
   "fractalAlg":"julian",
   "imageFileName":"example10.png",
   "colorAlg":"simple64",
   "width":8000,
   "height":8000,
   "cReal":0.5,
   "cImaginary":-0.5,
   "zReal":-0.1,
   "zImaginary":-0.1,
   "xRealRight":1.0,
   "xRealLeft":-1.0,
   "yImaginaryLow":0.0,
   "yImaginaryHigh":2.0,
   "bailoutThreshold":4,
   "maxIterationThreshold":63 
}'
