(* ::Package:: *)

(* 
Author: Celestine P. Lawrence
Date: 21/09/21
Runtime: ~8 mins using Wolfram Mathematica 12.2 on a HP Slim Desktop S01-pF1000nd
Result: Uploaded on https://youtu.be/wI4LyttYaf0 after combining the exported files into a *.mp4 using Windows video editor
*)	


t=1000; (*number of Bongo beats*)
c=CellularAutomaton[150,{{1},0},{{0,t-1}}];
Image[c]
dat=c[[1;;t,t-1]]; (*take center column of CA-150 to define a nested bitstream*)


(*Generate audio*)
audio150=Sound[dat//.{1->SoundNote["LowBongo",.1],0->SoundNote[None,.1]}];
Export["ca150.mp3" ,Audio@audio150 ]


(*Generate video*)
pics=Table[ Image@ArrayPad[c[[;;n]],{{0,t-n},0}],{n,t}];
vid=VideoGenerator[pics,100]
Export["ca150.avi" ,vid ]



