(* ::Package:: *)

(* 
Author: Celestine P. Lawrence
Date: 01/10/21
Runtime: ~8 mins using Wolfram Mathematica 12.3 on a HP Slim Desktop S01-pF1000nd
Result: Uploaded on https://youtu.be/FrumXDKqHhY 
*)	


(*Mathematical definitions*)
LM[\[Xi]_,\[Alpha]_]:=\[Alpha]*\[Xi](1-\[Xi]); (*Logistic map. Oscillatory behavior is observed for 3<\[Alpha]<4,
where a period doubling bifurcation results in an onset of chaos at a\[TildeTilde]3.57 that ultimately ends in a trifurcation at a\[TildeTilde]3.83, 
with each branch repeating the period doubling route to chaos and its end by trifurcation,
at recursively smaller scales until a=4*)
P[x_]:=Clip[1/(2x(1-x))-3,{0,1}];  (*Probability that L[x,a]<1/2 given x and assuming a is uniformly distributed between 3 and 4*)
U[x_]:=Module[{p= P[x]},4p(1-p)]; (*Ad-hoc expression for Uncertainty (related to information entropy),
satisfying the extrema constraints U=0 for p=0 or 1 and U=1 for p=1/2*) 
S[x_,a_]:= Module[{p= P[x]},If[LM[x,a]<1/2,1-p,p]]; (*Ad-hoc expression for Surprise (related to information content),
satisfying the extrema constraints S=0 or 1 for p= 0 or 1*)
R[x_,a_]:=U[x]+S[x,a]-2 U[x]S[x,a];(*Ad-hoc expression for musical pleasure (or Reward) as a function of the uncertainty and surprise,
satisfying the extrema constraints R =0 at U=S=0 or U=S=1 and R = 1 at U=1 S=0 or U=0 S=1*)
CL[x_]:=NArgMax[{R[x,aa],3<=aa<=4},aa,Method->{"DifferentialEvolution"}];(*Control law chosen to maximise R*) 


(*Generate bitstream from a controlled evolution of the logistic map*)
t=1000; (*number of beats*)
x = 0.5; a = 3.5;(*initial conditions of our discrete dynamical system where x is a main-variable and a is a step-mechanism *)
Dynamic@n
dat=Table[x=LM[x,a]; a=CL[x] ;{x,a},{n,t}]; (*iteratively update the system*)

(*Plot results*)
{ListPlot@dat[[;;,1]] (*main-variable evolution seems to be between order and disorder*),
ListPlot@dat[[;;,2]] (*step-mechanism evolution is not a uniform distribution - thus the estimation of P[x] is inaccurate - needs revision in future work!*),
ListPlot[ Table[ {x,a}=xa;{U[x],S[x,a]},{xa,dat}] , PlotRange->Full] (*state-space evolution seems to be preferentially near the U=1 S=0 or U=0 S=1 region*)}

(*Generate audio*)
sound[x_]:= If[x>0.5,SoundNote["LowBongo",0.1],SoundNote[None,0.1]]
audioLM=Sound[
sound/@dat[[;;,1]]]


(*Generate video*)
Rscape=N@ParallelTable[{U[x],0.,S[x,a]} ,{x,1/2/t,1,1/t},{a,3+1/4/t,4,1/2/t}];(* Obtain a visually interpretable reward-scape by shading uncertainty (in red) and surprise (in blue) across the entire state space *)
pics = Table[ {x,a}=dat[[n]]; (*iterate over the states to generate a sequence of images*)
Rscape[[Ceiling[ x*t],Clip[Round[(a-3)*2*t],{1,2*t}],2]]=1.; (*add a full shade of green to the current state*) 
{i,j}=Sort@Ceiling[{x,LM[x,a]}*t]; Cspace= Floor[255*Rscape]; 
Cspace[[i;;j,Clip[Round[(a-3)*2*t],{1,2*t}],2]]=ConstantArray[100,j-i+1]; (*add a partial shade of green to the line joining the current value to the next value of x at the current value of a*)
Image[Cspace,"Byte"] ,{n,t}];
VideoCombine[{SlideShowVideo[pics->0.1],audioLM}]
Export["LMargmaxDE.mp4",%]
