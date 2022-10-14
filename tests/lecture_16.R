\documentclass{beamer}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lecture 15 - Logistic Regression
%%%   
%%%
%%% Last Modified 10/22/2012
%%%
%%% Changelog:
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%\input{handoutPreamble.Rnw} %for when we use the handout argument for documentclass
\input{slidePreamble.Rnw}

%%%%%load in code chunks, and run some preperatory code
<<set-options, echo=FALSE, cache=FALSE, include=FALSE>>=
source("./beamerPrep.R")
read_chunk("./lecture_15.R")

opts_chunk$set(size = "footnotesize")
opts_chunk$set(fig.align = "center")
opts_chunk$set(out.height="0.55\\textwidth")
opts_chunk$set(out.width="0.55\\textwidth")
opts_chunk$set(fig.height=5)
opts_chunk$set(fig.width=5)
opts_chunk$set(tidy=FALSE)
opts_chunk$set(prompt=FALSE)
opts_chunk$set(comment="#")

par(mar=c(5,4,2,2))
@

<<lecture15prep, echo=FALSE, cache=FALSE, include=FALSE>>=
@

\begin{document}




\end{document}