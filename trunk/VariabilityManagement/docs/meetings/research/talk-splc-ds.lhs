%% LaTeX Beamer presentation template (requires beamer package)
%% see http://latex-beamer.sourceforge.net/
%% idea contributed by H. Turgut Uyar
%% template based on a template by Till Tantau
%% this template is still evolving - it might differ in future releases!

\documentclass[xcolor=svgnames]{beamer} 

%include polycode.fmt
%include lhs2tex.sty

% \mode<presentation>
% {
% \usetheme{Warsaw}
% 
% \setbeamercovered{transparent}
% }

\usetheme{Singapore}
\usepackage[english]{babel}
\usepackage[latin1]{inputenc}

% font definitions, try \usepackage{ae} instead of the following
% three lines if you don't like this look
\usepackage{mathptmx}
\usepackage[scaled=.90]{helvet}
\usepackage{courier} 


\usepackage[T1]{fontenc}

\usepackage{graphicx}
\usepackage{listings}

%\hypersetup{colorlinks=true,linkcolor=red}

\def\hilite<#1>{%
\temporal<#1>{\color{gray}}{\color{black}}%
{\color{gray}}}
 
\title{Towards a Crosscutting Approach for Variability Management}
\subtitle{SPLC Doctoral Symposium}


% - Use the \inst{?} command only if the authors have different
%   affiliation.
%\author{F.~Author\inst{1} \and S.~Another\inst{2}}
\author{Rodrigo Bonif\'{a}cio \and Paulo Borba}

% - Use the \inst command only if there are several affiliations.
% - Keep it simple, no one is interested in your street address.
\institute
{
	Informatics Center \\ Federal University of Pernambuco \\ Brazil
}

% \date{\date() / SPLC Doctoral Symposium}


% This is only inserted into the PDF information catalog. Can be left
% out.
%\subject{Talks}



% If you have a file called "university-logo-filename.xxx", where xxx
% is a graphic format that can be processed by latex or pdflatex,
% resp., then you can add a logo as follows:

% \pgfdeclareimage[height=0.5cm]{university-logo}{university-logo-filename}
% \logo{\pgfuseimage{university-logo}}




% If you wish to uncover everything in a step-wise fashion, uncomment
% the following command:

%\beamerdefaultoverlayspecification{<+->}



\begin{document}

\lstset{language=haskell}

\begin{frame}
\titlepage
\end{frame}

\section{Introduction}

% \begin{frame}
% \frametitle{Software Product Line (SPL)}
% 
% \begin{block}{Definition}
% \begin{itemize}
%   \item The SPL approach is a well known technique for implementing systematic
%   reuse in software engineering.
%   \item Based on this approach, products are customized (or generated) from a
%   set of reusable assets.
% \end{itemize}
% \end{block}
% \end{frame}
% 
% \begin{frame}
% \frametitle{Software Product Line (SPL)}
% 
% \begin{block}{Technical point: variability management}
% \begin{itemize}
%   \item Reusable assets are extensible by means of variation points.
%   \item Different techniques might be used to implement these VPs.
%   \item Product decisions (capabilities) drive the generation process.
% \end{itemize}
% \end{block}
% 
% \center{
%  \includegraphics[scale=0.25]{img/spl.eps}\footnote{Charles W. Krueger, New
%  methods in software product line practice, CACM (2006)} 
%  }
% 
% \end{frame}

\begin{frame}
\frametitle{Thesis context}

\begin{block}{Crosscutting Nature of Variability Management}
\begin{itemize}
	\item Variable features often require variation points to be scattered
	throughout different places in requirements, design, code, and test artifacts.
\end{itemize}
\end{block}

\begin{block}{Problem}
\begin{itemize}
  \item It is hard to evolve a software product line without a clear separation
  between the problem space and the solution space.
	
  \item	However, several techniques for dealing with variations require 
	some kind of tangling between those models.    
\end{itemize}
\end{block}
\end{frame}

\begin{frame}
\frametitle{Thesis context}

\begin{block}{Proposed solution:}
A modeling framework for representing variability management techniques as
crosscutting mechanisms.
\begin{itemize}
  \item Not only independent models for variability management
  \item Main focus on composition processes of different models
\end{itemize}
%that consider different languages.
\end{block}

\begin{block}{Hypothesis}
A better separation between VM and software engineering
artifacts would improve:

\begin{itemize}
  \item SPL Evolvability
  \item SPL Traceability
  \item SPL Configurability
\end{itemize}
\end{block}

\end{frame}

% \begin{frame}
% \frametitle{Thesis context}
% 
% \begin{block}{Hypothesis}
% A better separation between VM and software engineering
% artifacts would improve:
% 
% \begin{itemize}
%   \item SPL Evolvability
%   \item SPL Traceability
%   \item SPL Configurability
% \end{itemize}
% \end{block}
% 
% \begin{block}{Proposed approach:}
% Enforce such a SoC by means of a \textcolor{DarkBlue}{framework} for
% modeling Variability Management as \textcolor{DarkBlue}{crosscutting
% mechanisms}.
% \end{block}
% \end{frame}
\section{Proposed approach}

\begin{frame}
\frametitle{VM as crosscutting mechanisms}

Different input models crosscut each other with respect to a SPL member
(adapted from Masuhara and Kiczales --- ECOOP 2003).

\onslide+<2>
\begin{block}{Big picture:}
\center{
 \includegraphics[scale=0.25]{img/weave-process2.eps}
}
\end{block}

\end{frame}

\begin{frame}
\frametitle{VM as crosscutting mechanisms}
\begin{block}{Weaving Process}
\begin{itemize}
  \hilite<1> \item Sequence of weavers, guided by the
  configuration knowledge, that should be applied in order to generate specific instances.
  \hilite<2> \item Each weaver is represented as a
  tuple, highlighting the contribution of the
  corresponding input languages. \\ 
  $Weaver = \{O,\ O_{VP},\ L,\ L(l)_{id},\
  	L(l)_{eff}\}$
\hilite<3> \item We depict that a weaver is
  	realizable by relating the corresponding tuple elements to a reference implementation of the weaver.
\end{itemize}	
\end{block}

\end{frame}

\begin{frame}
\frametitle{Exemplo}
\includegraphics[scale=0.55]{img/ck.eps}
\end{frame}

\begin{frame}
\frametitle{VM as crosscutting mechanisms}
\begin{block}{Configuration knowledge}

\begin{itemize}
  \item A sequence of the algebraic data type: \textcolor{DarkBlue}{(Feature
  Expression, [Weaver])}
  \item The weaving process evaluate each feature expression. If it holds true
  for a SPL's member, the corresponding weavers are applied.
\end{itemize}
\end{block}
\center{

}
\hyperlink{ck}{\beamerbutton{code}}
\end{frame}

\begin{frame}
\frametitle{VM as crosscutting mechanisms}

\begin{center}
\begin{columns}
\begin{column}{0.4\textwidth}
 \includegraphics[scale=0.40]{img/product-line.eps}
\end{column}
\begin{column}{0.4\textwidth}
 
\end{column}
\end{columns}
\end{center}

\onslide+<2>
\begin{block}{Example of configuration knowledge}
\begin{scriptsize}
\begin{center}
\begin{tabular}{|p{1.0in}p{1.8in}|}
\hline 
Feature Expression &  Weavers 						\\ \hline
F1 				&  	select(square, WHITE)		\\ \hline
not(F1)			&	select(square, BLACK)		\\ \hline
F2 				&	select(circle)				\\ \hline
F2 and F3		& 	select(triangle), fill(circle, BLUE)\\ \hline				
\end{tabular}
\end{center}
\end{scriptsize}
\end{block}

\begin{block}{Example of SPL's member: $FC\ =\ (F1, F2, F3) $}
\end{block}

\end{frame}


\begin{frame}
\frametitle{VM as crosscutting mechanisms}

\begin{center}
\begin{columns}
\begin{column}{0.4\textwidth}
 \includegraphics[scale=0.40]{img/product-line.eps}
\end{column}
\begin{column}{0.4\textwidth}
 \includegraphics[scale=0.40]{img/instance-1.eps}
\end{column}
\end{columns}
\end{center}

\begin{block}{Example of configuration knowledge}
\begin{scriptsize}
\begin{center}
\begin{tabular}{|p{1.0in}p{1.8in}|}
\hline 
Feature Expression &  Weavers \\ \hline
\textcolor{DarkBlue}{F1} &  \textcolor{DarkBlue}{select(square, WHITE)} \\
\hline not(F1)			&	select(square, BLACK)		\\ \hline
F2 				&	select(circle)				\\ \hline
F2 and F3		& 	select(triangle), fill(circle, BLUE)\\ \hline				
\end{tabular}
\end{center}
\end{scriptsize}
\end{block}

\begin{block}{Example of SPL's member: $FC\ =\ (F1, F2, F3) $}
\end{block}

\end{frame}

\begin{frame}
\frametitle{VM as crosscutting mechanisms}

\begin{center}
\begin{columns}
\begin{column}{0.4\textwidth}
 \includegraphics[scale=0.40]{img/product-line.eps}
\end{column}
\begin{column}{0.4\textwidth}
 \includegraphics[scale=0.40]{img/instance-1.eps}
\end{column}
\end{columns}
\end{center}

\begin{block}{Example of configuration knowledge}
\begin{scriptsize}
\begin{center}
\begin{tabular}{|p{1.0in}p{1.8in}|}
\hline 
Feature Expression &  Weavers \\ \hline
\textcolor{DarkBlue}{F1} &  \textcolor{DarkBlue}{select(square, WHITE)} \\
\hline \textcolor{DarkRed}{not(F1)} & \textcolor{DarkRed}{select(square, BLACK)}\\ \hline 
F2 				&	select(circle)				\\ \hline 
F2 and F3		& 	select(triangle), fill(circle, BLUE)\\ \hline				
\end{tabular}
\end{center}
\end{scriptsize}
\end{block}

\begin{block}{Example of SPL's member: $FC\ =\ (F1, F2, F3) $}
\end{block}

\end{frame}

\begin{frame}
\frametitle{VM as crosscutting mechanisms}

\begin{center}
\begin{columns}
\begin{column}{0.4\textwidth}
 \includegraphics[scale=0.40]{img/product-line.eps}
\end{column}
\begin{column}{0.4\textwidth}
 \includegraphics[scale=0.40]{img/instance-2.eps}
\end{column}
\end{columns}
\end{center}

\begin{block}{Example of configuration knowledge}
\begin{scriptsize}
\begin{center}
\begin{tabular}{|p{1.0in}p{1.8in}|}
\hline 
Feature Expression &  Weavers \\ \hline
\textcolor{DarkBlue}{F1} &  \textcolor{DarkBlue}{select(square, WHITE)} \\
\hline \textcolor{DarkRed}{not(F1)} & \textcolor{DarkRed}{select(square, BLACK)}\\ \hline 
\textcolor{DarkBlue}{F2} & \textcolor{DarkBlue}{select(circle)}	\\ \hline
F2 and F3		& 	select(triangle), fill(circle, BLUE)\\ \hline
\end{tabular}
\end{center}
\end{scriptsize}
\end{block}

\begin{block}{Example of SPL's member: $FC\ =\ (F1, F2, F3) $}
\end{block}

\end{frame}

\begin{frame}
\frametitle{VM as crosscutting mechanisms}

\begin{center}
\begin{columns}
\begin{column}{0.4\textwidth}
 \includegraphics[scale=0.40]{img/product-line.eps}
\end{column}
\begin{column}{0.4\textwidth}
 \includegraphics[scale=0.40]{img/instance-3.eps}
\end{column}
\end{columns}
\end{center}

\begin{block}{Example of configuration knowledge}
\begin{scriptsize}
\begin{center}
\begin{tabular}{|p{1.0in}p{1.8in}|}
\hline 
Feature Expression &  Weavers \\ \hline
\textcolor{DarkBlue}{F1} &  \textcolor{DarkBlue}{select(square, WHITE)} \\
\hline \textcolor{DarkRed}{not(F1)} & \textcolor{DarkRed}{select(square, BLACK)}\\ \hline 
\textcolor{DarkBlue}{F2} & \textcolor{DarkBlue}{select(circle)}	\\ \hline
\textcolor{DarkBlue}{F2 and F3}		& 	\textcolor{DarkBlue}{select(triangle)},
fill(circle, BLUE)\\ \hline
\end{tabular}
\end{center}
\end{scriptsize}
\end{block}

\begin{block}{Example of SPL's member: $FC\ =\ (F1, F2, F3) $}
\end{block}

\end{frame}

\begin{frame}
\frametitle{VM as crosscutting mechanisms}

\begin{center}
\begin{columns}
\begin{column}{0.4\textwidth}
 \includegraphics[scale=0.40]{img/product-line.eps}
\end{column}
\begin{column}{0.4\textwidth}
 \includegraphics[scale=0.40]{img/instance-4.eps}
\end{column}
\end{columns}
\end{center}

\begin{block}{Example of configuration knowledge}
\begin{scriptsize}
\begin{center}
\begin{tabular}{|p{1.0in}p{1.8in}|}
\hline 
Feature Expression &  Weavers \\ \hline
\textcolor{DarkBlue}{F1} &  \textcolor{DarkBlue}{select(square, WHITE)} \\
\hline \textcolor{DarkRed}{not(F1)} & \textcolor{DarkRed}{select(square, BLACK)}\\ \hline 
\textcolor{DarkBlue}{F2} & \textcolor{DarkBlue}{select(circle)}	\\ \hline
\textcolor{DarkBlue}{F2 and F3}		& 	\textcolor{DarkBlue}{select(triangle)},
\textcolor{DarkBlue}{fill(circle, BLUE)}\\ \hline
\end{tabular}
\end{center}
\end{scriptsize}
\end{block}

\begin{block}{Example of SPL's member: $FC\ =\ (F1, F2, F3)$} \hyperlink{wp}{\beamerbutton{code}}
\end{block}

\end{frame}


\section{Modeling framework instance}

\begin{frame}
\frametitle{Walk through an example\ldots}
\onslide+<2>
\begin{block}{Environment}
\begin{itemize}
  \item VM in SPL specifications (use case scenarios)

  \begin{itemize} 
    \item Variability in function
    \item Variability in data
    \item Variability in control flow
  \end{itemize}
  \item Smart Home produtct line
\end{itemize}
\end{block}

\end{frame}

\begin{frame}
\frametitle{Variability in function}
Certain use cases and scenarios of the Smart Home PL might exist in some
products and not in others.

\begin{block}{Examples}
\begin{scriptsize}
\begin{center}
\begin{tabular}{|p{1.6in}p{0.4in}p{1.6in}|}
\hline 
Artifact &  & Feature \\ \hline
Configure Access to Environment & requires & Specific Environment\\ \hline
Handle Fire Detection & requires & Fire Detection \\ \hline
Notify Fire Control System & requires & Fire Detection and Control System \\ \hline
\ldots & & \ldots \\
\hline
\end{tabular}
\end{center}
\end{scriptsize}
\end{block}

\end{frame}

\begin{frame}
\frametitle{Variability in function}
\begin{center}
\includegraphics[scale=0.40]{img/select-weaver.eps}
\end{center}
\hyperlink{addScenario-weaver}{\beamerbutton{code}}
\end{frame}

\begin{frame}
\frametitle{Variability in function}
\begin{block}{Add Scenario representation }
\begin{scriptsize}
\begin{tabular}{p{0.6in}p{2.4in}}
   \hline\noalign{\smallskip}
  {\bf Element} & {\bf Description} \\
   \noalign{\smallskip}
   \hline
   \noalign{\smallskip}
   $O$             	& Scenarios of a product intance (list of scenarios) \\ 
   $O_{VP}$        	& Scenario declarations \\ 
   $L$             	& \{UCM, CK, FC\} \\ 
   $UCM_{ID}$ 	   	& IDs of the SPL scenarios \\ 
   $CK_{ID}$    	& Feature expressions and scenario's IDs\\  
   $FC_{ID}$    	& IDs of the SPL features \\ 
   $UCM_{EFF}$ 		& Provides declaration of scenarios \\  
   $CK_{EFF}$    	& Relates feature expressions to weavers \\ 
   $FC_{EFF}$    	& Selects the scenarios to be added \\
   \hline
  \end{tabular}
\end{scriptsize}
\end{block}
\end{frame}

\begin{frame}
\frametitle{Variability in data}
Certain scenarios of the Smart Home PL are parameterized.

\begin{block}{Example}
The amount of seconds to close the door depends on the product configuration.
\begin{center} 
\begin{small}
  \begin{tabular}{|p{0.2in}|p{1.4in}|p{1.4in}|}
   \hline
       Id & User Action  & System Response \\ \hline \hline
       \ldots & \ldots  & \ldots \\  \hline 
       M3 & The inhabitant informs the access password & \ldots after {\bf
       <TTC>} seconds the front door is closed \\
       \hline
    \end{tabular}
\end{small}
\end{center}
\end{block}
\end{frame}

\begin{frame}
\frametitle{Variability in data}
\begin{center}
\includegraphics[scale=0.40]{img/bind-weaver.eps}
\end{center}
\end{frame}

\begin{frame}
\frametitle{Variability in data}
\begin{block}{Bind Parameter representation}
\begin{scriptsize}
\begin{tabular}{p{0.6in}p{2.4in}}
   \hline\noalign{\smallskip}
  {\bf Element} & {\bf Description} \\
   \noalign{\smallskip}
   \hline
   \noalign{\smallskip}
   $O$             	& Scenarios of a product intance (list of scenarios) \\ 
   $O_{VP}$        	& Steps of scnarios \\ 
   $L$             	& \{UCM, CK, FC\} \\ 
   $UCM_{ID}$ 	   	& Parameters declaration \\ 
   $CK_{ID}$    	& Feature expressions and parameters name\\  
   $FC_{ID}$    	& IDs of the SPL features \\ 
   $UCM_{EFF}$ 		& Provides declaration of parameterized scenarios \\  
   $CK_{EFF}$    	& Relates feature expressions to weavers \\ 
   $FC_{EFF}$    	& States the actual values of parameters \\
   \hline
  \end{tabular}
\end{scriptsize}
\end{block}
\end{frame}

\begin{frame}
\frametitle{Variability in control flow}
Certain scenarios of the Smart Home PL describe different behavior 
dependend on the product configuration.

\begin{block}{Example}
Register Inhabitant scenario varies dependend on the security mechanism that
was selected.
\end{block}
\onslide+<2>
\begin{block}{Security Mechanism = Password}
\begin{center} 
\begin{tiny}
  \begin{tabular}{|p{0.2in}|p{1.8in}|p{1.8in}|}
   \hline
       Id & User Action  & System Response \\ \hline \hline
       M1 & \ldots.  & \ldots \\  \hline
       
       P1 & The home owner fills in the inhabitant personal form and selects
       the proceed option. & The system requires the inhabitant password (and password
       confirmation) for getting access to the home. \\ \hline 
       
       P2 & The new inhabitant fills in the password (and confirmation
       password) for home access. & The system requests the home owner
       configuration password. \\
       \hline
       
       M2 & \ldots.  & \ldots \\  \hline
    \end{tabular}
\end{tiny}
\end{center}
\end{block}
\end{frame}

\begin{frame}
\frametitle{Variability in control flow}
Certain scenarios of the Smart Home PL describe different behavior 
dependend on the product configuration.

\begin{block}{Example}
Register Inhabitant scenario varies dependend on the security mechanism that
was selected.
\end{block}
\begin{block}{Security Mechanism = Finger Print}
\begin{center} 
\begin{tiny}
  \begin{tabular}{|p{0.2in}|p{1.8in}|p{1.8in}|}
   \hline
       Id & User Action  & System Response \\ \hline \hline
       M1 & \ldots.  & \ldots \\  \hline
       
       F1 & The home owner fills in the inhabitant personal form and selects
       the proceed option. & The system requires the inhabitant inhabitant finger print to capture it. \\ \hline 
       
       F2 & The new inhabitant put his finger in the finger print capture
       device. & The system requests the home owner configuration password. \\
       \hline
       
       M2 & \ldots.  & \ldots \\  \hline
  \end{tabular}
\end{tiny}
\end{center}
\end{block}
\end{frame}

\begin{frame}
\frametitle{Variability in control flow}
\begin{center}
\includegraphics[scale=0.40]{img/aspect-weaver.eps}
\end{center}
\hyperlink{aspect-code}{\beamerbutton{code}}
\end{frame}

\begin{frame}
\frametitle{Variability in data}
\begin{block}{Evaluate Aspect representation}
\begin{scriptsize}
\begin{tabular}{p{0.6in}p{2.4in}}
   \hline\noalign{\smallskip}
  {\bf Element} & {\bf Description} \\
   \noalign{\smallskip}
   \hline
   \noalign{\smallskip}
   $O$             	& Scenarios of a product intance (list of scenarios) \\
   $O_{VP}$        	& Steps of scnarios \\
   $L$             	& \{AUCM, UCM, CK, FC\} \\
   $AUCM_{ID}$ 		& Step ids or step annotations \\
   $UCM_{ID}$ 		& Scenario declaration \\
   $CK_{ID}$    	& Feature expressions and aspects name\\
   $FC_{ID}$    	& IDs of the SPL features \\
   $AUCM_{EFF}$ 	& Augments scenario specifications	\\
   $UCM_{EFF}$ 		& Provides declaration of scenarios \\
   $CK_{EFF}$    	& Relates feature expressions to weavers \\
   $FC_{EFF}$    	& Selects the aspects to be evaluated \\
   \hline
  \end{tabular}
\end{scriptsize}
\end{block}
\end{frame}

\section{Concluding remarks}

\begin{frame}
\frametitle{Concluding remarks and future work}
\begin{block}{Contributions}
\begin{itemize}
  \item A novel approach for representing VM 
  \item New techniques proposed for VM in use case scenarios
  \item A Haskell DSL for variability management
  \begin{itemize}
    \item Feature modeling (integrated to FM Plugin)
    \item Product generation
  \end{itemize}  
  \item A suite of metrics adapted to our research context
\end{itemize}
\end{block}

\end{frame}

\begin{frame}
\frametitle{Concluding remarks and future work}
\begin{block}{Evaluation}
\begin{itemize}
  \item We have evaluated our proposals in three case studies, and observed: 
  \begin{itemize}
    \item A reduction in the size of specifications
    \item Features better modularized (DOS, DOF) 
    \item Improvements in SPL evolvability
  \end{itemize}  
\end{itemize}
\end{block}

\end{frame}

\begin{frame}
\frametitle{Concluding remarks and future work}
\begin{block}{Next steps}
\begin{itemize}
  \item Plan and execute a controlled experiment 
  \item Instantiate the modeling framework to other types of artifacts  
  \item Release a version of our Haskell Libraries and tools
\end{itemize}
\end{block}

\end{frame}

\begin{frame}
\titlepage
\end{frame}

\section*{Backup}

\begin{frame}[fragile]
\frametitle{Code Generation}

\begin{block}{velocity}
\end{block}
\begin{small}
\begin{verbatim}

public interface Expression {
	public $type eval();
#if($print)
	public String print();
#end
}
\end{verbatim}
\end{small}
\end{frame}

\begin{frame}[fragile]
\frametitle{Code Generation}

\begin{block}{velocity}
\end{block}
\begin{small}
\begin{verbatim}
public class BinaryExp implements Expression {
	...
	public String getOperator() {
		return operator;
	}
	
#if($print)
	public String print() {
		return left.print() + operator + right.print();
	}
#end
}
\end{verbatim}
\end{small}
\end{frame}



\begin{frame}
\frametitle{Message Application Product Line}

\begin{block}{Feature model}
\includegraphics[scale=0.5]{img/fm03.eps}
\end{block}
\end{frame}

\begin{frame}
\frametitle{PLUC Notation}
\begin{block}{No separation between VM and software assets}
\includegraphics[scale=0.4]{img/pluc.eps}
\end{block}
\end{frame}

\begin{frame}[fragile,label=ck]
\frametitle{Configuration Knowledge}
\begin{block}{Abstract Syntax}
\begin{center}
\begin{tiny}
\begin{lstlisting}[frame=tb]
type ConfigurationKnowledge = [Configuration]
 
data Configuration  = Configuration {
 expression :: FeatureExpression,
 weavers :: [Model2Model] 	 
}
\end{lstlisting}
\end{tiny}
\end{center}
\end{block}
\end{frame}

\begin{frame}[fragile,label=wp]
\frametitle{Configuration Knowledge}

\begin{block}{Weaving Process}
\begin{center}
\begin{tiny}
\begin{lstlisting}[frame=tb]

buildConfiguration spl fc ck = applyAllTransformations spl ck (emptyInstance spl fc)
    
applyAllTransformations spl [] productInstance = productInstance
applyAllTransformations spl (x:xs) productInstance = 
 let
 	f = (instanceConfiguration productInstance)
 	e = (expression x) 
 	w = (weavers x)
   in if (eval f e) 
      then applyAllTransformations spl xs (applyTransformations spl w productInstance) 
      else applyAllTransformations spl xs productInstance
\end{lstlisting}
\end{tiny}   
\end{center}  
\end{block}

\end{frame}

\begin{frame}[fragile,label=addScenario-weaver]
\frametitle{Reference Implementations}

\begin{block}{Add Scenario}
\begin{center}
\begin{tiny}
\begin{lstlisting}[frame=tb]

addScenariosM2M :: [Id] -> ProductLine -> ProductInstance -> ProductInstance
addScenariosM2M ids spl productInstance =
 let
 	inputUCM = splUseCaseModel spl
 	outputUCM = instanceUseCaseModel productInstance
	ins = ucmScenarios inputUCM
	outUseCases = useCases outputUCM
	rName = ucmName outputUCM
	rUseCases = addUseCaseM2M inputUCM outputUCM  [s | s <- ins, exists (scenarioId s) ids]
	rAspects = aspects outputUCM
   in
    productInstance {
   	 instanceUseCaseModel = UCM rName rUseCases rAspects
   	}
\end{lstlisting}
\end{tiny}
\end{center}
\end{block}
\end{frame}

\begin{frame}[fragile,label=aspect-code]
\frametitle{Aspectual Use Case}

\begin{block}{Abstract Syntax}
\begin{center}
\begin{tiny}
\begin{lstlisting}[frame=tb]
data AspectualUseCase = AspectualUseCase {
		aspectId :: Id,
		aspectName :: Name,
		advices :: [Advice]
	} 
	deriving(Show)
	
data Advice = 
 BeforeAdvice {pointCut :: [StepRef], aspectualScenario :: Scenario}  | 
 AfterAdvice { pointCut :: [StepRef], aspectualScenario :: Scenario }

\end{lstlisting}
\end{tiny}
\end{center}
\end{block}
\end{frame}

\end{document}
