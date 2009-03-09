%% LaTeX Beamer presentation template (requires beamer package)
%% see http://latex-beamer.sourceforge.net/
%% idea contributed by H. Turgut Uyar
%% template based on a template by Till Tantau
%% this template is still evolving - it might differ in future releases!

\documentclass{beamer} 

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include polycode.fmt

%format emptyline = "~"
%format |=> = "\Rightarrow"

% \mode<presentation>
% {
% \usetheme{Warsaw}
% 
% \setbeamercovered{transparent}
% }

\usetheme{CambridgeUS}
\usepackage[english]{babel}
\usepackage[latin1]{inputenc}


\title{A Haskell Library for Feature Modeling}

\author{Rodrigo Bonif\'{a}cio \and Paulo Borba}


\institute
{
	Informatics Center \\ Federal University of Pernambuco \\ Brazil
}


\begin{document}



\begin{frame}
\titlepage
\end{frame}

\begin{frame}
 \frametitle{Features}
 
 \begin{itemize}
  \item Feature models enriched with properties and global constraints
  \item Type and satisfiability checkers
  \item Integration with FMPlugin and FMIde
  \item Minnor points:
  \begin{itemize}
    \item Following the hackage structure
    \item Embedded in literate haskell--- is it good?
  \end{itemize} 
\end{itemize}
 
\end{frame}

\begin{frame}
\frametitle{Data types}
\begin{block}{Feature Model and Feature Configuration}
\begin{code}
data FeatureModel = FeatureModel {
	fmRoot :: Root,
        fmConstraints :: Constraints 
} deriving (Show)
data FeatureConfiguration = FeatureConfiguration {
	fcRoot :: Root
} deriving (Show)
\end{code}
\end{block}

\end{frame}

\begin{frame}
\frametitle{Data types}
\begin{block}{Feature}
\begin{code}
data Feature = Feature {
	fId :: Id, 
	fName :: Name, 
	fType :: FeatureType,
	groupType :: GroupType,
	children :: Children,
	properties :: Properties
 } | FeatureError  
\end{code}
\end{block}

\end{frame}

\begin{frame}
\frametitle{Data types}
\begin{block}{Global constraints}
\texttt{
\begin{code}
data ConstraintType = Implies | Iff 
 deriving (Show)

data Constraint = Constraint { 
 	constraintType :: ConstraintType, 
        constraintLHSExp :: FeatureExpression, 
	constraintRHSExp :: FeatureExpression
} deriving (Show)
\end{code}
}
\end{block}

\end{frame}

\begin{frame}
\frametitle{Feature Model Type Checker}

\begin{enumerate}
 \item The root feature must be a mandatory feature
 \item Feature ids must be unique in a feature model
 \item All features must be well typed
 \item Feature options must be defined as optional
 \item The global constraints must be well typed
 \item The feature model must accept at least one instance
 \item \ldots
\end{enumerate}
\end{frame}

\begin{frame}
\frametitle{Feature Model Satisfiability}
\begin{block}{SAT solver for checking the FM constraints}
 \begin{itemize}
  \item Features translated to CNF expressions 
  \begin{itemize}
   \item Using the De Morgan's laws and distribution of $\land$ and $\lor$
   \item Using the Tseitin's transformation approach 
  \end{itemize}
  \item FUNSAT library used for verifying CNF satisfability
 \end{itemize} 
\end{block}
\end{frame}

\begin{frame}
\frametitle{Mapping Features into Propositional Logic}
\begin{code}
 featureToPL f =
  let cs = children f
  in  
   case groupType f of
    Basic -> [f |=> c  | c <- cs, fType c == Mandatory] 
    IOR   -> [f |=> (foldOr [ref c | c <- cs])]  
    XOR   -> [f |=> (foldOr [xor c (delete c (cs)) | c <- cs])]  
\end{code}
\end{frame}

\begin{frame}
\frametitle{Benchmark for the FM type checker}

\begin{description}
 \item[CNF(FM100):] v = 381, c = 616
 \item[CNF(FM200):] v = 415, c = 663
 \item[CNF(FM500):] v = 988, c = 1587 
\end{description}
\begin{center}
 \includegraphics[scale=0.4]{images/bench.eps}
\end{center}
\end{frame}

\begin{frame}
\frametitle{Type Checker for the Feature Configuration}
\begin{itemize}
 \item The related feature model (FM) must be well typed
 \item The selection of features must be an instance of FM 
\end{itemize}
\end{frame}

\begin{frame}
\frametitle{Type Checker for the Feature Configuration}
\begin{block}{Interpreting both FM and FC}
\begin{code}
validInstance :: FeatureModel -> FeatureConfiguration -> ErrorList
validInstance fm fc = 
 let f1 = fmRoot fm
     f2 = fcRoot fc
 in 
  if (f1 == f2) then (checkType fm) ++ (checkFeatures f1 f2) ++ (checkConstraints fm fc)
  else ["The root elements must be the same"]

checkFeatures fm fc = 
 case (groupType fm) of 
   BasicFeature -> checkBasicFeature fm fc
   AlternativeFeature -> checkAlternativeFeature fm fc  
   OrFeature -> checkOrFeature fm fc
\end{code}
\end{block}
\end{frame}

\begin{frame}
\frametitle{Type Checker for the Feature Configuration}
\begin{block}{Evaluating FM constraints}
\begin{code}
validInstance' :: FeatureModel -> FeatureConfiguration -> Bool
validInstance' fm fc = 
 let fmExpression = foldAnd (fmToPropositionalLogic fm)
 in eval fc fmExpression
\end{code}
\end{block}
\end{frame}

\begin{frame}
\frametitle{Future work}
\begin{itemize}\item Identify the scalability problem of our satisfiability functions
\item Peform more benchmark tests
\item future, future, future work: detecting bad smells and refactoring 
\end{itemize}
\end{frame}


\end{document}
