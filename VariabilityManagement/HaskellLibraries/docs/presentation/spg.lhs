\documentclass{beamer}

\usefonttheme{serif}

%include polycode.fmt
%include lhs2tex.sty

\title{Hephaestus Technical Details} 

\author{Rodrigo Bonif\'{a}cio}

\begin{document}

\begin{frame}
\titlepage
\end{frame}

\begin{frame}
\frametitle{SPL data type}

\begin{code}
data SPLModel = SPLModel {
  splFm   :: FeatureModel,
  splReq  :: RequirementModel,  
  splUcm  :: UseCaseModel, 
  splComp :: ComponentModel 
}
\end{code}
\end{frame}

\begin{frame}
\frametitle{Instance data type} 
\begin{code}
data Product = Product { 
  fc  :: Configuration,
  req :: RequirementModel,   
  ucm :: UseCaseModel, 
  components :: [Name] 
}
\end{code}
\end{frame}

\begin{frame}
\frametitle{Transformations and Configuration Knowledge}

\begin{code}
type Transformation = SPLModel  ->  Product -> Product
data ConfigurationItem = ConfigurationItem {
  exp :: FeatureExpression,           
  ts :: [Transformation] 	    
} 
type CK = [Configuration]
\end{code}

\end{frame}

\begin{frame}
\frametitle{CK evaluation}
\begin{code}
evalCk :: Configuration -> CK -> [Transformation]

evalCk fc ck = concat [ts c | c <- ck, eval fc (exp c)]
\end{code}
\begin{itemize}
\item Therefore, \texttt{evalCk} returns a list of transformations 
that should be applied for a given feature configuration. 
\end{itemize}
\end{frame}

\begin{frame}
\frametitle{Building products}

\begin{code}
build fm fc ck spl = refine tasks spl emptyInstance
 where 
  tasks         = evalCk fc ck 
  emptyInstance = ...
  refine [] spl p = p
  refine (x:xs) spl p = refine xs spl (x spl p)
\end{code}

\begin{itemize}
\item Therefore, \texttt{build} returns a Product that was configured 
by means of composing several transformations. It is a \emph{purely functional} 
implementation without IO...
\end{itemize}

\end{frame}

\begin{frame}
\frametitle{Of course, some IO operations are required}

\begin{itemize}
\item But they are application specific. Hephaestus libraries do not 
deal with IO. In fact, tool developers are able to export (through IO operations) 
product representation in 
different ways:
\begin{itemize}
\item Latex representation of use case models
\item LST files from component models
\item Source files from component models 
\end{itemize}
\end{itemize}
\end{frame}

\begin{frame}
\frametitle{Examples of transformations}

\begin{block}{Use case model}
\begin{small}
\begin{code}
selectScenarios :: [Id] -> SPLModel -> Product -> Product
bindParameter :: Id -> Id -> SPLModel -> Product -> Product
evaluateAspects:: [Id] -> SPLModel -> Product -> Product
\end{code}
\end{small}
\end{block}

\begin{block}{Component model (mapping $name\ \Rightarrow \ source$)}
\begin{small}
\begin{code}
selectComponents :: [Id] -> SPLModel -> Product -> Product       
\end{code}
\end{small}
\end{block}
\end{frame}

\end{document}
