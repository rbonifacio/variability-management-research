function confirmarDelecaoProposta(form) { 
/* 
abaixo voc� coloca a configura��o de p�gina de confirma��o 
lembrando que n�o pode usar aspas simples (') se necess�rio colocar (\) antes, ex: \' 
os campos do formul�rio s�o chamados assim: 
(dentro de valor) = '+ document.formulario.ocampoaqui.value +' 
*/ 
	valor='' 
	valor+='<title>P�gina de confirma��o</title>' 
	valor+='<form action="" method="POST">' 
	valor+=''+ document.formulario.nome.value +'' 
	valor+='<input type="hidden" name="nome" value="'+ document.formulario.nome.value +'">' 
	valor+='<br>' 
	valor+=''+ document.formulario.email.value +'' 
	valor+='<input type="hidden" name="email" value="'+ document.formulario.nome.value +'">' 
	valor+='<br>' 
	valor+='<input type="submit" value="Enviar">' 
	valor+='<br>' 
	valor+='<center><input type="button" value="Fechar e voltar" onClick="javascript:window.close()"></center>' 
	valor+='</form>' 
	/* 
	fun��o para abrir a popup 
	*/ 
	abre=window.open("","_blank","status=no,resizable=no,scrollbars=no,menubar=no,width=400,height=330"); 
	abre.document.write(valor);
} 

