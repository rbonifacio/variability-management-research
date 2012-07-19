package br.unb.cdt.desafioPositivo.facade;

/**
 * Classe usada para reportar as excecoes de senha invalida e senha nao bate com a confirmacao.
 * 
 * @author positivo
 */
public class ExcecaoSenhaIncorreta extends Exception {
	
	private static final long serialVersionUID = 1L;
	
	private static final String SENHA_INCORRETA = "Senha incorreta";
	
	public ExcecaoSenhaIncorreta() {
		super(SENHA_INCORRETA);
	}

}
