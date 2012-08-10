package br.unb.cdt.desafioPositivo.facade;

/**
 * Classe usada para reportar as excecoes de senha invalida e senha nao bate com a confirmacao.
 * 
 * @author positivo
 */
public class ExcecaoSenhaDiferente extends Exception {
	
	private static final long serialVersionUID = 1L;
	
	private static final String SENHA_DIFERENTE = "Senha difere da confirmacao";
	
	public ExcecaoSenhaDiferente() {
		super(SENHA_DIFERENTE);
	}

}
