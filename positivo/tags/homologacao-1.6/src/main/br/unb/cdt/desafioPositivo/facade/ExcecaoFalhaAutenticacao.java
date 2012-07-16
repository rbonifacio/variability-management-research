package br.unb.cdt.desafioPositivo.facade;

/**
 * Excecao usada para reportar falhas na autenticacao.
 * 
 * @author positivo
 */
public class ExcecaoFalhaAutenticacao extends Exception {

	private static final long serialVersionUID = 1L;

	public ExcecaoFalhaAutenticacao(String message) {
		super(message);
	}
}
