package br.unb.cdt.desafioPositivo.model.acesso;

/**
 * Excecao usada para reportar erros no acesso 
 * do usuario.
 * 
 * @author rbonifacio
 */
public class ExcecaoAcessoUsuario extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public ExcecaoAcessoUsuario(String message) {
		super(message);
	}
}
