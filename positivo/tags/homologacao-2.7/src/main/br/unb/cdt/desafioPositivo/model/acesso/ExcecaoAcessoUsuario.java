package br.unb.cdt.desafioPositivo.model.acesso;

/**
 * Excecao usada para reportar erros no acesso 
 * do usuario.
 * 
 * @author rbonifacio
 */
public class ExcecaoAcessoUsuario extends Exception {
	public ExcecaoAcessoUsuario(String message) {
		super(message);
	}
}
