package br.unb.cdt.desafioPositivo.facade;

/**
 * Classe usada para reportar a excecao de usuario jah cadastrado.
 * 
 * @author positivo
 */
public class ExcecaoUsuarioCadastrado extends Exception {
	
	private static final long serialVersionUID = 1L;
	
	private static final String USUARIO_CADASTRADO = "Usuario jah cadastrado na rede positivo";
	
	public ExcecaoUsuarioCadastrado() {
		super(USUARIO_CADASTRADO);
	}

}
