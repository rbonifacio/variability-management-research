package br.unb.cdt.desafioPositivo.facade;

/**
 * Excecao usada para reportar problemas no envio 
 * de emails. 
 * 
 * @author positivo
 */
public class ExcecaoEnvioEmail extends Exception {
	
	private static final long serialVersionUID = 1L;

	public ExcecaoEnvioEmail(String message) {
		super(message);
	}
}
