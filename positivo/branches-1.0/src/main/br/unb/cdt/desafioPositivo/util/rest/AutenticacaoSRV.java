package br.unb.cdt.desafioPositivo.util.rest;

import org.jboss.seam.annotations.In;
import org.jboss.seam.core.ResourceBundle;
import org.jboss.seam.core.SeamResourceBundle;

public class AutenticacaoSRV extends PositivoAPI {

	private String email;
	private String senha;
		
	public AutenticacaoSRV(String email, String senha) {
		this.email = email;
		this.senha = senha;
	}
	
	@Override
	protected void atualizaParametros() {
		req.queryParameter("email", email);
		req.queryParameter("senha", senha);
	}

	@Override
	/*
	 * TODO: recuperar de um arquivo de configuracao
	 */
	protected String url() {
		System.out.println("URL Webservice -- " + bundle.getString("srv.autentica"));
		return bundle.getString("srv.autentica"); 
	}
	
}
