package br.unb.cdt.desafioPositivo.util.rest;

public class EnviaEmailSRV extends PositivoAPI {
	
	private String email;
	
	private String url;
	
	public EnviaEmailSRV(String email, String urlRecuperarSenhaPositivo) {
		this.email = email;
		this.url = urlRecuperarSenhaPositivo;
		//this.url = "https://www.concursoideiapp.com.br/desafioPositivo/usuario/recuperarSenhaPositivo.seam";
	}

	@Override
	protected void atualizaParametros() {
		req.queryParameter("email", email);
		req.queryParameter("url", url);
	}
	
	@Override
	protected String url() { 
		System.out.println("URL Webservice -- " + bundle.getString("srv.envia.email"));
		return bundle.getString("srv.envia.email"); 
	}

}
