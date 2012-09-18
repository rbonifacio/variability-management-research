package br.unb.cdt.desafioPositivo.util.rest;

public class EsqueciSenhaSRV extends PositivoAPI {

	private String ticket;

	private String novaSenha;

	public EsqueciSenhaSRV(String ticket, String novaSenha) {
		this.ticket = ticket;
		this.novaSenha = novaSenha;
	}

	@Override
	protected void atualizaParametros() {
		req.queryParameter("ticket", ticket);
		req.queryParameter("senhaNova", novaSenha);
	}

	@Override
	protected String url() {  
		System.out.println("URL Webservice -- " + bundle.getString("srv.nova.senha"));
		return bundle.getString("srv.nova.senha"); 
	}

}
