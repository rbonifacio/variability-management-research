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
		return "https://homolog.api.mundopositivo.com.br/integracao/rest/sso/novaSenha";
	}

}
