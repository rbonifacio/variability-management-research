package br.unb.cdt.desafioPositivo.util.rest;

public class ConsultaClienteSRV extends PositivoAPI {
	
	private String token;

	@Override
	protected void atualizaParametros() {
		req.queryParameter("token", token);
	}
	
	public ConsultaClienteSRV(String token) {
		this.token = token;
	}

	@Override
	protected String url() {
		return "https://homolog.api.mundopositivo.com.br/integracao/rest/sso/consultaCliente";
	}

}
