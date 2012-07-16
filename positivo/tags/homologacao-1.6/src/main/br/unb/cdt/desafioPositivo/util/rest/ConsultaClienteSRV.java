package br.unb.cdt.desafioPositivo.util.rest;

public class ConsultaClienteSRV extends PositivoAPI {

	@Override
	protected void atualizaParametros() {
		// TODO Auto-generated method stub

	}

	@Override
	protected String url() {
		return "https://api.mundopositivo.com.br/integracao/rest/sso/consultaCliente";
	}

}
