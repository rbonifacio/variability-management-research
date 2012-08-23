package br.unb.cdt.desafioPositivo.util.rest;

/**
 * Servico para a  criacao de novas senhas.
 * 
 * @author rbonifaco / alexandre lucchesi
 */
public class NovaSenhaSRV extends PositivoAPI {

	private String token;
	
	private String novaSenha;
	
	public NovaSenhaSRV(String token, String novaSenha) {
		this.token = token;
		this.novaSenha = novaSenha;
	}
	
	@Override
	protected void atualizaParametros() {
		req.queryParameter("token", token);
		req.queryParameter("senhaNova", novaSenha);
	}

	@Override
	protected String url() { 
		System.out.println("URL Webservice -- " + bundle.getString("srv.altera.senha"));
		return bundle.getString("srv.altera.senha"); 
	}

}
