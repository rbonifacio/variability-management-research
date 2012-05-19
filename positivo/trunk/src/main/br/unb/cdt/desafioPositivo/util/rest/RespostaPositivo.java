package br.unb.cdt.desafioPositivo.util.rest;

import org.json.JSONObject;

/**
 * Classe que encapsula as respostas aos 
 * servicos consumidos pela positivo. 
 */
public class RespostaPositivo {
	private Integer codigo;
	private String token;
	
	public RespostaPositivo() {} 
	
	public RespostaPositivo fromJASON(String resp) throws Exception {
		JSONObject json = new JSONObject(resp);
		
		setCodigo(json.getInt("result"));
		setToken(json.getString("token"));	
		
		return this;
	}
	
	public Integer getCodigo() {
		return codigo;
	}
	
	public void setCodigo(Integer codigo) {
		this.codigo = codigo;
	}
	
	public String getToken() {
		return token;
	}
	
	public void setToken(String token) {
		this.token = token;
	}	
}
