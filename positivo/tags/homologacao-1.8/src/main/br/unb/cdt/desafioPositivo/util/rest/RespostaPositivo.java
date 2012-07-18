package br.unb.cdt.desafioPositivo.util.rest;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Classe que encapsula as respostas aos 
 * servicos consumidos pela positivo. 
 */
public class RespostaPositivo {
	private Integer codigo;
	private String token;
	private String excecao;
	
	public RespostaPositivo() {} 
	
	public RespostaPositivo fromJASON(String resp) throws Exception {
		JSONObject json = new JSONObject(resp);
		
		codigo = json.getInt("result");
		
		//o campo result deve estar sempre preenchido,
		//mas o campo token pode nao ser submetido nass 
		//respostas. aqui tentaremos le-lo sempre, e caso algum 
		//problema ocorra, reportamos isso na excecao.
		
		try {
			token = json.getString("token");	
		}
		catch(JSONException e) {
			token = "";
			excecao = e.getLocalizedMessage();
		}
		
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
