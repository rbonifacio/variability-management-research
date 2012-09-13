package br.unb.cdt.desafioPositivo.util.rest;

import org.json.JSONException;
import org.json.JSONObject;

public class RespostaPositivoUsuario extends RespostaPositivo {
	
	private String nome; 
	private String sobrenome; 
	private String sexo; 
	private String dataNascimento; 
	private String estado; 
	private String email; 
	private String token; 
	
	public String getNome() {
		return nome;
	}

	public String getSobrenome() {
		return sobrenome;
	}

	public String getSexo() {
		return sexo;
	}

	public String getDataNascimento() {
		return dataNascimento;
	}

	public String getEstado() {
		return estado;
	}

	public String getEmail() {
		return email;
	}

	public String getToken() {
		return token;
	}
	
	public RespostaPositivo fromJASON(String resp) throws Exception {
		JSONObject json = new JSONObject(resp);
		
		try {
			nome = json.getString("nome");
			sobrenome = json.getString("sobrenome"); 
			sexo = json.getString("sexo"); 
			dataNascimento = json.getString("dataNascimento"); 
			estado = json.getString("estado"); 
			email = json.getString("email"); 
			token = json.getString("token");
		} catch(JSONException e) {
			token = "";
			nome = "";
			sobrenome = "";
			sexo = "";
			dataNascimento = "";
			estado = "";
			email = "";
		}
		
		return this;
	}
	
	
}
