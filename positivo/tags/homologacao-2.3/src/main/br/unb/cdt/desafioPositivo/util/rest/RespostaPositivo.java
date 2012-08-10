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
	
	private String nome; 
	private String sobrenome; 
	private String sexo; 
	private String dataNascimento; 
	private String estado; 
	private String email; 
	
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
		
		try {
			JSONObject json2 = json.getJSONObject("cliente");
			nome = json2.getString("nome");
			sobrenome = json2.getString("sobrenome"); 
			sexo = json2.getString("sexo"); 
			dataNascimento = json2.getString("dataNascimento"); 
			estado = json2.getString("estado"); 
			email = json2.getString("email");
			if((token == null) || (token.equals(""))) {
				token = json2.getString("token");
			}
		} catch(JSONException e) {
			nome = "";
			sobrenome = "";
			sexo = "";
			dataNascimento = "";
			estado = "";
			email = "";
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

}
