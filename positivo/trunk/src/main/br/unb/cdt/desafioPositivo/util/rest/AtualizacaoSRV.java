package br.unb.cdt.desafioPositivo.util.rest;

import java.text.SimpleDateFormat;

import br.unb.cdt.desafioPositivo.model.Sexo;
import br.unb.cdt.desafioPositivo.model.Usuario;

public class AtualizacaoSRV extends PositivoAPI {

	private Usuario usuario;
	
	public AtualizacaoSRV(Usuario usuario) {
		this.usuario = usuario;
	}
	
	
	/*
	 * TODO: recuperar de um arquivo de configuracao. 
	 */
	protected String url() {
		return "https://homolog.api.mundopositivo.com.br/integracao/rest/sso/cadastraCliente";
	}

	@Override
	protected void atualizaParametros() {
		req.queryParameter("email", usuario.getEmail());
		req.queryParameter("nome", usuario.getNome());
		req.queryParameter("sobrenome", usuario.getSobrenome());
		req.queryParameter("sexo", (usuario.getSexo().equals(Sexo.MASCULINO) ? "M" : "F"));
		req.queryParameter("dataNascimento", (new SimpleDateFormat("dd/MM/yyyy").format(usuario.getNascimento())));	
	}
	
}
