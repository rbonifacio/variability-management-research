package br.unb.cdt.desafioPositivo.util.rest;

import java.text.SimpleDateFormat;

import br.unb.cdt.desafioPositivo.model.Sexo;
import br.unb.cdt.desafioPositivo.model.Usuario;

public class CadastroSRV extends PositivoAPI {

	protected Usuario usuario;
	
	public CadastroSRV(Usuario usuario) {
		this.usuario = usuario;
	}
	
	
	/*
	 * TODO: recuperar de um arquivo de configuracao. 
	 */
	protected String url() {
		return "https://api.mundopositivo.com.br/integracao/rest/sso/cadastraCliente";
	}

	@Override
	protected void atualizaParametros() {
		req.queryParameter("email", usuario.getEmail());
		req.queryParameter("senha", usuario.getSenha());
		req.queryParameter("nome", usuario.getNome());
		req.queryParameter("sobrenome", usuario.getSobrenome());
		req.queryParameter("sexo", (usuario.getSexo().equals(Sexo.MASCULINO) ? "M" : "F"));
		req.queryParameter("estado", usuario.getEstado().getSigla());
		req.queryParameter("dataNascimento", (new SimpleDateFormat("dd/MM/yyyy").format(usuario.getNascimento())));	
	}
}
