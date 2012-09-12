package br.unb.cdt.desafioPositivo.util.rest;

import java.text.Normalizer;
import java.text.Normalizer.Form;
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
		System.out.println("URL Webservice -- " + bundle.getString("srv.cadastra.cliente"));
		return bundle.getString("srv.cadastra.cliente");
	}

	@Override
	protected void atualizaParametros() {
		req.queryParameter("email", usuario.getEmail());
		req.queryParameter("senha", usuario.getSenha());
		req.queryParameter("nome", Normalizer.normalize(usuario.getNome(), Form.NFD).replaceAll("[^\\p{ASCII}]", ""));
		req.queryParameter("sobrenome", Normalizer.normalize(usuario.getSobrenome(), Form.NFD).replace("[^\\p{ASCII}]", ""));
		req.queryParameter("sexo", (usuario.getSexo().equals(Sexo.MASCULINO) ? "M" : "F"));
		req.queryParameter("estado", usuario.getEstado().getSigla());
		req.queryParameter("dataNascimento", (new SimpleDateFormat("dd/MM/yyyy").format(usuario.getNascimento())));	
	}
}
