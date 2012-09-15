package br.unb.cdt.desafioPositivo.util.rest;

import java.text.Normalizer;
import java.text.SimpleDateFormat;
import java.text.Normalizer.Form;

import br.unb.cdt.desafioPositivo.model.Sexo;
import br.unb.cdt.desafioPositivo.model.Usuario;

/**
 * Consome o servico que possibilita a atualizacao dos 
 * dados cadastrais de um usuario.  
 * 
 * 
 * @author Alexandre Lucchesi, Rodrigo Bonifacio
 */
public class AtualizacaoSRV extends PositivoAPI {

	private Usuario usuario;
	
	public AtualizacaoSRV(Usuario usuario) {
		this.usuario = usuario;
	}
	
	/*
	 * TODO: recuperar de um arquivo de configuracao. 
	 */
	protected String url() {
		System.out.println("URL Webservice -- " + bundle.getString("srv.altera.cliente"));
		return bundle.getString("srv.altera.cliente");
	}

	@Override
	protected void atualizaParametros() {
		req.queryParameter("email", usuario.getEmail());
		req.queryParameter("nome", Normalizer.normalize(usuario.getNome(), Form.NFD).replaceAll("[^\\p{ASCII}]", ""));
		req.queryParameter("sobrenome", Normalizer.normalize(usuario.getSobrenome(), Form.NFD).replaceAll("[^\\p{ASCII}]", ""));
		req.queryParameter("sexo", (usuario.getSexo().equals(Sexo.MASCULINO) ? "M" : "F"));
		req.queryParameter("estado", usuario.getEstado().getSigla());
		req.queryParameter("dataNascimento", (new SimpleDateFormat("dd/MM/yyyy").format(usuario.getNascimento())));	
		req.queryParameter("token", usuario.getToken());
	}
	
}
