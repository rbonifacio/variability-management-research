package br.unb.cdt.desafioPositivo.action;

import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Name;

import br.unb.cdt.desafioPositivo.model.Proposta;

@Name("propostaAction")
@AutoCreate
public class PropostaAction {

	@In(required=false)
	private Proposta proposta;
	
	public void cadastro() {
		System.out.println(proposta.getNome());
		System.out.println(proposta.getDescricao());
	}
}
