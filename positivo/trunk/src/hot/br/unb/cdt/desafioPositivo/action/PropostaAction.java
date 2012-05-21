package br.unb.cdt.desafioPositivo.action;

import org.jboss.seam.ScopeType;
import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.annotations.Scope;

import br.unb.cdt.desafioPositivo.model.Proposta;

@Name("propostaAction")
@Scope(ScopeType.CONVERSATION)
@AutoCreate
public class PropostaAction {

	private Proposta proposta;
	
	public PropostaAction() {
		proposta = new Proposta();
	}
	
	public void cadastro() {
		System.out.println(proposta.getNome());
		System.out.println(proposta.getDescricao());
	}

	public Proposta getProposta() {
		return proposta;
	}

	public void setProposta(Proposta proposta) {
		this.proposta = proposta;
	}
	
	
}
