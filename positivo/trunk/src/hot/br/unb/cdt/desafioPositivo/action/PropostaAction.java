package br.unb.cdt.desafioPositivo.action;

import java.util.List;

import org.jboss.seam.ScopeType;
import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.annotations.Scope;
import org.jboss.seam.annotations.datamodel.DataModel;

import br.unb.cdt.desafioPositivo.facade.DesafioPositivoFacade;
import br.unb.cdt.desafioPositivo.model.Proposta;
import br.unb.cdt.desafioPositivo.model.Usuario;

@Name("propostaAction")
@Scope(ScopeType.CONVERSATION)
@AutoCreate
public class PropostaAction {

	private Proposta proposta;
	
	@In private Usuario usuarioLogado;
	
	@In private DesafioPositivoFacade facade; 
	
	@DataModel private List<Proposta> propostasSubmetidas;
	
	@DataModel private Proposta propostaSelecionada; 
	
	public PropostaAction() {
		proposta = new Proposta();
	}
	
	public void cadastro() {
		facade.adicionarProposta(usuarioLogado, proposta);
	}

	public Proposta getProposta() {
		return proposta;
	}

	public void setProposta(Proposta proposta) {
		this.proposta = proposta;
	}
	
	public void recuperaPropostasUsuario() {
		setPropostas(facade.recuperaPropostas(usuarioLogado));
	}

	public List<Proposta> getPropostas() {
		return propostasSubmetidas;
	}

	public void setPropostas(List<Proposta> propostas) {
		this.propostasSubmetidas = propostas;
	}

	public Proposta getPropostaSelecionada() {
		return propostaSelecionada;
	}

	public void setPropostaSelecionada(Proposta propostaSelecionada) {
		this.propostaSelecionada = propostaSelecionada;
	}
	
}
