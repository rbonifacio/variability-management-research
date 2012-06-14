package br.unb.cdt.desafioPositivo.action;

import java.util.List;

import javax.faces.application.FacesMessage;

import org.jboss.seam.ScopeType;
import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.Factory;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.annotations.Out;
import org.jboss.seam.annotations.Scope;
import org.jboss.seam.annotations.datamodel.DataModel;
import org.jboss.seam.annotations.datamodel.DataModelSelection;
import org.jboss.seam.faces.FacesMessages;

import br.unb.cdt.desafioPositivo.facade.DesafioPositivoFacade;
import br.unb.cdt.desafioPositivo.model.Proposta;
import br.unb.cdt.desafioPositivo.model.Usuario;

@Name("propostaAction")
@Scope(ScopeType.SESSION)
@AutoCreate
public class PropostaAction {

	private Proposta proposta;
	
	@In private Usuario usuarioLogado;
	
	@In private DesafioPositivoFacade facade; 
	
	@DataModel
	private List<Proposta> propostasSubmetidas;
	
	@DataModelSelection
	private Proposta propostaSelecionada; 
	
	@In 
	private FacesMessages facesMessages;
	
	public PropostaAction() {
		proposta = new Proposta();
	}
	
	public String cadastro() {
		try {
			facade.adicionarProposta(usuarioLogado, proposta);
			proposta = null;
			return "sumario";
		}
		catch(Exception e) {
			facesMessages.add(FacesMessage.SEVERITY_ERROR, e.getMessage());
			return null;
		}
	}

	public Proposta getProposta() {
		return proposta;
	}

	public void setProposta(Proposta proposta) {
		this.proposta = proposta;
	}
	
	@Factory("propostasSubmetidas") 
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
	
	public String novaProposta() {
		return "novaProposta";
	}
	
	public String editarProposta() {
		return "editarProposta";
	}
	
	public String editar() {
		try {
			facade.editarProposta(propostaSelecionada);
			return "sumario";
		} catch(Exception e) {
			facesMessages.add(FacesMessage.SEVERITY_ERROR, "Erro ao editar proposta.");
			return null;
		}
	}
	
	public void excluir() {
		try {
			/*
			 * Descomentar as linhas abaixo quando o método excluirProposta() estiver funcionando.
			 */
			facade.excluirProposta(propostaSelecionada);
			propostasSubmetidas.remove(propostaSelecionada);
			propostaSelecionada = null;
			facesMessages.add(FacesMessage.SEVERITY_INFO, "Proposta excluída com sucesso.");
		} catch(Exception e) {
			facesMessages.add(FacesMessage.SEVERITY_ERROR, "Erro ao excluir proposta.");
		}
	}
	
}
