package br.unb.cdt.desafioPositivo.action;

import java.util.ArrayList;
import java.util.List;

import javax.faces.application.FacesMessage;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;

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
import org.jboss.seam.international.StatusMessage;
import org.jboss.seam.international.StatusMessages;

import com.sun.xml.internal.ws.api.message.Attachment;

import br.unb.cdt.desafioPositivo.facade.DesafioPositivoFacade;
import br.unb.cdt.desafioPositivo.mensagens.Mensagens;
import br.unb.cdt.desafioPositivo.model.Proposta;
import br.unb.cdt.desafioPositivo.model.Usuario;

@Name("propostaAction")
@Scope(ScopeType.CONVERSATION)
@AutoCreate
public class PropostaAction {

	private Proposta proposta;
	
	@In(value="#{facesContext.externalContext}")
	private ExternalContext extCtx;
	
	@In(value="#{facesContext}")
	FacesContext facesContext;
	
	@In private Usuario usuarioLogado;
	
	@In private DesafioPositivoFacade facade; 
	
	@DataModel
	private List<Proposta> propostasSubmetidas;
	
	@DataModelSelection
	private Proposta propostaSelecionada; 
	
	
	public PropostaAction() {
		proposta = new Proposta();
	}
	
	/**
	 * Recupera as propostas submetidas pelo usuario logado.
	 */
	@Factory("propostasSubmetidas") 
	public void recuperaPropostasUsuario() {
		setPropostas(facade.recuperaPropostas(usuarioLogado));
	}
	
	/**
	 * Realiza o cadastro de uma proposta de um usuario, 
	 * onde o usuario corresponde ao bean usuarioLogado e a 
	 * proposta corresponde ao bean proposta.
	 */
	public String cadastro() {
		List<String> erros = validaDadosCadastrais(proposta);
		
		if(! erros.isEmpty()) {
			populaMensagensErro(erros);
			//StatusMessages.instance().add(StatusMessage.Severity.ERROR);
			return null;
		}
		if(proposta.getArquivoGUI().length == 0) {
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.ERROR, Mensagens.NOVA_PROPOSTA_ERROR);
			return null;
		}
		
		try {
			facade.adicionarProposta(usuarioLogado, proposta);
			proposta = null;
			StatusMessages.instance().add(StatusMessage.Severity.INFO, "Proposta adicionada com sucesso!");
			return "minhaProposta";
		}
		catch(Exception e) {
			StatusMessages.instance().add(StatusMessage.Severity.ERROR, e.getMessage());
			return null;
		}
	}
	
	/*
	 * Valida os dados cadastrais da proposta.
	 */
	private List<String> validaDadosCadastrais(Proposta proposta) {
		List<String> erros = new ArrayList<String>();
		
		if(proposta.getNome() == null || proposta.getNome().equals("")) {
			erros.add("positivo.novaProposta.nomeEmBranco");
		}
		
		if(proposta.getDescricao() == null || proposta.getDescricao().equals("")) {
			erros.add("positivo.novaProposta.descricaoEmBranco");
		}
		
		if(proposta.getObjetivos() == null || proposta.getObjetivos().equals("")){
			erros.add("positivo.novaProposta.objetivosEmBranco");
		}
		
		if(proposta.getDescricaoFuncional() == null || proposta.getDescricaoFuncional().equals("")) {
			erros.add("positivo.novaProposta.descricaoFuncionalEmBranco");
		} 
		if(proposta.getPublicoAlvo() == null || proposta.getPublicoAlvo().equals("")) {
			erros.add("positivo.novaProposta.publicoAlvoEmBranco");
		}
		if( !proposta.getNomeArquivo().endsWith(".ep") && !proposta.getNomeArquivo().endsWith(".epz") ){
			erros.add("positivo.novaProposta.formatoErrado");
		}
		
		return erros;
	}

	public void populaMensagensErro(List<String> erros) {
		for(String e: erros) {
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.ERROR, e);
		}
	}
	
	/**
	 * Confirma as alteracoes dos dados da proposta.
	 */
	public String editar() {
		List<String> erros = validaDadosCadastrais(propostaSelecionada);
		
		if(! erros.isEmpty()) {
			populaMensagensErro(erros);
			return null;
		}
		
		try {
			facade.editarProposta(propostaSelecionada);
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.INFO, "Proposta alterada com sucesso!");
			return "sumario";
		} catch(Exception e) {
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.ERROR, Mensagens.EDITAR_PROPOSTA_ERRO);
			return null;
		}
	}
	
	/**
	 * Exclui a proposta selecionada do banco de dados. 
	 */
	public void excluir() {
		try {
			facade.excluirProposta(propostaSelecionada);
			propostasSubmetidas.remove(propostaSelecionada);
			propostaSelecionada = null;
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.INFO, Mensagens.EXCLUIR_PROPOSTA_SUCESSO);
		} catch(Exception e) {
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.ERROR, Mensagens.EXCLUIR_PROPOSTA_ERRO);
		}
	}

	public Proposta getProposta() {
		return proposta;
	}

	public void setProposta(Proposta proposta) {
		this.proposta = proposta;
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
	
	public String download() {
		String extensao = "", nome = "";

		if(propostaSelecionada.getNomeArquivo().endsWith(".ep"))
			extensao = "ep";
		else if(propostaSelecionada.getNomeArquivo().endsWith(".epz") )
			extensao = "epz";
		else
			return "GRAVE ERRO";
		
		nome = propostaSelecionada.getNomeArquivo().
				substring(0, propostaSelecionada.getNomeArquivo().length() - ( extensao.length() ) );
		
		HttpServletResponse response = (HttpServletResponse)extCtx.getResponse();
		response.setContentType(extensao);
		System.out.println("ESTAMOS AQUI" + extensao);
		response.addHeader(	"Content-disposition", "attachment; filename=\"" + nome +"\"");
		
		try {
			ServletOutputStream os = response.getOutputStream();
			os.write(propostaSelecionada.getArquivoGUI() );
			os.flush();
			os.close();
			facesContext.responseComplete();
		} catch(Exception e) {
			StatusMessages.instance().
			addFromResourceBundle(StatusMessage.Severity.INFO, "Não foi possível carregar a proposta antiga.");
		}

		return null;
	}
	
	
}
