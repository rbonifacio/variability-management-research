package br.unb.cdt.desafioPositivo.action;

import javax.faces.application.FacesMessage;

import org.jboss.seam.ScopeType;
import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.annotations.Scope;
import org.jboss.seam.faces.FacesMessages;

import br.unb.cdt.desafioPositivo.facade.DesafioPositivoFacade;
import br.unb.cdt.desafioPositivo.model.Usuario;

@Name("usuarioLogadoAction")
@Scope(ScopeType.CONVERSATION)
@AutoCreate
public class UsuarioLogadoAction {

	@In
	private Usuario usuarioLogado;

	@In
	private FacesMessages facesMessages;

	@In
	private DesafioPositivoFacade facade;

	private String senhaAntiga;

	private String novaSenha;

	private String confirmacaoNovaSenha;


	public String getSenhaAntiga() {
		return senhaAntiga;
	}

	public void setSenhaAntiga(String senhaAntiga) {
		this.senhaAntiga = senhaAntiga;
	}

	public String getNovaSenha() {
		return novaSenha;
	}

	public void setNovaSenha(String novaSenha) {
		this.novaSenha = novaSenha;
	}

	public String getConfirmacaoNovaSenha() {
		return confirmacaoNovaSenha;
	}

	public void setConfirmacaoNovaSenha(String confirmacaoNovaSenha) {
		this.confirmacaoNovaSenha = confirmacaoNovaSenha;
	}

	public String novaSenha() {
		return "alterarSenha";
	}

	public String alterarSenha() {
		if(novaSenha.equals(confirmacaoNovaSenha)) {
			try {
				// Esse método sempre cai no "catch", pois "usuarioLogado.getSenha() == null".
				// Tem que recuperar a senha do usuário do banco da Positivo.
				if(usuarioLogado.getSenha().equals(senhaAntiga)) {
					usuarioLogado.setSenha(novaSenha);
					usuarioLogado.setConfirmacaoSenha(confirmacaoNovaSenha);
					facade.alterarSenha(usuarioLogado);
					return "sumario";
					// Retirar esse else e tratar via exceções.
				} else {
					facesMessages.add(FacesMessage.SEVERITY_ERROR, "Senha antiga incorreta.");
					return null;
				}
			} catch(Exception e) {
				facesMessages.add(FacesMessage.SEVERITY_ERROR, "Ocorreu um erro ao alterar a senha.");
				return null;
			}
		} else {
			facesMessages.add(FacesMessage.SEVERITY_ERROR, "Verifique sua senha.");
			return null;
		}
	}


	public String atualizarDados() {
		return "atualizarDados";
	}

	public String atualizar() {
		try {
			facade.atualizarUsuario(usuarioLogado);
			//facesMessages.add(FacesMessage.SEVERITY_INFO, #{usuarioLogado.nome});
			return "sumario";
		} catch(Exception e) {
			facesMessages.add(FacesMessage.SEVERITY_ERROR, e.getMessage());
			return null;
		}
	}

}
