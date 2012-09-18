package br.unb.cdt.desafioPositivo.model.dto;

import org.jboss.seam.ScopeType;
import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.annotations.Scope;

import br.unb.cdt.desafioPositivo.util.criptografia.CriptografiaUtil;

@Name("alteraSenhaDTO")
@Scope(ScopeType.CONVERSATION)
@AutoCreate
public class AlteraSenhaDTO {

	private String senhaAtual;
	private String novaSenha;
	private String confirmacaoNovaSenha;
	
	public String getSenhaAtual() {
		return senhaAtual;
	}
	
	public void setSenhaAtual(String senhaAtual) {
		this.senhaAtual = senhaAtual;
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
	
	public boolean valida() {
		return novaSenha != null &&
				confirmacaoNovaSenha != null &&
				novaSenha.equals(confirmacaoNovaSenha) &&
				CriptografiaUtil.verificaSenha(novaSenha);
	}
}
